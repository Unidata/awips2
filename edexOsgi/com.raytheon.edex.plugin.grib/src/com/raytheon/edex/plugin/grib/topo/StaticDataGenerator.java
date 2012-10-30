/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.grib.topo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.measure.unit.SI;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.edex.plugin.grib.util.GribModelLookup;
import com.raytheon.edex.plugin.grib.util.GribParamInfoLookup;
import com.raytheon.edex.plugin.grib.util.GridModel;
import com.raytheon.edex.plugin.grib.util.ParameterInfo;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.util.StaticGridData;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;
import com.raytheon.uf.edex.plugin.grid.topo.StaticTopoData;

/**
 * Populates the data store with static data which includes topo,spacing, and
 * coriolis parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class StaticDataGenerator implements Processor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StaticDataGenerator.class);

    private static final String GEOPOTENTIAL_HEIGHT_PARAM = "GH";

    private static final String GEOMETRIC_HEIGHT_PARAM = "GeH";

    private static final String SURFACE_LEVEL = "SFC";

    private static final String STATIC_TOPO = "staticTopo";

    private static final String STATIC_SPACING = "staticSpacing";

    private static final String STATIC_CORIOLIS = "staticCoriolis";

    /**
     * Cache of modelName, refTime, Forecast that keeps track of whether static
     * topo needs to be regen'd for that forecast time.
     */
    private ConcurrentMap<String, ConcurrentMap<Date, ConcurrentMap<Integer, String>>> staticTopoTimeCache = new ConcurrentHashMap<String, ConcurrentMap<Date, ConcurrentMap<Integer, String>>>(
            125);

    private static StaticDataGenerator instance = new StaticDataGenerator();

    public static StaticDataGenerator getInstance() {
        return instance;
    }

    private StaticDataGenerator() {
        checkModelTopo();
    }

    /**
     * Private method used by the initialization code to see if the static topo
     * data for a coverage has been initialized
     */
    private void checkModelTopo() {
        Map<String, GridModel> models = GribModelLookup.getInstance()
                .getModelByNameMap();

        List<String> modelsToProcess = new LinkedList<String>(models.keySet());
        int size = modelsToProcess.size();
        while (size > 0) {
            Iterator<String> iter = modelsToProcess.iterator();
            while (iter.hasNext()) {
                String name = iter.next();
                boolean processed = false;
                if (GribParamInfoLookup.getInstance().getParameterInfo(name,
                        "staticTopo") != null) {
                    processed = true;
                    for (GridCoverage coverage : GribSpatialCache.getInstance()
                            .getGridsForModel(name)) {
                        processed &= StaticTopoData.getInstance()
                                .checkModelTopo(coverage);
                    }
                } else {
                    processed = true;
                }

                // remove the item from the queue
                if (processed) {
                    iter.remove();
                }
            }

            int curSize = modelsToProcess.size();
            if (curSize > 0 && curSize == size) {
                // we still have items to process and we didn't process any then
                // they are in process by another server and we need to sleep
                // and try again
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    // ignore
                }
            }

            size = curSize;
        }
    }

    @Override
    public void process(Exchange exchange) throws Exception {
        Object payload = exchange.getIn().getBody();
        List<GridRecord> stopoRecords = null;

        if (payload instanceof PluginDataObject[]) {
            PluginDataObject[] records = (PluginDataObject[]) payload;
            if (records == null || records.length == 0) {
                // nothing to process, cancel the exchange
                exchange.getOut().setFault(true);
            }

            stopoRecords = new ArrayList<GridRecord>(records.length);
            for (PluginDataObject pdo : records) {
                try {
                    List<GridRecord> staticRecords = checkForStaticTopo((GridRecord) pdo);
                    if (staticRecords != null && !staticRecords.isEmpty()) {
                        stopoRecords.addAll(staticRecords);
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                            "Error creating/saving staticTopo data!", e);
                }
            }
        } else {
            statusHandler.handle(Priority.ERROR,
                    "StaticTopoGenerator received unhandled message type. Expected: "
                            + (new GridRecord[0].getClass().toString())
                            + " received: " + payload.getClass());
        }

        if (stopoRecords == null || stopoRecords.size() == 0) {
            // nothing to process, cancel the exchange
            exchange.getOut().setFault(true);
        } else {
            exchange.getOut().setBody(
                    stopoRecords.toArray(new GridRecord[stopoRecords.size()]));
        }
    }

    private List<GridRecord> checkForStaticTopo(GridRecord record)
            throws Exception {
        GridDao dao = new GridDao();
        ParameterInfo topoParamInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(record.getDatasetId(), STATIC_TOPO);
        ParameterInfo spacingParamInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(record.getDatasetId(), STATIC_SPACING);
        ParameterInfo coriolisParamInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(record.getDatasetId(), STATIC_CORIOLIS);
        if (topoParamInfo == null && spacingParamInfo == null
                && coriolisParamInfo == null) {
            return Collections.emptyList();
        }

        GridRecord staticTopoGridRecord = null;
        GridRecord staticXSpacingRecord = null;
        GridRecord staticYSpacingRecord = null;
        GridRecord staticCoriolisRecord = null;
        ConcurrentMap<Date, ConcurrentMap<Integer, String>> stModelCache = staticTopoTimeCache
                .get(record.getDatasetId());

        if (stModelCache == null) {
            // should only ever have 1 refTime in it
            stModelCache = new ConcurrentHashMap<Date, ConcurrentMap<Integer, String>>(
                    4);
            staticTopoTimeCache.put(record.getDatasetId(), stModelCache);
        }

        DataTime dataTime = record.getDataTime();
        ConcurrentMap<Integer, String> stRefTimeCache = stModelCache
                .get(dataTime.getRefTime());

        if (stRefTimeCache == null) {
            stRefTimeCache = new ConcurrentHashMap<Integer, String>(50);
            stModelCache.clear();
            stModelCache.put(dataTime.getRefTime(), stRefTimeCache);
        }

        ClusterTask ct = null;
        try {
            if (!stRefTimeCache.containsKey(dataTime.getFcstTime())) {
                // first for this forecastTime, grab lock
                ct = getStaticTopoClusterLock(record);
            }

            /*
             * If it is Geopotential Height at Surface and this model contains
             * staticTop, then store a staticTopoRecord
             */
            if ((record.getParameter().getAbbreviation()
                    .equals(GEOPOTENTIAL_HEIGHT_PARAM) || record.getParameter()
                    .getAbbreviation().equals(GEOMETRIC_HEIGHT_PARAM))
                    && record.getLevel().getMasterLevel().getName()
                            .equals(SURFACE_LEVEL)) {
                if (ct == null) {
                    ct = getStaticTopoClusterLock(record);
                }

                float[] rawData = (float[]) record.getMessageData();
                FloatDataRecord staticTopoData = new FloatDataRecord(
                        STATIC_TOPO, "/" + record.getLocation().getId(),
                        rawData, 2, new long[] { record.getLocation().getNx(),
                                record.getLocation().getNy() });
                staticTopoGridRecord = createTopoRecord(record);
                staticXSpacingRecord = createXSpacing(record);
                staticYSpacingRecord = createYSpacing(record);
                staticCoriolisRecord = createCoriolis(record);
                IDataStore dataStore = null;
                if (staticTopoGridRecord != null) {
                    dataStore = dao.getDataStore(staticTopoGridRecord);
                } else if (staticXSpacingRecord != null) {
                    dataStore = dao.getDataStore(staticXSpacingRecord);
                } else if (staticYSpacingRecord != null) {
                    dataStore = dao.getDataStore(staticYSpacingRecord);
                } else if (staticCoriolisRecord != null) {
                    dataStore = dao.getDataStore(staticCoriolisRecord);
                }
                dataStore.addDataRecord(staticTopoData);
                getStaticData(staticXSpacingRecord, staticYSpacingRecord,
                        staticCoriolisRecord, dataStore);
                StorageStatus status = dataStore.store(StoreOp.REPLACE);
                StorageException[] se = status.getExceptions();
                if (se == null || se.length == 0) {
                    persistStaticDataToDatabase(dao, staticTopoGridRecord,
                            staticXSpacingRecord, staticYSpacingRecord,
                            staticCoriolisRecord);
                    stRefTimeCache.put(dataTime.getFcstTime(), "");
                } else {
                    statusHandler.handle(Priority.ERROR,
                            "Error persisting staticTopo data to hdf5", se[0]);
                    staticTopoGridRecord = null;
                }
            } else if (!stRefTimeCache.containsKey(dataTime.getFcstTime())) {
                // double check cache in case lock had to wait for running to
                // finish
                staticTopoGridRecord = createTopoRecord(record);
                staticXSpacingRecord = createXSpacing(record);
                staticYSpacingRecord = createYSpacing(record);
                staticCoriolisRecord = createCoriolis(record);
                IDataStore dataStore = null;
                if (staticTopoGridRecord != null) {
                    dataStore = dao.getDataStore(staticTopoGridRecord);
                } else if (staticXSpacingRecord != null) {
                    dataStore = dao.getDataStore(staticXSpacingRecord);
                } else if (staticYSpacingRecord != null) {
                    dataStore = dao.getDataStore(staticYSpacingRecord);
                } else if (staticCoriolisRecord != null) {
                    dataStore = dao.getDataStore(staticCoriolisRecord);
                }
                String[] dataSets = null;

                try {
                    // check if its already been stored
                    dataSets = dataStore.getDatasets("/"
                            + record.getLocation().getId());
                } catch (Exception e) {
                    // Ignore
                }
                if (dataSets == null
                        || (dataSets != null && !Arrays.asList(dataSets)
                                .contains(STATIC_TOPO))) {
                    if (staticTopoGridRecord != null) {
                        FloatDataRecord staticTopoRecord = StaticTopoData
                                .getInstance().getStopoData(
                                        record.getLocation());
                        staticTopoRecord.setGroup("/"
                                + record.getLocation().getId());
                        staticTopoRecord.setName(STATIC_TOPO);
                        dataStore.addDataRecord(staticTopoRecord);
                    }
                    getStaticData(staticXSpacingRecord, staticYSpacingRecord,
                            staticCoriolisRecord, dataStore);
                    StorageStatus status = dataStore.store(StoreOp.REPLACE);
                    StorageException[] se = status.getExceptions();
                    if (se == null || se.length == 0) {
                        // store to database
                        persistStaticDataToDatabase(dao, staticTopoGridRecord,
                                staticXSpacingRecord, staticYSpacingRecord,
                                staticCoriolisRecord);
                        stRefTimeCache.put(dataTime.getFcstTime(), "");
                    } else {
                        // failed to store
                        statusHandler.handle(Priority.ERROR,
                                "Error persisting staticTopo data to hdf5",
                                se[0]);
                        staticTopoGridRecord = null;
                    }
                } else {
                    // dataset existed verify in database
                    persistStaticDataToDatabase(dao, staticTopoGridRecord,
                            staticXSpacingRecord, staticYSpacingRecord,
                            staticCoriolisRecord);
                    stRefTimeCache.put(dataTime.getFcstTime(), "");
                }
            }
        } finally {
            if (ct != null) {
                ClusterLockUtils.deleteLock(ct.getId().getName(), ct.getId()
                        .getDetails());
            }
        }
        List<GridRecord> staticRecords = new ArrayList<GridRecord>();
        if (staticTopoGridRecord != null) {
            staticRecords.add(staticTopoGridRecord);
        }
        if (staticXSpacingRecord != null) {
            staticRecords.add(staticXSpacingRecord);
        }
        if (staticYSpacingRecord != null) {
            staticRecords.add(staticYSpacingRecord);
        }
        if (staticCoriolisRecord != null) {
            staticRecords.add(staticCoriolisRecord);
        }
        return staticRecords;
    }

    private ClusterTask getStaticTopoClusterLock(GridRecord record) {
        String taskDetails = record.getDatasetId()
                + record.getDataTime().getRefTime();
        ClusterTask rval = null;
        do {
            rval = ClusterLockUtils.lock(STATIC_TOPO, taskDetails, 1500, true);
        } while (!rval.getLockState().equals(LockState.SUCCESSFUL));
        return rval;
    }

    private GridRecord createTopoRecord(GridRecord record) throws Exception {
        GridRecord staticTopoRecord = null;

        ParameterInfo paramInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(record.getDatasetId(), STATIC_TOPO);
        if (paramInfo == null) {
            return null;
        }

        staticTopoRecord = new GridRecord(record);
        staticTopoRecord.setId(0);
        staticTopoRecord.setDataURI(null);
        staticTopoRecord.setDataTime(null);
        staticTopoRecord.setInsertTime(TimeTools.getSystemCalendar());

        Calendar refTime = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
        refTime.setTime(record.getDataTime().getRefTime());
        DataTime dataTime = new DataTime(refTime, record.getDataTime()
                .getFcstTime());

        Parameter param = new Parameter(paramInfo.getShort_name(),
                paramInfo.getLong_name(), SI.METER);
        staticTopoRecord.setParameter(param);
        staticTopoRecord.setLevel(LevelFactory.getInstance().getLevel("Dflt",
                0, "m"));

        staticTopoRecord.setDataTime(dataTime);
        staticTopoRecord.getInfo().setId(null);
        staticTopoRecord.setMessageData(null);
        staticTopoRecord.setOverwriteAllowed(true);
        staticTopoRecord.constructDataURI();
        return staticTopoRecord;
    }

    private void getStaticData(GridRecord staticXRecord,
            GridRecord staticYRecord, GridRecord staticCoriolisRecord,
            IDataStore dataStore) throws Exception {

        if (staticXRecord != null) {
            FloatDataRecord dxRecord = StaticGridData.getInstance(
                    staticXRecord.getLocation()).getDx();
            if (staticYRecord == null) {
                dxRecord.setName("staticSpacing");
            } else {
                dxRecord.setName("staticXspacing");
            }
            dxRecord.setGroup("/" + staticXRecord.getLocation().getId());
            dataStore.addDataRecord(dxRecord);
        }

        if (staticYRecord != null) {
            FloatDataRecord dyRecord = StaticGridData.getInstance(
                    staticYRecord.getLocation()).getDy();
            dyRecord.setName("staticYspacing");
            dyRecord.setGroup("/" + staticXRecord.getLocation().getId());
            dataStore.addDataRecord(dyRecord);
        }

        if (staticCoriolisRecord != null) {
            FloatDataRecord coriolisRecord = StaticGridData.getInstance(
                    staticCoriolisRecord.getLocation()).getCoriolis();
            coriolisRecord.setName("staticCoriolis");
            coriolisRecord.setGroup("/" + staticXRecord.getLocation().getId());
            dataStore.addDataRecord(coriolisRecord);
        }
    }

    private GridRecord createXSpacing(GridRecord record) throws Exception {
        ParameterInfo paramInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(record.getDatasetId(), STATIC_SPACING);
        if (paramInfo == null) {
            paramInfo = GribParamInfoLookup.getInstance().getParameterInfo(
                    record.getDatasetId(), "staticXspacing");
            if (paramInfo == null) {
                return null;
            } else {
                return createStaticRecord(record, "staticXspacing", paramInfo);
            }
        } else {
            return createStaticRecord(record, "staticSpacing", paramInfo);
        }

    }

    private GridRecord createYSpacing(GridRecord record) throws Exception {
        ParameterInfo paramInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(record.getDatasetId(), "staticYspacing");
        if (paramInfo == null) {
            return null;
        } else {
            return createStaticRecord(record, "staticYspacing", paramInfo);
        }
    }

    private GridRecord createCoriolis(GridRecord record) throws Exception {
        ParameterInfo paramInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(record.getDatasetId(), "staticCoriolis");
        return createStaticRecord(record, "staticCoriolis", paramInfo);
    }

    private GridRecord createStaticRecord(GridRecord record, String name,
            ParameterInfo paramInfo) throws Exception {
        GridRecord staticRecord = null;

        staticRecord = new GridRecord(record);
        staticRecord.setId(0);
        staticRecord.setDataURI(null);
        staticRecord.setDataTime(null);
        staticRecord.setInsertTime(TimeTools.getSystemCalendar());

        Calendar refTime = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
        refTime.setTime(record.getDataTime().getRefTime());
        DataTime dataTime = new DataTime(refTime, record.getDataTime()
                .getFcstTime());

        Parameter param = new Parameter(name, paramInfo.getLong_name(),
                paramInfo.getUnits());
        staticRecord.setParameter(param);
        staticRecord.setLevel(LevelFactory.getInstance().getLevel("Dflt", 0,
                "m"));
        staticRecord.getInfo().setId(null);
        staticRecord.setDataTime(dataTime);

        staticRecord.setMessageData(null);
        staticRecord.setOverwriteAllowed(true);
        staticRecord.constructDataURI();
        return staticRecord;

    }

    /**
     * Used to persist the static topo record to the database.
     * 
     * @param topo
     */
    private void persistStaticDataToDatabase(GridDao dao, GridRecord topo,
            GridRecord staticXSpacing, GridRecord staticYSpacing,
            GridRecord staticCoriolis) throws SerializationException,
            EdexException {
        if (topo != null && !isSTopoInDb(dao, topo)) {
            statusHandler.handle(Priority.DEBUG,
                    "Persisting static topo record to the database!!!");
            dao.persistToDatabase(topo);
        }
        if (staticXSpacing != null && !isSTopoInDb(dao, staticXSpacing)) {
            statusHandler.handle(Priority.DEBUG, "Persisting static "
                    + staticXSpacing.getParameter().getAbbreviation()
                    + " record to the database!!!");
            dao.persistToDatabase(staticXSpacing);
        }
        if (staticYSpacing != null && !isSTopoInDb(dao, staticYSpacing)) {
            statusHandler.handle(Priority.DEBUG, "Persisting static "
                    + staticYSpacing.getParameter().getAbbreviation()
                    + " record to the database!!!");
            dao.persistToDatabase(staticYSpacing);
        }
        if (staticCoriolis != null && !isSTopoInDb(dao, staticCoriolis)) {
            statusHandler.handle(Priority.DEBUG, "Persisting static "
                    + staticCoriolis.getParameter().getAbbreviation()
                    + " record to the database!!!");
            dao.persistToDatabase(staticCoriolis);
        }
    }

    private boolean isSTopoInDb(GridDao dao, GridRecord record)
            throws DataAccessLayerException {
        List<String> fields = Arrays.asList("dataURI");
        List<Object> values = Arrays.asList((Object) record.getDataURI());
        List<?> list = dao.queryByCriteria(fields, values);
        return !list.isEmpty();
    }
}
