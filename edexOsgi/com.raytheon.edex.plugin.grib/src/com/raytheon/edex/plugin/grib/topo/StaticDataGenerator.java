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
import java.util.List;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

import com.raytheon.edex.plugin.grib.dao.GribDao;
import com.raytheon.edex.plugin.grib.util.GribModelCache;
import com.raytheon.edex.plugin.grib.util.GribParamInfoLookup;
import com.raytheon.edex.plugin.grib.util.ParameterInfo;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.util.StaticGridData;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

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
        // Force initialization of static topo data
        StaticTopoData.getInstance();
    }

    @Override
    public void process(Exchange exchange) throws Exception {
        Object payload = exchange.getIn().getBody();
        List<GribRecord> stopoRecords = null;

        if (payload instanceof PluginDataObject[]) {
            PluginDataObject[] records = (PluginDataObject[]) payload;
            if (records == null || records.length == 0) {
                // nothing to process, cancel the exchange
                exchange.getOut().setFault(true);
            }

            stopoRecords = new ArrayList<GribRecord>(records.length);
            for (PluginDataObject pdo : records) {
                try {
                    List<GribRecord> staticRecords = checkForStaticTopo((GribRecord) pdo);
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
                            + (new GribRecord[0].getClass().toString())
                            + " received: " + payload.getClass());
        }

        if (stopoRecords == null || stopoRecords.size() == 0) {
            // nothing to process, cancel the exchange
            exchange.getOut().setFault(true);
        } else {
            exchange.getOut().setBody(
                    stopoRecords.toArray(new GribRecord[stopoRecords.size()]));
        }
    }

    private List<GribRecord> checkForStaticTopo(GribRecord record)
            throws Exception {
        GribDao dao = new GribDao();
        GribModel model = record.getModelInfo();
        ParameterInfo topoParamInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(model.getModelName(), STATIC_TOPO);
        ParameterInfo spacingParamInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(model.getModelName(), STATIC_SPACING);
        ParameterInfo coriolisParamInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(model.getModelName(), STATIC_CORIOLIS);
        if (topoParamInfo == null && spacingParamInfo == null
                && coriolisParamInfo == null) {
            return Collections.emptyList();
        }

        GribRecord staticTopoGribRecord = null;
        GribRecord staticXSpacingRecord = null;
        GribRecord staticYSpacingRecord = null;
        GribRecord staticCoriolisRecord = null;
        ConcurrentMap<Date, ConcurrentMap<Integer, String>> stModelCache = staticTopoTimeCache
                .get(model.getModelName());

        if (stModelCache == null) {
            // should only ever have 1 refTime in it
            stModelCache = new ConcurrentHashMap<Date, ConcurrentMap<Integer, String>>(
                    4);
            staticTopoTimeCache.put(model.getModelName(), stModelCache);
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
            if ((model.getParameterAbbreviation().equals(
                    GEOPOTENTIAL_HEIGHT_PARAM) || model
                    .getParameterAbbreviation().equals(GEOMETRIC_HEIGHT_PARAM))
                    && model.getLevelName().equals(SURFACE_LEVEL)) {
                if (ct == null) {
                    ct = getStaticTopoClusterLock(record);
                }

                float[] rawData = (float[]) record.getMessageData();
                FloatDataRecord staticTopoData = new FloatDataRecord(
                        STATIC_TOPO, "/", rawData, 2, new long[] {
                                record.getModelInfo().getLocation().getNx(),
                                record.getModelInfo().getLocation().getNy() });
                staticTopoGribRecord = createTopoRecord(record);
                staticXSpacingRecord = createXSpacing(record);
                staticYSpacingRecord = createYSpacing(record);
                staticCoriolisRecord = createCoriolis(record);
                IDataStore dataStore = null;
                if (staticTopoGribRecord != null) {
                    dataStore = dao.getDataStore(staticTopoGribRecord);
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
                    persistStaticDataToDatabase(dao, staticTopoGribRecord,
                            staticXSpacingRecord, staticYSpacingRecord,
                            staticCoriolisRecord);
                    stRefTimeCache.put(dataTime.getFcstTime(), "");
                } else {
                    statusHandler.handle(Priority.ERROR,
                            "Error persisting staticTopo data to hdf5", se[0]);
                    staticTopoGribRecord = null;
                }
            } else if (!stRefTimeCache.containsKey(dataTime.getFcstTime())) {
                // double check cache in case lock had to wait for running to
                // finish
                staticTopoGribRecord = createTopoRecord(record);
                staticXSpacingRecord = createXSpacing(record);
                staticYSpacingRecord = createYSpacing(record);
                staticCoriolisRecord = createCoriolis(record);
                IDataStore dataStore = null;
                if (staticTopoGribRecord != null) {
                    dataStore = dao.getDataStore(staticTopoGribRecord);
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
                    dataSets = dataStore.getDatasets("/");
                } catch (Exception e) {
                    // Ignore
                }
                if (dataSets == null
                        || (dataSets != null && !Arrays.asList(dataSets)
                                .contains(STATIC_TOPO))) {
                    if (staticTopoGribRecord != null) {
                        FloatDataRecord staticTopoRecord = StaticTopoData
                                .getInstance().getStopoData(
                                        record.getModelInfo().getModelName());
                        staticTopoRecord.setGroup("/");
                        staticTopoRecord.setName(STATIC_TOPO);
                        dataStore.addDataRecord(staticTopoRecord);
                    }
                    getStaticData(staticXSpacingRecord, staticYSpacingRecord,
                            staticCoriolisRecord, dataStore);
                    StorageStatus status = dataStore.store(StoreOp.REPLACE);
                    StorageException[] se = status.getExceptions();
                    if (se == null || se.length == 0) {
                        // store to database
                        persistStaticDataToDatabase(dao, staticTopoGribRecord,
                                staticXSpacingRecord, staticYSpacingRecord,
                                staticCoriolisRecord);
                        stRefTimeCache.put(dataTime.getFcstTime(), "");
                    } else {
                        // failed to store
                        statusHandler.handle(Priority.ERROR,
                                "Error persisting staticTopo data to hdf5",
                                se[0]);
                        staticTopoGribRecord = null;
                    }
                } else {
                    // dataset existed verify in database
                    persistStaticDataToDatabase(dao, staticTopoGribRecord,
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
        List<GribRecord> staticRecords = new ArrayList<GribRecord>();
        if (staticTopoGribRecord != null) {
            staticRecords.add(staticTopoGribRecord);
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

    private ClusterTask getStaticTopoClusterLock(GribRecord record) {
        String taskDetails = record.getModelInfo().getModelName()
                + record.getDataTime().getRefTime();
        ClusterTask rval = null;
        do {
            rval = ClusterLockUtils.lock(STATIC_TOPO, taskDetails, 1500, true);
        } while (!rval.getLockState().equals(LockState.SUCCESSFUL));
        return rval;
    }

    private GribRecord createTopoRecord(GribRecord record) throws Exception {
        GribRecord staticTopoRecord = null;
        GribModel model = record.getModelInfo();

        ParameterInfo paramInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(model.getModelName(), STATIC_TOPO);
        if (paramInfo == null) {
            return null;
        }

        staticTopoRecord = new GribRecord(record);
        staticTopoRecord.setId(0);
        staticTopoRecord.setDataURI(null);
        staticTopoRecord.setDataTime(null);
        staticTopoRecord.setInsertTime(TimeTools.getSystemCalendar());

        Calendar refTime = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
        refTime.setTime(record.getDataTime().getRefTime());
        DataTime dataTime = new DataTime(refTime, record.getDataTime()
                .getFcstTime());

        GribModel staticTopoModelInfo = new GribModel(model);
        staticTopoModelInfo.setId(null);
        staticTopoModelInfo.setParameterAbbreviation(paramInfo.getShort_name());
        staticTopoModelInfo.setParameterName(paramInfo.getLong_name());
        staticTopoModelInfo.setParameterUnit("m");
        staticTopoModelInfo.setLevel(LevelFactory.getInstance().getLevel(
                "Dflt", 0, "m"));

        staticTopoRecord.setDataTime(dataTime);
        staticTopoModelInfo = (GribModelCache.getInstance()
                .getModel(staticTopoModelInfo));
        staticTopoRecord.setModelInfo(staticTopoModelInfo);
        staticTopoRecord.setMessageData(null);
        staticTopoRecord.setOverwriteAllowed(true);
        staticTopoRecord.constructDataURI();
        return staticTopoRecord;
    }

    private void getStaticData(GribRecord staticXRecord,
            GribRecord staticYRecord, GribRecord staticCoriolisRecord,
            IDataStore dataStore) throws Exception {

        if (staticXRecord != null) {
            FloatDataRecord dxRecord = StaticGridData.getInstance(
                    staticXRecord.getModelInfo().getLocation()).getDx();
            if (staticYRecord == null) {
                dxRecord.setName("staticSpacing");
            } else {
                dxRecord.setName("staticXspacing");
            }
            dxRecord.setGroup("/");
            dataStore.addDataRecord(dxRecord);
        }

        if (staticYRecord != null) {
            FloatDataRecord dyRecord = StaticGridData.getInstance(
                    staticYRecord.getModelInfo().getLocation()).getDy();
            dyRecord.setName("staticYspacing");
            dyRecord.setGroup("/");
            dataStore.addDataRecord(dyRecord);
        }

        if (staticCoriolisRecord != null) {
            FloatDataRecord coriolisRecord = StaticGridData.getInstance(
                    staticCoriolisRecord.getModelInfo().getLocation())
                    .getCoriolis();
            coriolisRecord.setName("staticCoriolis");
            coriolisRecord.setGroup("/");
            dataStore.addDataRecord(coriolisRecord);
        }
    }

    private GribRecord createXSpacing(GribRecord record) throws Exception {
        GribModel model = record.getModelInfo();
        ParameterInfo paramInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(model.getModelName(), STATIC_SPACING);
        if (paramInfo == null) {
            paramInfo = GribParamInfoLookup.getInstance().getParameterInfo(
                    model.getModelName(), "staticXspacing");
            if (paramInfo == null) {
                return null;
            } else {
                return createStaticRecord(record, "staticXspacing", paramInfo);
            }
        } else {
            return createStaticRecord(record, "staticSpacing", paramInfo);
        }

    }

    private GribRecord createYSpacing(GribRecord record) throws Exception {
        GribModel model = record.getModelInfo();
        ParameterInfo paramInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(model.getModelName(), "staticYspacing");
        if (paramInfo == null) {
            return null;
        } else {
            return createStaticRecord(record, "staticYspacing", paramInfo);
        }
    }

    private GribRecord createCoriolis(GribRecord record) throws Exception {
        GribModel model = record.getModelInfo();
        ParameterInfo paramInfo = GribParamInfoLookup.getInstance()
                .getParameterInfo(model.getModelName(), "staticCoriolis");
        return createStaticRecord(record, "staticCoriolis", paramInfo);
    }

    private GribRecord createStaticRecord(GribRecord record, String name,
            ParameterInfo paramInfo) throws Exception {
        GribRecord staticRecord = null;
        GribModel model = record.getModelInfo();

        staticRecord = new GribRecord(record);
        staticRecord.setId(0);
        staticRecord.setDataURI(null);
        staticRecord.setDataTime(null);
        staticRecord.setInsertTime(TimeTools.getSystemCalendar());

        Calendar refTime = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
        refTime.setTime(record.getDataTime().getRefTime());
        DataTime dataTime = new DataTime(refTime, record.getDataTime()
                .getFcstTime());

        GribModel staticModelInfo = new GribModel(model);
        staticModelInfo.setId(null);
        staticModelInfo.setParameterAbbreviation(name);
        staticModelInfo.setParameterName(paramInfo.getLong_name());
        staticModelInfo.setParameterUnit(paramInfo.getUnits());
        staticModelInfo.setLevel(LevelFactory.getInstance().getLevel("Dflt", 0,
                "m"));

        staticRecord.setDataTime(dataTime);
        staticModelInfo = (GribModelCache.getInstance()
                .getModel(staticModelInfo));
        staticRecord.setModelInfo(staticModelInfo);
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
    private void persistStaticDataToDatabase(GribDao dao, GribRecord topo,
            GribRecord staticXSpacing, GribRecord staticYSpacing,
            GribRecord staticCoriolis) throws SerializationException,
            EdexException {
        if (topo != null && !dao.isSTopoInDb(topo)) {
            statusHandler.handle(Priority.DEBUG,
                    "Persisting static topo record to the database!!!");
            dao.persistToDatabase(topo);
        }
        if (staticXSpacing != null && !dao.isSTopoInDb(staticXSpacing)) {
            statusHandler.handle(Priority.DEBUG, "Persisting static "
                    + staticXSpacing.getModelInfo().getParameterAbbreviation()
                    + " record to the database!!!");
            dao.persistToDatabase(staticXSpacing);
        }
        if (staticYSpacing != null && !dao.isSTopoInDb(staticYSpacing)) {
            statusHandler.handle(Priority.DEBUG, "Persisting static "
                    + staticYSpacing.getModelInfo().getParameterAbbreviation()
                    + " record to the database!!!");
            dao.persistToDatabase(staticYSpacing);
        }
        if (staticCoriolis != null && !dao.isSTopoInDb(staticCoriolis)) {
            statusHandler.handle(Priority.DEBUG, "Persisting static "
                    + staticCoriolis.getModelInfo().getParameterAbbreviation()
                    + " record to the database!!!");
            dao.persistToDatabase(staticCoriolis);
        }
    }
}
