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
package com.raytheon.uf.edex.grid.staticdata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;

import com.raytheon.edex.plugin.gfe.paraminfo.GridParamInfoLookup;
import com.raytheon.edex.plugin.gfe.paraminfo.ParameterInfo;
import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.util.StaticGridData;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.audit.DataStatus;
import com.raytheon.uf.common.datastorage.audit.DataStorageAuditEvent;
import com.raytheon.uf.common.datastorage.audit.DataStorageAuditUtils;
import com.raytheon.uf.common.datastorage.audit.DataStorageAuditerContainer;
import com.raytheon.uf.common.datastorage.audit.Hdf5DataIdentifier;
import com.raytheon.uf.common.datastorage.audit.IDataIdentifier;
import com.raytheon.uf.common.datastorage.audit.IDataStorageAuditer;
import com.raytheon.uf.common.datastorage.audit.MetadataAndDataId;
import com.raytheon.uf.common.datastorage.audit.MetadataStatus;
import com.raytheon.uf.common.datastorage.records.DataUriMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.IMetadataIdentifier.MetadataSpecificity;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.plugin.DataURIDatabaseUtil;
import com.raytheon.uf.edex.grid.staticdata.topo.StaticTopoData;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Populates the data store with static data which includes topo,spacing, and
 * coriolis parameters
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 03, 2010           rjpeter     Initial creation
 * Feb 15, 2013  1638     mschenke    Moved DataURINotificationMessage to uf.common.dataplugin
 * Mar 07, 2013  1587     bsteffen    rewrite static data generation.
 * Mar 14, 2013  1587     bsteffen    Fix persisting to datastore.
 * Apr 14, 2014  16752    MPorricelli Add ensembleid to hash key to allow
 *                                    creation of static data for all perturbations
 *                                    of Ensemble models
 * Apr 21, 2014  2060     njensen     Remove dependency on grid dataURI column
 * Sep 19, 2014  3627     mapeters    Updated deprecated TimeTools usage.
 * Aug 06, 2013  3805     bsteffen    Increase time of cluster locks.
 * Apr 29, 2015  4167     nabowle     Propagate exceptions to #processNotification()
 * Oct 06, 2021  8608     mapeters    Add special auditing for static records that are only
 *                                    generated in the database or the datastore, not both
 * Feb 16, 2022  8608     mapeters    Handle minor data storage audit refactors
 *
 * </pre>
 *
 * @author rjpeter
 */
public class StaticDataGenerator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StaticDataGenerator.class);

    private static final String GEOPOTENTIAL_HEIGHT_PARAM = "GH";

    private static final String GEOMETRIC_HEIGHT_PARAM = "GeH";

    private static final String SURFACE_LEVEL = "SFC";

    private static final String STATIC_TOPO = "staticTopo";

    private static final String STATIC_SPACING = "staticSpacing";

    private static final String STATIC_X_SPACING = "staticXspacing";

    private static final String STATIC_Y_SPACING = "staticYspacing";

    private static final String STATIC_CORIOLIS = "staticCoriolis";

    private static final String[] STATIC_PARAMETERS = { STATIC_TOPO,
            STATIC_SPACING, STATIC_X_SPACING, STATIC_Y_SPACING,
            STATIC_CORIOLIS };

    private static int CACHE_SIZE = 1000;

    /**
     * Cache of modelName, refTime, Forecast, location that keeps track of
     * whether static data has been created to avoid too many trips to db to
     * check.
     */
    private final Map<CacheKey, CacheKey> dataGeneratedCache = Collections
            .synchronizedMap(new LinkedHashMap<CacheKey, CacheKey>(
                    (int) (CACHE_SIZE / 0.75f) + 1, 0.75f, true) {

                private static final long serialVersionUID = 5068414717181919964L;

                @Override
                protected boolean removeEldestEntry(
                        Entry<CacheKey, CacheKey> eldest) {
                    return size() > CACHE_SIZE;
                }

            });

    private static StaticDataGenerator instance = new StaticDataGenerator();

    public static StaticDataGenerator getInstance() {
        return instance;
    }

    private StaticDataGenerator() {

    }

    /**
     * This method is the beginning of static data generation, it takes
     * notification messages and creates the appropriate static records.
     */
    public GridRecord[] processNotification(DataURINotificationMessage msg)
            throws Exception {
        String[] uris = msg.getDataURIs();
        Set<GridRecord> staticRecords = new HashSet<>(uris.length, 1);
        for (String dataURI : uris) {
            if (dataURI.startsWith("/grid/")) {
                try {
                    GridRecord record = new GridRecord(dataURI);
                    staticRecords.addAll(processRecord(record));
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                            "Error creating/saving staticTopo data.", e);
                }
            }
        }
        return staticRecords.toArray(new GridRecord[0]);
    }

    /**
     * Takes a record, which should have been parsed from a notification and
     * processes the record to create any needed static records for the same
     * model and time.
     */
    private Collection<GridRecord> processRecord(GridRecord record)
            throws Exception {
        String datasetid = record.getDatasetId();
        GridParamInfoLookup gpif = GridParamInfoLookup.getInstance();

        // this list has an entry for each parameter that needs to be generated
        List<ParameterInfo> infoObjects = new ArrayList<>(
                STATIC_PARAMETERS.length);
        // Populate the lsit based off the paramInfo.
        for (String parameter : STATIC_PARAMETERS) {
            ParameterInfo paramInfo = gpif.getParameterInfo(datasetid,
                    parameter);
            if (paramInfo != null) {
                infoObjects.add(paramInfo);
            }
        }

        if (infoObjects.isEmpty()) {
            // nothing to generate
            return Collections.emptySet();
        }

        CacheKey key = new CacheKey(record);
        ClusterTask ct = null;
        try {
            if (!dataGeneratedCache.containsKey(key)) {
                // first for this forecastTime, grab lock
                ct = getStaticTopoClusterLock(record);
            }

            boolean topoCompatible = isTopoCompatibleRecord(record);

            // double check cache in case lock had to wait for running to finish
            if (dataGeneratedCache.containsKey(key)) {
                // if this key is been done clearing this stops processing.
                infoObjects.clear();
                if (topoCompatible) {
                    // put topo back in to allow it to be processed.
                    ParameterInfo paramInfo = gpif.getParameterInfo(datasetid,
                            STATIC_TOPO);
                    if (paramInfo != null) {
                        infoObjects.add(paramInfo);
                    }
                }
            }

            if (!infoObjects.isEmpty()) {
                if (ct == null) {
                    // if this is topoCompatible we can get here without locking
                    // so grab it.
                    ct = getStaticTopoClusterLock(record);
                }

                dataGeneratedCache.put(key, key);
                return createStaticRecords(infoObjects, record, topoCompatible);

            }
        } finally {
            if (ct != null) {
                ClusterLockUtils.deleteLock(ct.getId().getName(),
                        ct.getId().getDetails());
            }
        }
        return Collections.emptySet();
    }

    /**
     * Create and persist any static records that have not already been created.
     * This method assumes cluster locking and caching is already done, it will
     * check the database and datastore for duplicates.
     */
    private Collection<GridRecord> createStaticRecords(
            List<ParameterInfo> infoObjects, GridRecord record,
            boolean topoCompatible) throws Exception {
        // the static records are created and sorted, some may need to be stored
        // to the database some may need to be stored to hdf5, some may need
        // both and some may need neither.
        //
        // 1) Brand new model runs will need to store to both
        // 2) Since all forecast times share a datastore entry, after the first
        // forecast hour only the database is persisted.
        // 3) When a topo compatible record arrives it overrides any previous
        // data and only the datastore is written.
        // 4) If this has already been handled completely by another jvm then
        // neither is written

        GridDao dao = new GridDao();

        /*
         * The below are all maps of trace IDs -> records. They can't just be
         * sets of records because the records are modified in the below code in
         * ways that change their hash codes.
         */
        // contains all the static records.
        Map<String, GridRecord> staticRecords = new HashMap<>();
        // contains any static records that need to be saved to the db
        Map<String, GridRecord> databaseRecords = new HashMap<>();
        // contains any static records that need to be stored to hdf5.
        Map<String, GridRecord> datastoreRecords = new HashMap<>();

        for (ParameterInfo paramInfo : infoObjects) {
            GridRecord staticRecord = createStaticRecord(record, paramInfo);
            if (topoCompatible
                    && STATIC_TOPO.equals(paramInfo.getShort_name())) {
                // always fill topo records with topo compatible data
                copyRawData(dao, record, staticRecord);
                // force static topo into the datastore.
                datastoreRecords.put(staticRecord.getTraceId(), staticRecord);
            }
            staticRecords.put(staticRecord.getTraceId(), staticRecord);
        }

        databaseRecords.putAll(checkDatabase(staticRecords));
        datastoreRecords.putAll(checkDatastore(dao, databaseRecords));

        if (!datastoreRecords.isEmpty()) {
            for (GridRecord staticRecord : datastoreRecords.values()) {
                populateMessageData(staticRecord);
            }
            dao.persistToHDF5(
                    datastoreRecords.values().toArray(new PluginDataObject[0]));
        }
        if (!databaseRecords.isEmpty()) {
            dao.persistToDatabase(
                    databaseRecords.values().toArray(new PluginDataObject[0]));
        }

        auditPartialDataStorageOps(databaseRecords, datastoreRecords,
                dao.pathProvider);

        // remove records that have not changed from staticRecords so alerts are
        // only generated for new or changed records.
        staticRecords.keySet().retainAll(databaseRecords.keySet());
        staticRecords.putAll(datastoreRecords);
        return staticRecords.values();
    }

    /**
     * Audit records that are only stored to the database or only to the
     * datastore. Send other data storage operation pieces to the auditer so
     * that it doesn't complain about uncompleted data storage operations.
     *
     * @param databaseRecords
     *            all records stored to the database
     * @param datastoreRecords
     *            all records stored to the datastore
     */
    private void auditPartialDataStorageOps(
            Map<String, GridRecord> databaseRecords,
            Map<String, GridRecord> datastoreRecords,
            IHDFFilePathProvider pathProvider) {

        Map<String, GridRecord> databaseOnlyRecords = new HashMap<>(
                databaseRecords);
        Map<String, GridRecord> datastoreOnlyRecords = new HashMap<>(
                datastoreRecords);

        datastoreOnlyRecords.keySet().removeAll(databaseRecords.keySet());
        databaseOnlyRecords.keySet().removeAll(datastoreRecords.keySet());

        IDataStorageAuditer auditer = DataStorageAuditerContainer.getInstance()
                .getAuditer();

        if (!databaseOnlyRecords.isEmpty()) {
            /*
             * Database-only records - the datastore route is what sends the
             * metadata and data IDs so we have to send those as well as data
             * statuses
             */
            List<MetadataAndDataId> databaseOnlyDataIds = new ArrayList<>();
            for (GridRecord gr : databaseOnlyRecords.values()) {
                // Based off GridDao.populateDataStore
                String group = "/" + gr.getLocation().getId();
                String datasetName = gr.getParameter().getAbbreviation();
                Set<String> datasets = new HashSet<>();
                datasets.add(datasetName);

                String dataPath = HDF5Util.findHDF5Location(gr).getPath();

                IDataIdentifier dataId = new Hdf5DataIdentifier(gr.getTraceId(),
                        dataPath, group, datasets);
                IMetadataIdentifier metaId = new DataUriMetadataIdentifier(gr,
                        MetadataSpecificity.DATASET);
                databaseOnlyDataIds.add(new MetadataAndDataId(metaId, dataId));
            }
            Map<String, DataStatus> dataStatuses = databaseOnlyRecords.keySet()
                    .stream()
                    .collect(Collectors.toMap(t -> t, t -> DataStatus.SUCCESS));
            DataStorageAuditEvent databaseOnlyAuditEvent = new DataStorageAuditEvent();
            databaseOnlyAuditEvent.setDataIds(
                    databaseOnlyDataIds.toArray(MetadataAndDataId[]::new));
            databaseOnlyAuditEvent.setDataStatuses(dataStatuses);
            auditer.processEvent(databaseOnlyAuditEvent);
        }

        if (!datastoreOnlyRecords.isEmpty()) {
            // Datastore-only records - send metadata statuses
            DataStorageAuditUtils.auditMetadataStatuses(MetadataStatus.SUCCESS,
                    datastoreOnlyRecords.values());
        }
    }

    /**
     * Get the cluster lock that ensures we only process topo for records like
     * the one provided once.
     */
    private ClusterTask getStaticTopoClusterLock(GridRecord record) {
        String taskDetails = record.getDatasetId()
                + record.getDataTime().getRefTime();
        ClusterTask rval = null;
        do {
            /*
             * 4 minutes is needed because when static topo has not been
             * pregenerated it must go through a lengthy topo initialization
             * process. This is rare but we do not want the cluster lock to time
             * out in these cases. The worst case scenerio that has been
             * observed is generating topo for grid 233 which took 107 seconds.
             * 4 minutes leaves extra time in case other grids have worse
             * performance.
             */
            rval = ClusterLockUtils.lock(STATIC_TOPO, taskDetails,
                    4 * TimeUtil.MILLIS_PER_MINUTE, true);
        } while (!rval.getLockState().equals(LockState.SUCCESSFUL));
        return rval;
    }

    /**
     * Determine if the provided record contains data that can be used as topo.
     * The provided record should have been parsed from a notification.
     */
    private boolean isTopoCompatibleRecord(GridRecord record) {
        String param = record.getParameter().getAbbreviation();
        // must be geometric or geopotential height
        if (!GEOMETRIC_HEIGHT_PARAM.equals(param)
                && !GEOPOTENTIAL_HEIGHT_PARAM.equals(param)) {
            return false;
        }
        // must be surface level.
        if (!SURFACE_LEVEL
                .equals(record.getLevel().getMasterLevel().getName())) {
            return false;
        }
        return true;
    }

    /**
     * Load the hdf5 data for record into the message data for staticRecord.
     * This method should be used for topoCompatible records.
     */
    private void copyRawData(GridDao dao, GridRecord record,
            GridRecord staticRecord) throws PluginException {
        FloatDataRecord rawDataRecord = (FloatDataRecord) dao
                .getHDF5Data(record, -1)[0];
        staticRecord.setMessageData(rawDataRecord.getFloatData());
        staticRecord.setOverwriteAllowed(true);
    }

    /**
     * Populate the message data for a static record, loads the correct type of
     * data based of the parameter in the record.
     *
     * @throws Exception
     *             If the static topo data cannot retrieved for a GridCoverage.
     */
    private void populateMessageData(GridRecord staticRecord) throws Exception {
        if (staticRecord.getMessageData() != null) {
            // already populated, hits for static topo copied from the record.
            return;
        }
        String param = staticRecord.getParameter().getAbbreviation();
        GridCoverage loc = staticRecord.getLocation();
        FloatDataRecord fdr = null;
        if (STATIC_SPACING.equals(param) || STATIC_X_SPACING.equals(param)) {
            fdr = StaticGridData.getInstance(loc).getDx();
        } else if (STATIC_Y_SPACING.equals(param)) {
            fdr = StaticGridData.getInstance(loc).getDy();
        } else if (STATIC_CORIOLIS.equals(param)) {
            fdr = StaticGridData.getInstance(loc).getCoriolis();
        } else if (STATIC_TOPO.equals(param)) {
            fdr = StaticTopoData.getInstance().getStopoData(loc);
        }
        if (fdr != null) {
            staticRecord.setMessageData(fdr.getFloatData());
        }
    }

    /**
     * Create a new GridRecord that is a copy of the provided record but is for
     * a static parameter defined with paramInfo.
     */
    private GridRecord createStaticRecord(GridRecord record,
            ParameterInfo paramInfo) throws Exception {
        GridRecord staticRecord = null;

        staticRecord = new GridRecord(record);
        staticRecord.setId(0);
        staticRecord.setInsertTime(TimeUtil.newGmtCalendar());

        Calendar refTime = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
        refTime.setTime(record.getDataTime().getRefTime());
        DataTime dataTime = new DataTime(refTime,
                record.getDataTime().getFcstTime());

        Parameter param = new Parameter(paramInfo.getShort_name(),
                paramInfo.getLong_name(), paramInfo.getUnits());
        staticRecord.setParameter(param);
        staticRecord
                .setLevel(LevelFactory.getInstance().getLevel("Dflt", 0, "m"));
        staticRecord.setDataTime(dataTime);
        return staticRecord;

    }

    /**
     * Return a set with only records which are not already in the database
     *
     * @throws PluginException
     */
    private Map<String, GridRecord> checkDatabase(
            Map<String, GridRecord> staticRecords) throws PluginException {
        if (staticRecords.isEmpty()) {
            return staticRecords;
        }
        Map<String, GridRecord> missing = new HashMap<>();
        for (GridRecord staticRecord : staticRecords.values()) {
            // TODO a possible future optimization would be to do one bulk query
            // for all records.
            if (!DataURIDatabaseUtil.existingDataURI(staticRecord)) {
                missing.put(staticRecord.getTraceId(), staticRecord);
            }
        }
        return missing;
    }

    /**
     * Return a set with only records which are not already in the datastore
     */
    private Map<String, GridRecord> checkDatastore(GridDao dao,
            Map<String, GridRecord> staticRecords) throws Exception {
        if (staticRecords.isEmpty()) {
            return staticRecords;
        }
        List<String> datasets = null;
        Map<String, GridRecord> missing = new HashMap<>();
        for (GridRecord staticRecord : staticRecords.values()) {
            if (datasets == null) {
                try {
                    IDataStore datastore = dao.getDataStore(staticRecord);
                    String[] datasetsArr = datastore.getDatasets(
                            "/" + staticRecord.getLocation().getId());
                    datasets = Arrays.asList(datasetsArr);
                } catch (StorageException e) {
                    statusHandler.warn(
                            "Unable to load hdf5 datasets for " + staticRecord,
                            e);
                    datasets = Collections.emptyList();
                }
            }
            if (!datasets
                    .contains(staticRecord.getParameter().getAbbreviation())) {
                missing.put(staticRecord.getTraceId(), staticRecord);
            }
        }
        return missing;
    }

    /**
     * Key object that holds datasetid, time and coverage for which this
     * instance has already generated static data.
     */
    private static final class CacheKey {

        private final String datasetid;

        private final Date refTime;

        private final int forecastTime;

        private final int coverageid;

        private final String ensembleid;

        public CacheKey(GridRecord record) {
            this.datasetid = record.getDatasetId();
            this.refTime = record.getDataTime().getRefTime();
            this.forecastTime = record.getDataTime().getFcstTime();
            this.coverageid = record.getLocation().getId();
            this.ensembleid = record.getEnsembleId();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = (prime * result) + coverageid;
            result = (prime * result)
                    + ((datasetid == null) ? 0 : datasetid.hashCode());
            result = (prime * result) + forecastTime;
            result = (prime * result)
                    + ((refTime == null) ? 0 : refTime.hashCode());
            result = (prime * result)
                    + ((ensembleid == null) ? 0 : ensembleid.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            CacheKey other = (CacheKey) obj;
            if (coverageid != other.coverageid) {
                return false;
            }
            if (!Objects.equals(datasetid, other.datasetid)) {
                return false;
            }
            if (forecastTime != other.forecastTime) {
                return false;
            }
            if (!Objects.equals(refTime, other.refTime)) {
                return false;
            }
            if (!Objects.equals(ensembleid, other.ensembleid)) {
                return false;
            }
            return true;
        }

    }
}
