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

package com.raytheon.uf.edex.database.plugin;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.database.purge.PurgeRule;
import com.raytheon.uf.edex.database.purge.PurgeRulePK;
import com.raytheon.uf.edex.database.purge.PurgeRuleSet;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Abstract implementation of a Plugin data access object
 * <p>
 * Plugins wishing to store data in the HDF5 repository may extend this class
 * directly. If HDF5 persistance is not necessary, plugins may use or extend
 * DefaultPluginDao
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/6/09       1990       bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public abstract class PluginDao extends CoreDao {

    /** The default retention time to retain data */
    public static final int DEFAULT_RETENTION_TIME = 24;

    /** The default tile to retrieve. Currently unused */
    public static final int DEFAULT_TILE = -1;

    /** The hdf5 file system suffix */
    public static final String HDF5_SUFFIX = ".h5";

    /** The base path of the hdf5 data store */
    public static final String HDF5_DIR = PropertiesFactory.getInstance()
            .getEnvProperties().getEnvValue("HDF5DIR");

    /** The base path of the folder containing HDF5 data for the owning plugin */
    public final String PLUGIN_HDF5_DIR;

    /** The path provider for determining paths to hdf5 data */
    public IHDFFilePathProvider pathProvider;

    /** The owning plugin name */
    protected String pluginName;

    /** The sql string used to check for duplicates in the database */
    protected String dupCheckSql = "select id from awips.:tableName where dataURI=':dataURI'";

    protected static final String PURGE_VERSION_FIELD = "dataTime.refTime";

    /**
     * Constructs a new PluginDao for the given plugin
     * 
     * @param pluginName
     *            The name of the plugin to create the data access object for
     * @throws PluginException
     *             If problems occur while gathering data about the data type
     *             plugin
     */
    public PluginDao(String pluginName) throws PluginException {
        super(DaoConfig.forDatabase(PluginFactory.getInstance().getDatabase(
                pluginName)));
        Class<PluginDataObject> clazz = PluginFactory.getInstance()
                .getPluginRecordClass(pluginName);
        if (clazz != null) {
            this.setDaoClass(clazz);
        }

        this.pluginName = pluginName;
        PLUGIN_HDF5_DIR = HDF5_DIR + File.separator + pluginName
                + File.separator;
        dupCheckSql = dupCheckSql.replace(":tableName", PluginFactory
                .getInstance().getPrimaryTable(pluginName));
        pathProvider = PluginFactory.getInstance().getPathProvider(pluginName);
    }

    /**
     * Defines the behavior for storing data to the HDF5 data store
     * 
     * @param dataStore
     *            The datastore to save the data to
     * @param obj
     *            The object containing the data to be stored
     * @return The updated data store
     * @throws Exception
     *             if problems occur while interacting with the HDF5 data store
     */
    protected abstract IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception;

    /**
     * Persists a group of records to the data stores
     * 
     * @param records
     *            The records to persist
     * @throws PluginException
     *             If problems occur while interacting with the HDF5 data stores
     */
    public void persistRecords(PluginDataObject... records)
            throws PluginException {
        persistToHDF5(records);
        persistToDatabase(records);
    }

    public PluginDataObject[] persistToDatabase(PluginDataObject... records) {
        List<PersistableDataObject> toPersist = new ArrayList<PersistableDataObject>();
        for (PluginDataObject record : records) {
            toPersist.add(record);
        }
        List<PersistableDataObject> duplicates = mergeAll(toPersist);
        for (PersistableDataObject pdo : duplicates) {
            logger.info("Discarding duplicate: "
                    + ((PluginDataObject) (pdo)).getDataURI());
            toPersist.remove(pdo);
        }
        return toPersist.toArray(new PluginDataObject[toPersist.size()]);
    }

    /**
     * Persists the HDF5 component of the records to the HDF5 repository
     * 
     * @param records
     *            The records to persist
     * @return The status of the storage operation
     * @throws PluginException
     *             If problems occur while interacting with the HDF5 data
     *             storesr
     */
    public StorageStatus persistToHDF5(PluginDataObject... records)
            throws PluginException {
        // Step 1: sort the objects by the file they belong to
        Map<File, List<IPersistable>> persistableMap = new HashMap<File, List<IPersistable>>(
                records.length);

        for (PluginDataObject pdo : records) {
            if (pdo instanceof IPersistable) {
                IPersistable persistable = (IPersistable) pdo;

                // get the directory
                String directory = HDF5_DIR
                        + File.separator
                        + pdo.getPluginName()
                        + File.separator
                        + pathProvider.getHDFPath(pdo.getPluginName(),
                                persistable);
                File dataStoreFile = new File(directory
                        + File.separator
                        + pathProvider.getHDFFileName(pdo.getPluginName(),
                                persistable));

                List<IPersistable> pdoList = persistableMap.get(dataStoreFile);
                if (pdoList == null) {
                    pdoList = new ArrayList<IPersistable>();
                    persistableMap.put(dataStoreFile, pdoList);
                }

                pdoList.add(persistable);
            }
        }

        // Step 2: Iterate through all the files, and persist all records that
        // belong to each file in bulk

        Iterator<File> fileIterator = persistableMap.keySet().iterator();
        List<StorageException> exceptions = new ArrayList<StorageException>();
        while (fileIterator.hasNext()) {
            File file = fileIterator.next();
            List<IPersistable> persistables = persistableMap.get(file);

            // create the directories necessary for this file, if they do not
            // exist
            // File directory = file.getParentFile();
            // if (!directory.exists()) {
            // directory.mkdirs();
            // }

            IDataStore dataStore = DataStoreFactory.getDataStore(file);
            IDataStore replaceDataStore = null;

            for (IPersistable persistable : persistables) {
                try {
                    if (((PersistableDataObject) persistable)
                            .isOverwriteAllowed()) {
                        if (replaceDataStore == null) {
                            replaceDataStore = DataStoreFactory
                                    .getDataStore(file);
                        }

                        populateDataStore(replaceDataStore, persistable);
                    } else {
                        populateDataStore(dataStore, persistable);
                    }
                } catch (Exception e) {
                    throw new PluginException("Error populating data store", e);
                }
            }

            try {
                StorageStatus s = dataStore.store();
                // add exceptions to a list for aggregation
                exceptions.addAll(Arrays.asList(s.getExceptions()));
            } catch (StorageException e) {
                logger.error("Error persisting to HDF5", e);
            }

            if (replaceDataStore != null) {
                try {
                    StorageStatus s = replaceDataStore.store(StoreOp.REPLACE);
                    // add exceptions to a list for aggregation
                    exceptions.addAll(Arrays.asList(s.getExceptions()));
                } catch (StorageException e) {
                    logger.error("Error persisting replace records to HDF5", e);
                }
            }
        }

        // Create an aggregated status object
        StorageStatus status = new StorageStatus();
        status.setExceptions(exceptions.toArray(new StorageException[exceptions
                .size()]));
        return status;
    }

    /**
     * Retrieves metadata from the database according to the provided query
     * 
     * @param query
     *            The query
     * @return The query results
     * @throws DataAccessLayerException
     *             If problems occur while interacting with the database
     */
    @SuppressWarnings("unchecked")
    public PluginDataObject[] getMetadata(DatabaseQuery query)
            throws PluginException {
        try {
            return ((List<PluginDataObject>) super.queryByCriteria(query))
                    .toArray(new PluginDataObject[] {});
        } catch (DataAccessLayerException e) {
            throw new PluginException("Error getting metadata", e);
        }
    }

    /**
     * Retrieves the complete set of metadata from the database for the record
     * with the provided dataURI
     * 
     * @param dataURI
     *            The dataURI of the record for which to retrieve metadata
     * @return The record populated with a complete set of metadata
     * @throws DataAccessLayerException
     *             If problems occur while interacting with the database
     */
    @SuppressWarnings("unchecked")
    public PluginDataObject getMetadata(String dataURI) throws PluginException {
        List<PluginDataObject> result;
        try {
            result = ((List<PluginDataObject>) queryBySingleCriteria("dataURI",
                    dataURI));
        } catch (DataAccessLayerException e) {
            throw new PluginException("Error getting metadata", e);
        }
        if (result.isEmpty()) {
            return null;
        }
        return result.get(0);
    }

    /**
     * Retrieves the HDF5 component of the records provided
     * 
     * @param objects
     *            The objects to retrieve the HDF5 component for
     * @param tileSet
     *            The tile set to retrieve. Currently unimplemented
     * @return The HDF5 data records
     * @throws StorageException
     *             If problems occur while interacting with HDF5 data stores
     */
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {

        List<IDataRecord[]> retVal = new ArrayList<IDataRecord[]>();

        for (PluginDataObject obj : objects) {
            IDataRecord[] record = null;

            if (obj instanceof IPersistable) {
                /* connect to the data store and retrieve the data */
                IDataStore dataStore = getDataStore((IPersistable) obj);
                if (tileSet != -1) {
                    record = new IDataRecord[tileSet + 1];
                    for (int i = 0; i <= tileSet; i++) {
                        try {
                            record[i] = dataStore.retrieve(obj.getDataURI()
                                    + File.separator + "Data-interpolated"
                                    + File.separator, String.valueOf(tileSet),
                                    Request.ALL);
                        } catch (Exception e) {
                            throw new PluginException(
                                    "Error getting HDF5 data", e);
                        }
                    }
                } else {
                    record = new IDataRecord[1];
                    try {
                        record[0] = dataStore.retrieve(obj.getDataURI(),
                                "Data", Request.ALL);
                    } catch (Exception e) {
                        throw new PluginException("Error getting HDF5 data", e);
                    }
                }
                retVal.add(record);
            }
        }

        return retVal;
    }

    /**
     * Retrieves the HDF5 component of the record provided
     * 
     * @param object
     *            The objects to retrieve the HDF5 component for
     * @param tileSet
     *            The tile set to retrieve. Currently unimplemented
     * @return The HDF5 data records
     * @throws StorageException
     *             If problems occur while interacting with HDF5 data stores
     */
    public IDataRecord[] getHDF5Data(PluginDataObject object, int tile)
            throws PluginException {
        return getHDF5Data(Arrays.asList(new PluginDataObject[] { object }),
                tile).get(0);
    }

    /**
     * Retrieves the fully populated object from the data stores according to
     * the provided query
     * 
     * @param query
     *            The query to execute
     * @param tileSet
     *            The tile set to retrieve. Currently unimplemented
     * @return The results of the query
     * @throws DataAccessLayerException
     *             If problems occur while interacting with the metadata
     *             database
     * @throws StorageException
     *             If probolems occur while interacting with the metadata
     *             database
     */
    public PluginDataObject[] getFullRecord(DatabaseQuery query, int tile)
            throws PluginException {
        PluginDataObject[] queryResults = getMetadata(query);
        for (PluginDataObject obj : queryResults) {
            obj.setPluginName(pluginName);
            obj.setMessageData(getHDF5Data(obj, tile));
        }
        return queryResults;
    }

    public void initializePlugin() throws PluginException {

    }

    /**
     * Purges all data associated with the owning plugin based on criteria
     * specified by the owning plugin
     * 
     * @throws PluginException
     *             If problems occur while interacting with the data stores
     */
    public void purgeAllData() throws PluginException {
        try {
            List<Date> allRefTimes = getRefTimes();
            Set<Date> roundedRefTimes = new HashSet<Date>();
            for (Date d : allRefTimes) {
                this.purgeDataByRefTime(d, "");
                roundedRefTimes.add(roundDateToHour(d));
            }
            for (Date d : roundedRefTimes) {
                this.purgeHDF5DataByRefTime(d, "");
            }
        } catch (Exception e) {
            throw new PluginException("Error purging all data for "
                    + pluginName + " plugin.", e);
        }
    }

    /**
     * Purges data according to purge criteria specified by the owning plugin
     * 
     * @throws PluginException
     *             If problems occur while interacting with data stores
     */
    public void purgeExpiredData() throws PluginException {
        try {
            PluginDao pluginDao = PluginFactory.getInstance().getPluginDao(
                    pluginName);
            List<PurgeRule> ruleList = new ArrayList<PurgeRule>();
            ruleList.addAll(getPurgeRulesForPlugin(pluginName));

            /*
             * This section generates default rules for data not addressed by
             * the rule set accompanying this plugin. This prevents orphaned
             * data.
             */
            Set<String> keysWithoutRules = new HashSet<String>();

            // Query the database to get all possible product keys for this data
            // and assume they have no rules associated with them
            keysWithoutRules.addAll(getDistinctProductKeys());
            Set<String> keysWithRules = new HashSet<String>();
            PurgeRule pluginDefaultRule = null;

            // Get the default rule for this plugin if it specifies one.
            for (PurgeRule rule : ruleList) {
                if (rule.getId().getKey().equals("default")) {
                    pluginDefaultRule = rule;
                    continue;
                }
                keysWithRules.add(rule.getId().getKey());
            }

            // If this plugin does not specify a default rule, the system
            // default will be used
            if (pluginDefaultRule == null) {
                pluginDefaultRule = loadDefaultPurgeRule();
                if (pluginDefaultRule == null) {
                    PurgeLogger.logError("Unable to purge plugin", pluginName);
                }
            } else {
                ruleList.remove(pluginDefaultRule);
            }

            // Remove the keys we have determined to have rules associated with
            // them
            keysWithoutRules.removeAll(keysWithRules);

            // If keys remain without rules, we iterate over them and assign the
            // default rule
            if (!keysWithoutRules.isEmpty()) {
                PurgeLogger.logWarn("Data exists with no purge rule.",
                        pluginName);
                for (String keyNeedingRule : keysWithoutRules) {
                    PurgeLogger.logWarn(
                            "Generated default purge rule for key: "
                                    + keyNeedingRule, pluginName);
                    PurgeRule newRule = (PurgeRule) pluginDefaultRule.clone();
                    newRule.getId().setPluginName(pluginName);
                    newRule.getId().setKey(keyNeedingRule);
                    ruleList.add(newRule);
                }
            }

            // If no purge rules are specified for this plugin, we assign the
            // default rule so data for this plugin is still purged
            if (ruleList.isEmpty()) {
                PurgeLogger.logWarn("No purge rules specified.  Using default",
                        pluginName);
                pluginDefaultRule.getId().setPluginName(pluginName);
                pluginDefaultRule.getId().setKey("");
                ruleList.add(pluginDefaultRule);
            }

            // Map containing the times to retain for each rule
            Map<PurgeRulePK, Set<Date>> timesToKeep = new HashMap<PurgeRulePK, Set<Date>>();

            // Map containing the times to purge for each rule
            Map<PurgeRulePK, Set<Date>> timesToPurge = new HashMap<PurgeRulePK, Set<Date>>();

            // Iterate through the rules and determine the times to purge
            for (PurgeRule rule : ruleList) {

                // If this rule is not valid, we cannot apply it
                if (!isRuleValid(rule)) {
                    PurgeLogger.logWarn("Ignoring Invalid Rule: " + rule,
                            this.pluginName);
                    continue;
                }

                // Holds the times kept by this rule
                List<Date> timesKeptByRule = new ArrayList<Date>();

                Set<Date> roundedTimes = new HashSet<Date>();

                // Holds the times to be purged by this rule
                List<Date> timesPurgedByRule = new ArrayList<Date>();

                PurgeRulePK rulePK = rule.getId();
                if (!timesToKeep.containsKey(rulePK)) {
                    timesToKeep.put(rulePK, new HashSet<Date>());
                }
                if (!timesToPurge.containsKey(rulePK)) {
                    timesToPurge.put(rulePK, new HashSet<Date>());
                }

                String key = rulePK.getKey();

                /*
                 * This section applies the purge rule
                 */

                List<Date> refTimesForKey = pluginDao
                        .getRefTimesForCriteria(key);

                if (rule.isModTimeToWaitSpecified()) {
                    Date maxInsertTime = pluginDao.getMaxInsertTime(key);
                    if (maxInsertTime != null) {
                        long lastInsertTime = maxInsertTime.getTime();
                        long currentTime = System.currentTimeMillis();
                        if ((currentTime - lastInsertTime) < rule
                                .getModTimeToWaitInMillis()) {
                            PurgeLogger
                                    .logInfo(
                                            "For key, "
                                                    + key
                                                    + ", the most recent version is less than "
                                                    + rule.getModTimeToWaitDescription()
                                                    + " old. Increasing versions to keep for this key.",
                                            pluginName);
                            rule.setVersionsToKeep(rule.getVersionsToKeep() + 1);
                        }
                    }
                }

                // Calculate the period cutoff time if necessary
                Date periodCutoffTime = new Date();
                if (rule.isPeriodSpecified()) {
                    if (rule.isPeriodBasedOnLatestTime()) {
                        Date maxRefTime = pluginDao.getMaxRefTime(key);
                        if (maxRefTime == null) {
                            PurgeLogger.logInfo("No data available to purge",
                                    pluginName);
                            continue;
                        } else {
                            periodCutoffTime = new Date(maxRefTime.getTime()
                                    - rule.getPeriodInMillis());
                        }
                    } else {
                        periodCutoffTime = new Date(System.currentTimeMillis()
                                - rule.getPeriodInMillis());
                    }
                }

                // Filter the keepers by the delta time specified
                if (rule.isDeltaSpecified()) {
                    for (Date refTime : refTimesForKey) {
                        Date timeToCompare = rule.getRoundedDate(refTime)[1];
                        long delta = rule.getDeltaTimeInMillis();
                        long dateTimeAsLong = timeToCompare.getTime();

                        if (rule.isDeltaTimeMultiple()) {
                            if (dateTimeAsLong % delta == 0) {
                                // If the versions to keep is zero we keep it if
                                // it does not exceed the period specified, if
                                // any
                                if (rule.getVersionsToKeep() == 0) {
                                    if (rule.isPeriodSpecified()
                                            && refTime.before(periodCutoffTime)) {
                                        timesPurgedByRule.add(refTime);

                                    } else {
                                        timesKeptByRule.add(refTime);
                                    }
                                }

                                // If the versions to keep is not zero and
                                // adding this will not exceed the specified
                                // number of versions to keep and it does not
                                // exceed the period specified, the time is kept
                                else if (rule.getVersionsToKeep() > 0) {
                                    if (rule.isRoundSpecified()) {
                                        if (roundedTimes.size() < rule
                                                .getVersionsToKeep()) {
                                            roundedTimes.add(timeToCompare);
                                            timesKeptByRule.add(refTime);
                                        } else {
                                            timesPurgedByRule.add(refTime);
                                        }
                                    } else {
                                        if (timesKeptByRule.size() < rule
                                                .getVersionsToKeep()) {
                                            if (rule.isPeriodSpecified()
                                                    && refTime
                                                            .before(periodCutoffTime)) {
                                                timesPurgedByRule.add(refTime);
                                            } else {
                                                timesKeptByRule.add(refTime);
                                            }
                                        }
                                    }

                                }
                            } else {
                                timesPurgedByRule.add(refTime);
                            }
                        }
                    }
                }

                /*
                 * If a versions to keep is specified, determine the versions to
                 * keep. If a delta is specified for this rule, then the
                 * versions have already been calculated based on the delta
                 * time. This section is used only if a delta time is not used
                 */
                else if (!rule.isDeltaSpecified()
                        && rule.isVersionsToKeepSpecified()) {
                    Date currentRefTime = null;
                    for (int i = 0; i < refTimesForKey.size(); i++) {
                        currentRefTime = refTimesForKey.get(i);
                        if (i < rule.getVersionsToKeep()) {
                            if (rule.isPeriodSpecified()
                                    && currentRefTime.before(periodCutoffTime)) {
                                timesPurgedByRule.add(currentRefTime);
                            } else {
                                timesKeptByRule.add(currentRefTime);
                            }
                            timesKeptByRule.add(currentRefTime);
                        } else {
                            timesPurgedByRule.add(currentRefTime);
                        }

                    }
                }

                /*
                 * This rule only specifies a time cutoff
                 */
                else if (!rule.isDeltaSpecified()
                        && !rule.isVersionsToKeepSpecified()
                        && rule.isPeriodSpecified()) {
                    for (Date currentRefTime : refTimesForKey) {
                        if (currentRefTime.before(periodCutoffTime)) {
                            timesPurgedByRule.add(currentRefTime);
                        } else {
                            timesKeptByRule.add(currentRefTime);
                        }
                    }
                }

                /*
                 * This rule has been so poorly written that it does nothing
                 */
                else {
                    PurgeLogger
                            .logInfo(
                                    "Purge rule does not specify a delta, period, or versions to keep.",
                                    pluginName);
                }

                /*
                 * If log only is specified, log the results but purge nothing
                 */
                if (rule.isLogOnly()) {
                    PurgeLogger.logInfo("Rule is configured to log only",
                            pluginName);
                    PurgeLogger.logInfo(
                            "These version would be removed by the rule:",
                            pluginName);
                    Collections.sort(timesPurgedByRule);
                    Collections.sort(timesKeptByRule);
                    for (Date d : timesPurgedByRule) {
                        PurgeLogger.logInfo(d.toString(), pluginName);
                    }
                    PurgeLogger
                            .logInfo(
                                    "These versions would have been retained by the rule:",
                                    pluginName);
                    for (Date d : timesKeptByRule) {
                        PurgeLogger.logInfo(d.toString(), pluginName);
                    }

                } else {
                    timesToKeep.get(rulePK).addAll(timesKeptByRule);
                    timesToPurge.get(rulePK).addAll(timesPurgedByRule);
                }

                // We must remove the keep times from the purge list. This
                // ensures that if the time passes at least one rule, then it
                // will be retained
                timesToPurge.get(rulePK).removeAll(timesToKeep.get(rulePK));
            }

            // Counter for recording the number of items purged for a given time
            int itemsDeletedForTime = 0;

            // The total number of items purged
            int totalItems = 0;

            for (PurgeRulePK purgeKey : timesToPurge.keySet()) {
                String key = purgeKey.getKey();

                int itemsDeletedForKey = 0;
                for (Date deleteDate : timesToPurge.get(purgeKey)) {

                    // Delete the data in the database
                    itemsDeletedForTime = pluginDao.purgeDataByRefTime(
                            deleteDate, key);

                    totalItems += itemsDeletedForTime;
                    itemsDeletedForKey += itemsDeletedForTime;
                }
                if (itemsDeletedForKey > 0) {
                    StringBuilder messageBuffer = new StringBuilder();
                    messageBuffer.append("Purged ").append(itemsDeletedForKey)
                            .append(" item");
                    if (itemsDeletedForKey != 1) {
                        messageBuffer.append("s");
                    }
                    if (key.equals("default")) {
                        messageBuffer.append(" for default key");
                    } else if (!key.isEmpty()) {
                        messageBuffer.append(" for key ").append(key);
                    }
                    PurgeLogger.logInfo(messageBuffer.toString(), pluginName);
                }
                boolean purgeHDF5Data = false;
                try {
                    // Determine if this plugin uses HDF5 to store data
                    purgeHDF5Data = (PluginFactory.getInstance()
                            .getPluginRecordClass(pluginName).newInstance() instanceof IPersistable);
                } catch (Exception e) {
                    throw new DataAccessLayerException(
                            "Error instantiating plugin record class!", e);
                }
                if (purgeHDF5Data) {

                    /*
                     * If this plugin stores data in HDF5, we must round the
                     * times to hour granularity since the files are named to
                     * the hour. Rounding the time allows us to delete files
                     * instead of deleting individual records from the HDF5
                     * file.
                     */
                    Set<Date> roundedTimesToPurge = new HashSet<Date>();
                    Set<Date> roundedTimesToKeep = new HashSet<Date>();

                    for (Date dateToRound : timesToKeep.get(purgeKey)) {
                        roundedTimesToKeep.add(roundDateToHour(dateToRound));
                    }
                    for (Date dateToRound : timesToPurge.get(purgeKey)) {
                        roundedTimesToPurge.add(roundDateToHour(dateToRound));
                    }

                    // Make sure not files are erroneously purged
                    roundedTimesToPurge.removeAll(roundedTimesToKeep);

                    // Delete the HDF5 files that are no longer referenced
                    for (Date deleteDate : roundedTimesToPurge) {
                        this.purgeHDF5DataByRefTime(deleteDate,
                                purgeKey.getKey());
                    }
                }

            }
            StringBuilder messageBuffer = new StringBuilder();
            messageBuffer.append("Purged ").append(totalItems).append(" item");
            if (totalItems != 1) {
                messageBuffer.append("s");
            }
            messageBuffer.append(" total.");

            PurgeLogger.logInfo(messageBuffer.toString(), pluginName);

            // Debug output to see which times were retained
            if (PurgeLogger.isDebugEnabled()) {
                for (PurgeRulePK purgeKey : timesToKeep.keySet()) {
                    List<Date> keepers = new ArrayList<Date>();
                    keepers.addAll(timesToKeep.get(purgeKey));
                    if (!keepers.isEmpty()) {
                        StringBuilder builder = new StringBuilder();
                        Collections.sort(keepers);
                        builder.append("The following times were retained");
                        if (purgeKey.getKey().isEmpty()) {
                            builder.append(":");
                        } else {
                            builder.append(" for key ")
                                    .append(purgeKey.getKey()).append(":");
                        }

                        for (Date keepDate : keepers) {
                            builder.append("[").append(keepDate).append("]")
                                    .append(" ");
                        }
                        PurgeLogger.logDebug(builder.toString(), pluginName);
                    }
                }
            }

        } catch (EdexException e) {
            throw new PluginException("Error applying purge rule!!", e);
        }
    }

    /**
     * Gets the data store for the given object
     * 
     * @param obj
     *            The object for which to get the data store
     * @return The data store
     */
    public IDataStore getDataStore(IPersistable obj) {
        String persistDir = PLUGIN_HDF5_DIR
                + pathProvider.getHDFPath(this.pluginName, obj)
                + File.separator;
        String archive = pathProvider.getHDFFileName(this.pluginName, obj);

        File persistFile = new File(persistDir, archive);
        /* connect to the data store and retrieve the data */
        return DataStoreFactory.getDataStore(persistFile);
    }

    /**
     * Gets a list of the distinct product keys for this plugin
     * 
     * @return The list of distinct product keys for this plugin
     * @throws DataAccessLayerException
     *             If errors occur while querying for the data
     */
    @SuppressWarnings("unchecked")
    public Set<String> getDistinctProductKeys() throws DataAccessLayerException {
        Set<String> distinctKeys = new HashSet<String>();
        List<String> keyFields = this.pathProvider.getKeyNames(pluginName);
        if (!keyFields.isEmpty()) {
            DatabaseQuery query = new DatabaseQuery(this.daoClass);
            for (int i = 0; i < keyFields.size(); i++) {
                if (i == 0) {
                    query.addDistinctParameter(keyFields.get(i));
                } else {
                    query.addReturnedField(keyFields.get(i));
                }
            }
            if (keyFields.size() == 1) {
                List<?> results = this.queryByCriteria(query);
                for (int i = 0; i < results.size(); i++) {
                    distinctKeys.add(keyFields.get(0) + "="
                            + results.get(i).toString());
                }
            } else {
                List<Object[]> results = (List<Object[]>) this
                        .queryByCriteria(query);
                for (Object[] result : results) {
                    StringBuilder newKey = new StringBuilder();
                    for (int i = 0; i < result.length; i++) {
                        newKey.append(keyFields.get(i) + "=" + result[i]);
                        if (i != result.length - 1) {
                            newKey.append(";");
                        }
                    }
                    distinctKeys.add(newKey.toString());
                }
            }
        }
        return distinctKeys;
    }

    /**
     * Gets all distinct reference times for this plugin
     * 
     * @param productKey
     *            The product key to get the list of reference times for
     * @return A list of distinct reference times for the given productKey
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<Date> getRefTimes() throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addDistinctParameter(PURGE_VERSION_FIELD);
        query.addOrder(PURGE_VERSION_FIELD, false);
        List<Date> times = (List<Date>) this.queryByCriteria(query);
        return times;
    }

    /**
     * Gets all distinct reference times for the given productKey
     * 
     * @param productKey
     *            The product key to get the list of reference times for
     * @return A list of distinct reference times for the given productKey
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<Date> getRefTimesForCriteria(String productKey)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        List<String[]> keys = this.getProductKeyParameters(productKey);
        for (String[] key : keys) {
            query.addQueryParam(key[0], key[1]);
        }
        query.addDistinctParameter(PURGE_VERSION_FIELD);
        query.addOrder(PURGE_VERSION_FIELD, false);
        List<Date> times = (List<Date>) this.queryByCriteria(query);
        return times;
    }

    /**
     * Purges data from the database for this plugin with the given reference
     * time matching the given productKy
     * 
     * @param refTime
     *            The reftime to delete data for
     * @param productKey
     *            The key to use as a constraint for deletions
     * @return
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public int purgeDataByRefTime(Date refTime, String productKey)
            throws DataAccessLayerException {

        int results = 0;
        DatabaseQuery dataQuery = new DatabaseQuery(this.daoClass);
        dataQuery.addQueryParam(PURGE_VERSION_FIELD, refTime);
        List<String[]> keys = this.getProductKeyParameters(productKey);
        for (String[] key : keys) {
            dataQuery.addQueryParam(key[0], key[1]);
        }

        List<Integer> idList = null;
        DatabaseQuery idQuery = null;

        dataQuery.addReturnedField("id");
        dataQuery.setMaxResults(500);
        do {
            idList = (List<Integer>) this.queryByCriteria(dataQuery);
            if (!idList.isEmpty()) {
                idQuery = new DatabaseQuery(this.daoClass);
                idQuery.addQueryParam("id", idList, QueryOperand.IN);
                List<PluginDataObject> pdos = (List<PluginDataObject>) this
                        .queryByCriteria(idQuery);
                this.delete(pdos);
                results += pdos.size();
            }

        } while (idList != null && !idList.isEmpty());

        return results;
    }

    /**
     * Purges the HDF5 data according to the provided time and key.
     * 
     * @param refTime
     *            The time to delete
     * @param productKey
     *            The key to delete
     */
    public void purgeHDF5DataByRefTime(Date refTime, String productKey)
            throws DataAccessLayerException {
        IDataStore dataStore = DataStoreFactory.getDataStore(new File(this
                .getHDF5Path(productKey)));
        try {
            dataStore.deleteFiles(new String[] { File.separator
                    + this.pluginName + File.separator
                    + TimeUtil.formatDate(refTime) });
        } catch (Exception e) {
            throw new DataAccessLayerException("Error purging hdf5 data", e);
        }
    }

    /**
     * Gets the maximum reference time contained in the database for the given
     * key. The key corresponds to the productKey field in the data object.
     * 
     * @param productKey
     *            The key for which to get the maximum reference time
     * @return Null if this key was not found, else the maximum reference time
     * @throws DataAccessLayerException
     *             If errors occur while querying the database
     */
    @SuppressWarnings("unchecked")
    public Date getMaxRefTime(String productKey)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addDistinctParameter(PURGE_VERSION_FIELD);
        List<String[]> keys = this.getProductKeyParameters(productKey);
        for (String[] key : keys) {
            query.addQueryParam(key[0], key[1]);
        }
        query.addOrder(PURGE_VERSION_FIELD, false);
        query.setMaxResults(1);
        List<Date> result = (List<Date>) this.queryByCriteria(query);
        if (result.isEmpty()) {
            return null;
        } else {
            return result.get(0);
        }
    }

    /**
     * Gets the maximum insert time contained in the database for the given key.
     * The key corresponds to the productKey field in the data object.
     * 
     * @param productKey
     *            The key for which to get the maximum insert time
     * @return Null if this key was not found, else the maximum insert time
     * @throws DataAccessLayerException
     *             If errors occur while querying the database
     */
    @SuppressWarnings("unchecked")
    public Date getMaxInsertTime(String productKey)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        // doing distinct is wasted with a ordered max
        // query.addDistinctParameter("insertTime");
        List<String[]> keys = this.getProductKeyParameters(productKey);
        for (String[] key : keys) {
            query.addQueryParam(key[0], key[1]);
        }
        query.addReturnedField("insertTime");
        query.addOrder("insertTime", false);
        query.setMaxResults(1);
        List<Calendar> result = (List<Calendar>) this.queryByCriteria(query);
        if (result.isEmpty()) {
            return null;
        } else {
            return result.get(0).getTime();
        }
    }

    /**
     * Gets the minimum insert time contained in the database for the given key.
     * The key corresponds to the productKey field in the data object.
     * 
     * @param productKey
     *            The key for which to get the minimum reference time
     * @return Null if this key was not found, else the minimum reference time
     * @throws DataAccessLayerException
     *             If errors occur while querying the database
     */
    @SuppressWarnings("unchecked")
    public Date getMinInsertTime(String productKey)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        // doing distinct is wasted with a ordered max
        // query.addDistinctParameter("insertTime");
        List<String[]> keys = this.getProductKeyParameters(productKey);
        for (String[] key : keys) {
            query.addQueryParam(key[0], key[1]);
        }
        query.addReturnedField("insertTime");
        query.addOrder("insertTime", true);
        query.setMaxResults(1);
        List<Calendar> result = (List<Calendar>) this.queryByCriteria(query);
        if (result == null || result.isEmpty()) {
            return null;
        } else {
            return result.get(0).getTime();
        }
    }

    /**
     * Gets the minimum reference time contained in the database for the given
     * key. The key corresponds to the productKey field in the data object.
     * 
     * @param productKey
     *            The key for which to get the minimum reference time
     * @return Null if this key was not found, else the minimum reference time
     * @throws DataAccessLayerException
     *             If errors occur while querying the database
     */
    @SuppressWarnings("unchecked")
    public Date getMinRefTime(String productKey)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addDistinctParameter(PURGE_VERSION_FIELD);
        List<String[]> keys = this.getProductKeyParameters(productKey);
        for (String[] key : keys) {
            query.addQueryParam(key[0], key[1]);
        }
        query.addOrder(PURGE_VERSION_FIELD, true);
        query.setMaxResults(1);
        List<Date> result = (List<Date>) this.queryByCriteria(query);
        if (result.isEmpty()) {
            return null;
        } else {
            return result.get(0);
        }
    }

    /**
     * Breaks the product key into key value pairs.
     * 
     * @param productKey
     *            The product key to break apart into pairs
     * @return The product key/value pairs
     */
    protected List<String[]> getProductKeyParameters(String productKey) {
        List<String[]> params = new ArrayList<String[]>();
        if (productKey.isEmpty()) {
            return params;
        }

        String[] keyTokens = productKey.trim().split(";");
        for (int i = 0; i < keyTokens.length; i++) {
            String[] constraintTokens = keyTokens[i].split("=");
            constraintTokens[0] = constraintTokens[0].trim();
            constraintTokens[1] = constraintTokens[1].trim();
            params.add(constraintTokens);
        }
        return params;
    }

    /**
     * Gets the path to the HDF5 file based on the provided product key
     * 
     * @param productKey
     *            The product key for which to generate the path to the hdf5
     *            file
     * @return The path to the hdf5 file based on the provided product key
     */
    protected String getHDF5Path(String productKey) {
        StringBuilder pathBuilder = new StringBuilder();
        List<String[]> params = getProductKeyParameters(productKey);
        pathBuilder.append(this.PLUGIN_HDF5_DIR);
        for (String[] param : params) {
            pathBuilder.append(File.separator + param[1]);
        }
        return pathBuilder.toString();
    }

    /**
     * Checks to see if a rule is valid. A rule is valid if the keys used in
     * this rule are listed in the pathKey.xml file for this plugin.
     * 
     * @param rule
     *            The rule to check
     * @return True if the rule is valid. False if the rule is invalid
     */
    private boolean isRuleValid(PurgeRule rule) {
        boolean retVal = true;
        List<String[]> keyParams = getProductKeyParameters(rule.getId()
                .getKey());
        List<String> keyNames = this.pathProvider.getKeyNames(pluginName);
        for (String[] param : keyParams) {
            if (!keyNames.contains(param[0])) {
                retVal = false;
            }
        }
        return retVal;
    }

    private Date roundDateToHour(Date dateToRound) {
        return new Date(dateToRound.getTime() - dateToRound.getTime() % 3600000);
    }

    /**
     * Deletes an object from the database
     * 
     * @param obj
     *            The object to delete
     */
    public void delete(final List<PluginDataObject> objs) {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            public void doInTransactionWithoutResult(TransactionStatus status) {
                getHibernateTemplate().deleteAll(objs);
            }
        });
    }

    public static List<PurgeRule> getPurgeRulesForPlugin(String pluginName) {
        File rulesFile = PathManagerFactory.getPathManager().getStaticFile(
                "purge/" + pluginName + "PurgeRules.xml");
        if (rulesFile != null) {
            try {
                PurgeRuleSet purgeRules = (PurgeRuleSet) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(rulesFile);
                return purgeRules.getRules();
            } catch (SerializationException e) {
                PurgeLogger
                        .logError(
                                "Error deserializing purge rules! Data will not be purged. Please define rules.",
                                pluginName);
            }
        }
        return Collections.emptyList();
    }

    public static PurgeRule loadDefaultPurgeRule() {
        File defaultRule = PathManagerFactory.getPathManager().getStaticFile(
                "purge/defaultPurgeRules.xml");
        if (defaultRule == null) {
            PurgeLogger.logError("Default purge rule not defined!!", "EDEX");
            statusHandler
                    .error("Default purge rule not defined!!  Data will not be purged for plugins which do not specify purge rules!");
            return null;
        }
        try {
            PurgeRuleSet purgeRules = (PurgeRuleSet) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(defaultRule);
            return purgeRules.getRules().get(0);
        } catch (SerializationException e) {
            PurgeLogger.logError("Error deserializing default purge rule!",
                    "DEFAULT");
        }
        return null;
    }

    public void archiveData(String archivePath, Calendar insertStartTime,
            Calendar insertEndTime) throws DataAccessLayerException,
            SerializationException, IOException {
        List<PersistableDataObject> pdos = getRecordsToArchive(insertStartTime,
                insertEndTime);
        if (pdos != null && pdos.size() > 0) {
            // map of file to list of pdo
            Map<String, List<PersistableDataObject>> pdoMap = new HashMap<String, List<PersistableDataObject>>();
            if (pdos.get(0) instanceof IPersistable) {
                IHDFFilePathProvider pathProvider = this.pathProvider;

                for (PersistableDataObject pdo : pdos) {
                    IPersistable persistable = (IPersistable) pdo;
                    String path = pathProvider.getHDFPath(pluginName,
                            persistable)
                            + File.separator
                            + pathProvider.getHDFFileName(pluginName,
                                    persistable);
                    List<PersistableDataObject> list = pdoMap.get(path);
                    if (list == null) {
                        list = new ArrayList<PersistableDataObject>(pdos.size());
                        pdoMap.put(path, list);
                    }
                    list.add(pdo);
                }
            } else {
                // order files by refTime hours
                for (PersistableDataObject pdo : pdos) {
                    String timeString = null;
                    if (pdo instanceof PluginDataObject) {
                        PluginDataObject pluginDataObj = (PluginDataObject) pdo;
                        Date time = pluginDataObj.getDataTime()
                                .getRefTimeAsCalendar().getTime();

                        synchronized (DefaultPathProvider.fileNameFormat) {
                            timeString = DefaultPathProvider.fileNameFormat
                                    .format(time);
                        }
                    } else {
                        // no refTime to use bounded insert query bounds
                        Date time = insertStartTime.getTime();

                        synchronized (DefaultPathProvider.fileNameFormat) {
                            timeString = DefaultPathProvider.fileNameFormat
                                    .format(time);
                        }
                    }

                    String path = pluginName + timeString;
                    List<PersistableDataObject> list = pdoMap.get(path);
                    if (list == null) {
                        list = new ArrayList<PersistableDataObject>(pdos.size());
                        pdoMap.put(path, list);
                    }
                    list.add(pdo);
                }

            }

            for (Map.Entry<String, List<PersistableDataObject>> entry : pdoMap
                    .entrySet()) {
                String path = archivePath + File.separator + pluginName
                        + File.separator + entry.getKey();

                // remove .h5
                int index = path.lastIndexOf('.');
                if (index > 0 && path.length() - index < 5) {
                    // ensure its end of string in case extension is
                    // dropped/changed
                    path = path.substring(0, index);
                }

                path += ".bin.gz";

                File file = new File(path);

                if (file.exists()) {
                    // pull the
                }

                // Thrift serialize pdo list
                byte[] data = SerializationUtil.transformToThrift(entry
                        .getValue());

                // debug transform back for object inspection
                Object obj = SerializationUtil.transformFromThrift(data);

                // save list to disk (in gz format?)
                FileUtil.bytes2File(data, file, true);
            }
        }

    }

    @SuppressWarnings("unchecked")
    public List<PersistableDataObject> getRecordsToArchive(
            Calendar insertStartTime, Calendar insertEndTime)
            throws DataAccessLayerException {
        DatabaseQuery dbQuery = new DatabaseQuery(this.getDaoClass());
        dbQuery.addQueryParam("insertTime", insertStartTime,
                QueryOperand.GREATERTHANEQUALS);
        dbQuery.addQueryParam("insertTime", insertEndTime,
                QueryOperand.LESSTHAN);
        dbQuery.addOrder("dataTime.refTime", true);

        return (List<PersistableDataObject>) this.queryByCriteria(dbQuery);
    }
}
