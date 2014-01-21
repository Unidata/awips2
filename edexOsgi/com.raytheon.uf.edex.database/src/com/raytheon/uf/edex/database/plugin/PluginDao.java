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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.beanutils.PropertyUtils;
import org.hibernate.Criteria;
import org.hibernate.QueryException;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.exception.ConstraintViolationException;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
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
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.processor.IDatabaseProcessor;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.database.purge.PurgeRule;
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
 * Feb 06, 2009 1990       bphillip    Initial creation
 * Jun 29, 2012 828        dgilling    Force getPurgeRulesForPlugin()  to search
 *                                     only COMMON_STATIC.
 * Oct 10, 2012 1261       djohnson    Add some generics wildcarding.
 * Jan 14, 2013 1469       bkowal      No longer retrieves the hdf5 data
 *                                     directory  from the environment.
 * Feb 12, 2013 1608       randerso    Changed to call deleteDatasets
 * Feb 26, 2013 1638       mschenke    Moved OGC specific functions to OGC
 *                                     project
 * Mar 27, 2013 1821       bsteffen    Remove extra store in persistToHDF5 for
 *                                     replace only operations.
 * Apr 04, 2013            djohnson    Remove formerly removed methods that
 *                                     won't compile.
 * Apr 15, 2013 1868       bsteffen    Rewrite mergeAll in PluginDao.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * May 16, 2013 1869       bsteffen    Rewrite dataURI property mappings.
 * Jun 11, 2013 2090       djohnson    Separate the hdf5 purge by ref time for
 *                                     reuse.
 * Jun 11, 2013 2092       bclement    Added purge results
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Sept23, 2013 2399       dhladky     Changed logging of duplicate records.
 * Oct 07, 2013 2392       rjpeter     Updated to pass null productKeys as actual null instead of string null.
 * Dec 13, 2013 2555       rjpeter     Refactored archiving logic into processArchiveRecords.
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

    // should match batch size in hibernate config
    protected static final int COMMIT_INTERVAL = 100;

    protected static final ConcurrentMap<Class<?>, DuplicateCheckStat> pluginDupCheckRate = new ConcurrentHashMap<Class<?>, DuplicateCheckStat>();

    // Map for tracking which PDOs store dataURI as a column in the DB.
    protected static final ConcurrentMap<Class<?>, Boolean> pluginDataURIColumn = new ConcurrentHashMap<Class<?>, Boolean>();

    /** The base path of the folder containing HDF5 data for the owning plugin */
    public final String PLUGIN_HDF5_DIR;

    /** The path provider for determining paths to hdf5 data */
    public IHDFFilePathProvider pathProvider;

    /** The owning plugin name */
    protected String pluginName;

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
        PLUGIN_HDF5_DIR = pluginName + File.separator;
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
        List<PluginDataObject> toPersist = new ArrayList<PluginDataObject>();
        for (PluginDataObject record : records) {
            toPersist.add(record);
        }
        List<PluginDataObject> duplicates = mergeAll(toPersist);
        toPersist.removeAll(duplicates);
        
        if (!duplicates.isEmpty()) {
            logger.info("Discarded : " + duplicates.size() + " duplicates!");
            if (logger.isDebugEnabled()) {
                for (PluginDataObject pdo : duplicates) {
                    logger.debug("Discarding duplicate: " + ((pdo)).getDataURI());
                }
            }
        }
        return toPersist.toArray(new PluginDataObject[toPersist.size()]);
    }

    /**
     * Commits(merges) a list of pdos into the database. Duplicates will not be
     * committed unless the object allows override.
     * 
     * @param objects
     *            the objects to commit
     * @return any duplicate objects which already existed in the db.
     */
    public List<PluginDataObject> mergeAll(final List<PluginDataObject> objects) {
        if ((objects == null) || objects.isEmpty()) {
            return objects;
        }
        List<PluginDataObject> duplicates = new ArrayList<PluginDataObject>();
        Class<? extends PluginDataObject> pdoClass = objects.get(0).getClass();
        DuplicateCheckStat dupStat = pluginDupCheckRate.get(pdoClass);
        if (dupStat == null) {
            dupStat = new DuplicateCheckStat();
            pluginDupCheckRate.put(pdoClass, dupStat);
        }
        boolean duplicateCheck = dupStat.isDuplicateCheck();
        int dupCommitCount = 0;
        int noDupCommitCount = 0;

        Session session = null;
        try {
            session = getHibernateTemplate().getSessionFactory().openSession();
            // process them all in fixed sized batches.
            for (int i = 0; i < objects.size(); i += COMMIT_INTERVAL) {
                List<PluginDataObject> subList = objects.subList(i,
                        Math.min(i + COMMIT_INTERVAL, objects.size()));
                List<PluginDataObject> subDuplicates = new ArrayList<PluginDataObject>();
                boolean constraintViolation = false;
                Transaction tx = null;
                if (!duplicateCheck) {
                    // First attempt is to just shove everything in the database
                    // as fast as possible and assume no duplicates.
                    try {
                        tx = session.beginTransaction();
                        for (PluginDataObject object : subList) {
                            if (object == null) {
                                continue;
                            }
                            session.save(object);
                        }
                        tx.commit();
                    } catch (ConstraintViolationException e) {
                        tx.rollback();
                        session.clear();
                        constraintViolation = true;
                    }
                }
                if (constraintViolation || duplicateCheck) {
                    // Second attempt will do duplicate checking, and possibly
                    // overwrite.
                    constraintViolation = false;
                    try {
                        tx = session.beginTransaction();
                        for (PluginDataObject object : subList) {
                            if (object == null) {
                                continue;
                            }
                            try {
                                Criteria criteria = session
                                        .createCriteria(pdoClass);
                                populateDatauriCriteria(criteria, object);
                                criteria.setProjection(Projections.id());
                                Integer id = (Integer) criteria.uniqueResult();
                                if (id != null) {
                                    object.setId(id);
                                    if (object.isOverwriteAllowed()) {
                                        session.update(object);
                                    } else {
                                        subDuplicates.add(object);
                                    }
                                } else {
                                    session.save(object);
                                }
                            } catch (PluginException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        "Query failed: Unable to insert or update "
                                                + object.getIdentifier(), e);
                            }
                        }
                        tx.commit();
                    } catch (ConstraintViolationException e) {
                        constraintViolation = true;
                        tx.rollback();
                        session.clear();
                    }
                }
                if (constraintViolation) {
                    // Third attempt will commit each pdo individually.
                    subDuplicates.clear();
                    for (PluginDataObject object : subList) {
                        if (object == null) {
                            continue;
                        }
                        try {
                            tx = session.beginTransaction();
                            Criteria criteria = session
                                    .createCriteria(pdoClass);
                            populateDatauriCriteria(criteria, object);
                            criteria.setProjection(Projections.id());
                            Integer id = (Integer) criteria.uniqueResult();
                            if (id != null) {
                                object.setId(id);
                                if (object.isOverwriteAllowed()) {
                                    session.update(object);
                                } else {
                                    subDuplicates.add(object);
                                }
                            } else {
                                session.save(object);
                            }
                            tx.commit();
                        } catch (ConstraintViolationException e) {
                            subDuplicates.add(object);
                            tx.rollback();
                        } catch (PluginException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Query failed: Unable to insert or update "
                                            + object.getIdentifier(), e);
                        }
                    }
                }
                if (subDuplicates.isEmpty()) {
                    noDupCommitCount += 1;
                } else {
                    dupCommitCount += 1;
                    duplicates.addAll(subDuplicates);
                }
            }
            dupStat.updateRate(noDupCommitCount
                    / (noDupCommitCount + dupCommitCount));
        } finally {
            if (session != null) {
                session.close();
            }
        }
        return duplicates;
    }

    private void populateDatauriCriteria(Criteria criteria, PluginDataObject pdo)
            throws PluginException {
        Class<? extends PluginDataObject> pdoClazz = pdo.getClass();
        Boolean hasDataURIColumn = pluginDataURIColumn.get(pdoClazz);
        if (!Boolean.FALSE.equals(hasDataURIColumn)) {
            try {
                getSessionFactory().getClassMetadata(pdoClazz).getPropertyType(
                        "dataURI");
                criteria.add(Restrictions.eq("dataURI", pdo.getDataURI()));
                return;
            } catch (QueryException e) {
                hasDataURIColumn = Boolean.FALSE;
                pluginDataURIColumn.put(pdoClazz, hasDataURIColumn);
            }
        }
        // This means dataURI is not a column.
        for (Entry<String, Object> uriEntry : DataURIUtil.createDataURIMap(pdo)
                .entrySet()) {
            String key = uriEntry.getKey();
            if (key.equals("pluginName")) {
                ;// this is not in the db, only used internally.
                continue;
            }
            Object value = uriEntry.getValue();
            int dotIndex = key.indexOf(".");
            if (dotIndex > 0) {
                key = key.substring(0, dotIndex);
                try {
                    value = PropertyUtils.getProperty(pdo, key);
                } catch (Exception e) {
                    throw new PluginException(e);
                }
            }
            if (value == null) {
                criteria.add(Restrictions.isNull(key));
            } else {
                criteria.add(Restrictions.eq(key, value));
            }
        }
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
                String directory = pdo.getPluginName()
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

            IDataStore dataStore = null;
            IDataStore replaceDataStore = null;

            for (IPersistable persistable : persistables) {
                try {
                    if (((PersistableDataObject<?>) persistable)
                            .isOverwriteAllowed()) {
                        if (replaceDataStore == null) {
                            replaceDataStore = DataStoreFactory
                                    .getDataStore(file);
                        }

                        populateDataStore(replaceDataStore, persistable);
                    } else {
                        if (dataStore == null) {
                            dataStore = DataStoreFactory.getDataStore(file);
                        }
                        populateDataStore(dataStore, persistable);
                    }
                } catch (Exception e) {
                    throw new PluginException("Error populating data store", e);
                }
            }

            if (dataStore != null) {
                try {
                    StorageStatus s = dataStore.store();
                    // add exceptions to a list for aggregation
                    exceptions.addAll(Arrays.asList(s.getExceptions()));
                } catch (StorageException e) {
                    logger.error("Error persisting to HDF5", e);
                }
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
     *            The tile set to retrieve. Any value less than or equal to zero
     *            returns the "base" data only.
     * @return The HDF5 data records
     * @throws StorageException
     *             If problems occur while interacting with HDF5 data stores
     */
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {

        List<IDataRecord[]> retVal = new ArrayList<IDataRecord[]>();

        for (PluginDataObject obj : objects) {

            if (obj instanceof IPersistable) {
                /* connect to the data store and retrieve the data */
                IDataStore dataStore = getDataStore((IPersistable) obj);
                boolean interpolated = DataStoreFactory.isInterpolated(tileSet);
                if (!interpolated) {
                    tileSet = 0;
                }
                IDataRecord[] record = new IDataRecord[tileSet + 1];
                try {
                    String group = DataStoreFactory.createGroupName(
                            obj.getDataURI(),
                            DataStoreFactory.DEF_DATASET_NAME, interpolated);
                    // Retrieve the base record.
                    record[0] = dataStore.retrieve(obj.getDataURI(),
                            DataStoreFactory.DEF_DATASET_NAME, Request.ALL);
                    // Now get the interpolated data, if any!
                    for (int tile = 1; tile < record.length; tile++) {
                        record[tile] = dataStore.retrieve(group,
                                String.valueOf(tile), Request.ALL);
                    }
                } catch (Exception e) {
                    throw new PluginException("Error getting HDF5 data", e);
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
        purgeAllData(null);
    }

    /**
     * Purges all data associated with the productKeys and owning plugin
     * 
     * @throws PluginException
     *             If problems occur while interacting with the data stores
     */
    public void purgeAllData(Map<String, String> productsKeys)
            throws PluginException {
        boolean purgeHdf5Data = false;
        try {
            // Determine if this plugin uses HDF5 to store data
            purgeHdf5Data = (PluginFactory.getInstance()
                    .getPluginRecordClass(pluginName).newInstance() instanceof IPersistable);
        } catch (Exception e) {
            PurgeLogger.logError(
                    "Unabled to determine if plugin has HDF5 data to purge",
                    this.pluginName, e);
        }

        try {
            List<Date> allRefTimes = getRefTimes();
            Map<String, List<String>> filesToDelete = new HashMap<String, List<String>>();
            for (Date d : allRefTimes) {
                this.purgeDataByRefTime(d, productsKeys, purgeHdf5Data, false,
                        filesToDelete);
            }
            if (purgeHdf5Data) {
                for (String file : filesToDelete.keySet()) {
                    try {
                        IDataStore ds = DataStoreFactory.getDataStore(new File(
                                file));
                        ds.deleteFiles(null);
                    } catch (Exception e) {
                        PurgeLogger.logError("Error occurred purging file: "
                                + file, this.pluginName, e);
                    }
                }
            }
        } catch (Exception e) {
            throw new PluginException("Error purging all data for "
                    + pluginName + " plugin.", e);
        }
    }

    /**
     * Result of purge for rule
     */
    protected static class RuleResult {
        public Set<Date> timesKept;

        public Set<Date> timesPurged;

        public int itemsDeletedForKey;

        public RuleResult(Set<Date> timesKept, Set<Date> timesPurged,
                int itemsDeletedForKey) {
            this.timesKept = timesKept;
            this.timesPurged = timesPurged;
            this.itemsDeletedForKey = itemsDeletedForKey;
        }

    }

    /**
     * Purges data according to purge criteria specified by the owning plugin
     * 
     * @throws PluginException
     *             If problems occur while interacting with data stores
     */
    public void purgeExpiredData() throws PluginException {
        purgeExpiredDataWithResults();
    }

    /**
     * Purges data according to purge criteria specified by the owning plugin
     * 
     * @throws PluginException
     *             If problems occur while interacting with data stores
     */
    public PurgeResults purgeExpiredDataWithResults() throws PluginException {
        try {
            PurgeRuleSet ruleSet = getPurgeRulesForPlugin(pluginName);
            Map<String, Set<Date>> timesKept = new HashMap<String, Set<Date>>();
            Map<String, Set<Date>> timesPurged = new HashMap<String, Set<Date>>();
            if (ruleSet == null) {
                PurgeLogger.logInfo(
                        "No valid purge rules found. Skipping purge.",
                        pluginName);
                return new PurgeResults(timesKept, timesPurged);
            }

            // Query the database to get all possible product keys for this data
            List<String> ruleKeys = ruleSet.getKeys();
            int totalItems = 0;

            if ((ruleKeys != null) && !ruleKeys.isEmpty()) {
                // Iterate through keys, fully purge each key set
                String[][] distinctKeys = getDistinctProductKeyValues(ruleSet
                        .getKeys());
                for (String[] key : distinctKeys) {
                    RuleResult res = purgeExpiredKey(ruleSet, key);
                    timesKept.put(Arrays.toString(key), res.timesKept);
                    timesPurged.put(Arrays.toString(key), res.timesPurged);
                    totalItems += res.itemsDeletedForKey;
                }
            } else {
                // no rule keys defined, can only apply default rule
                RuleResult res = purgeExpiredKey(ruleSet, null);
                timesKept.put("default", res.timesKept);
                timesPurged.put("default", res.timesPurged);
                totalItems += res.itemsDeletedForKey;
            }

            StringBuilder messageBuffer = new StringBuilder();
            messageBuffer.append("Purged ").append(totalItems).append(" item");
            if (totalItems != 1) {
                messageBuffer.append("s");
            }
            messageBuffer.append(" total.");

            PurgeLogger.logInfo(messageBuffer.toString(), pluginName);
            return new PurgeResults(timesKept, timesPurged);
        } catch (EdexException e) {
            throw new PluginException("Error applying purge rule!!", e);
        }
    }

    /**
     * Takes the purgeKeys, looks up the associated purge rule, and applies it
     * to the data matched by purgeKeys.
     * 
     * @param ruleSet
     * @param purgeKeys
     * @return Summary of purge for keys
     * @throws DataAccessLayerException
     */
    protected RuleResult purgeExpiredKey(PurgeRuleSet ruleSet,
            String[] purgeKeys) throws DataAccessLayerException {
        List<PurgeRule> rules = ruleSet.getRuleForKeys(purgeKeys);

        if (rules == null) {
            PurgeLogger.logWarn(
                    "No rules found for purgeKeys: "
                            + Arrays.toString(purgeKeys), pluginName);
            return new RuleResult(Collections.<Date> emptySet(),
                    Collections.<Date> emptySet(), 0);
        }

        /*
         * This section applies the purge rule
         */
        Map<String, String> productKeys = null;
        if (purgeKeys != null) {
            productKeys = new LinkedHashMap<String, String>(purgeKeys.length);
            Iterator<String> iter = ruleSet.getKeys().iterator();
            for (String purgeKey : purgeKeys) {
                productKeys.put(iter.next(), purgeKey);
            }
        }

        List<Date> refTimesForKey = getRefTimesForCriteria(productKeys);
        String productKeyString = null;
        if (productKeys != null) {
            StringBuilder productKeyBuilder = new StringBuilder();
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                productKeyBuilder.append('[').append(pair.getKey()).append('=')
                        .append(pair.getValue()).append(']');
            }
            productKeyString = productKeyBuilder.toString();
        }

        Set<Date> timesKept = new HashSet<Date>();
        Set<Date> timesPurged = new HashSet<Date>();

        for (PurgeRule rule : rules) {
            // Holds the times kept by this rule
            List<Date> timesKeptByRule = new ArrayList<Date>();

            Set<Date> roundedTimes = new HashSet<Date>();

            // Holds the times to be purged by this rule
            List<Date> timesPurgedByRule = new ArrayList<Date>();

            if (rule.isModTimeToWaitSpecified()) {
                Date maxInsertTime = getMaxInsertTime(productKeys);
                if (maxInsertTime != null) {
                    long lastInsertTime = maxInsertTime.getTime();
                    long currentTime = System.currentTimeMillis();
                    if ((currentTime - lastInsertTime) < rule
                            .getModTimeToWaitInMillis()) {
                        PurgeLogger
                                .logInfo(
                                        "For procuct key, "
                                                + productKeyString
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
                    Date maxRefTime = getMaxRefTime(productKeys);
                    if (maxRefTime == null) {
                        PurgeLogger.logInfo("No data available to purge",
                                pluginName);
                        return new RuleResult(Collections.<Date> emptySet(),
                                Collections.<Date> emptySet(), 0);
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
                        if ((dateTimeAsLong % delta) == 0) {
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
             * keep. If a delta is specified for this rule, then the versions
             * have already been calculated based on the delta time. This
             * section is used only if a delta time is not used
             */
            else if (!rule.isDeltaSpecified()
                    && rule.isVersionsToKeepSpecified()) {
                Date currentRefTime = null;
                for (int i = 0; i < refTimesForKey.size(); i++) {
                    currentRefTime = refTimesForKey.get(i);
                    if (i < rule.getVersionsToKeep()) {
                        // allow for period to override versions to keep
                        if (rule.isPeriodSpecified()
                                && currentRefTime.before(periodCutoffTime)) {
                            timesPurgedByRule.add(currentRefTime);
                        } else {
                            timesKeptByRule.add(currentRefTime);
                        }
                    } else {
                        timesPurgedByRule.add(currentRefTime);
                    }
                }
                /*
                 * This rule only specifies a time cutoff
                 */
            } else if (!rule.isDeltaSpecified()
                    && !rule.isVersionsToKeepSpecified()
                    && rule.isPeriodSpecified()) {
                for (Date currentRefTime : refTimesForKey) {
                    if (currentRefTime.before(periodCutoffTime)) {
                        timesPurgedByRule.add(currentRefTime);
                    } else {
                        timesKeptByRule.add(currentRefTime);
                    }
                }
                /*
                 * This rule has been so poorly written that it does nothing
                 */
            } else {
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
                PurgeLogger.logInfo(
                        "These versions would have been retained by the rule:",
                        pluginName);
                for (Date d : timesKeptByRule) {
                    PurgeLogger.logInfo(d.toString(), pluginName);
                }
            } else {
                timesKept.addAll(timesKeptByRule);
                timesPurged.addAll(timesPurgedByRule);
            }
        }

        // We must remove the keep times from the purge list. This
        // ensures that if the time passes at least one time constraint,
        // then it will be retained
        timesPurged.removeAll(timesKept);

        // flags to control how hdf5 is purged and what needs to be returned
        // from the database purge to properly purge hdf5. If purging and
        // trackToUri is false, hdf5PurgeDates is used to determine if the
        // underlying hdf5 data can be kept. This is optimized based on data
        // being stored in hourly chunks.
        // TODO: Update to allow files to not be in hourly granularity
        boolean purgeHdf5Data = false;
        boolean trackToUri = false;

        try {
            Set<Date> roundedTimesKept = new HashSet<Date>();

            // Determine if this plugin uses HDF5 to store data
            purgeHdf5Data = (PluginFactory.getInstance()
                    .getPluginRecordClass(pluginName).newInstance() instanceof IPersistable);

            // determine if hdf5 purge can be optimized
            if (purgeHdf5Data) {
                // check how the path keys line up to purge keys
                List<String> pathKeys = pathProvider
                        .getKeyNames(this.pluginName);
                boolean pathKeysEmpty = (pathKeys == null)
                        || pathKeys.isEmpty();
                boolean productKeysEmpty = (productKeys == null)
                        || (productKeys.isEmpty());

                // determine if hdf5 purge can be optimized
                if (!pathKeysEmpty) {
                    if (productKeysEmpty) {
                        // Purging on higher magnitude that path, only need to
                        // track file
                        trackToUri = false;
                    } else if (pathKeys.size() < productKeys.size()) {
                        // there are more purge keys than path keys, cannot
                        // optimize hdf5 purge
                        trackToUri = true;
                    } else {
                        // need to compare each key to check for optimized
                        // purge, all productKeys must be a pathKey for
                        // optimized purge, both key lists should be small 3 or
                        // less, no need to optimize list look ups
                        trackToUri = false;
                        for (String productKey : productKeys.keySet()) {
                            if (!pathKeys.contains(productKey)) {
                                trackToUri = true;
                                break;
                            }
                        }
                    }
                } else {
                    // if purge is same level as path, optimize
                    trackToUri = !productKeysEmpty;
                }

                // we can optimize purge, sort dates by hour to determine files
                // to drop, also don't remove from metadata if we are keeping
                // the hdf5 around
                if (!trackToUri) {
                    for (Date dateToRound : timesKept) {
                        roundedTimesKept.add(roundDateToHour(dateToRound));
                    }

                    Iterator<Date> purgeTimeIterator = timesPurged.iterator();

                    while (purgeTimeIterator.hasNext()) {
                        Date purgeTime = purgeTimeIterator.next();

                        // keeping this hdf5 file, remove the purge time
                        if (roundedTimesKept
                                .contains(roundDateToHour(purgeTime))) {
                            purgeTimeIterator.remove();
                        }
                    }
                }
            }
        } catch (Exception e) {
            PurgeLogger.logError(
                    "Unabled to determine if plugin has HDF5 data to purge",
                    this.pluginName, e);
        }

        int itemsDeletedForKey = 0;
        List<Date> orderedTimesPurged = new ArrayList<Date>(timesPurged);
        Collections.sort(orderedTimesPurged);

        Map<String, List<String>> hdf5FileToUriMap = new HashMap<String, List<String>>();
        Date previousRoundedDate = null;
        for (Date deleteDate : orderedTimesPurged) {
            Date roundedDate = roundDateToHour(deleteDate);

            // Delete the data in the database
            int itemsDeletedForTime = purgeDataByRefTime(deleteDate,
                    productKeys, purgeHdf5Data, trackToUri, hdf5FileToUriMap);

            itemsDeletedForKey += itemsDeletedForTime;

            // check if any hdf5 data up to this point can be deleted
            if (purgeHdf5Data
                    && (trackToUri || ((previousRoundedDate != null) && roundedDate
                            .after(previousRoundedDate)))) {
                // delete these entries now
                for (Map.Entry<String, List<String>> hdf5Entry : hdf5FileToUriMap
                        .entrySet()) {
                    try {
                        IDataStore ds = DataStoreFactory.getDataStore(new File(
                                hdf5Entry.getKey()));
                        List<String> uris = hdf5Entry.getValue();
                        if (uris == null) {
                            ds.deleteFiles(null);
                        } else {
                            ds.deleteDatasets(uris.toArray(new String[uris
                                    .size()]));
                        }
                    } catch (Exception e) {
                        PurgeLogger.logError("Error occurred purging file: "
                                + hdf5Entry.getKey(), this.pluginName, e);
                    }
                }
                hdf5FileToUriMap.clear();
            }
        }

        if (purgeHdf5Data) {
            // delete any remaining data
            for (Map.Entry<String, List<String>> hdf5Entry : hdf5FileToUriMap
                    .entrySet()) {
                try {
                    IDataStore ds = DataStoreFactory.getDataStore(new File(
                            hdf5Entry.getKey()));
                    List<String> uris = hdf5Entry.getValue();
                    if (uris == null) {
                        ds.deleteFiles(null);
                    } else {
                        ds.deleteDatasets(uris.toArray(new String[uris.size()]));
                    }
                } catch (Exception e) {
                    PurgeLogger.logError("Error occurred purging file: "
                            + hdf5Entry.getKey(), this.pluginName, e);
                }
            }
        }

        if (itemsDeletedForKey > 0) {
            StringBuilder messageBuffer = new StringBuilder();
            messageBuffer.append("Purged ").append(itemsDeletedForKey)
                    .append(" item");
            if (itemsDeletedForKey != 1) {
                messageBuffer.append("s");
            }
            messageBuffer.append(" for key ").append(productKeyString);
            PurgeLogger.logInfo(messageBuffer.toString(), pluginName);
        }

        // Debug output to see which times were retained
        if (PurgeLogger.isDebugEnabled()) {
            if (!timesPurged.isEmpty()) {
                StringBuilder builder = new StringBuilder();
                List<Date> orderedTimesKept = new ArrayList<Date>(timesKept);
                Collections.sort(orderedTimesPurged);
                Collections.sort(orderedTimesKept);
                builder.append("The following times were retained");
                builder.append(" for key ").append(productKeyString)
                        .append(":");

                for (Date keepDate : orderedTimesKept) {
                    builder.append("[").append(keepDate).append("]")
                            .append(" ");
                }
                PurgeLogger.logDebug(builder.toString(), pluginName);
            }
        }

        return new RuleResult(timesKept, timesPurged, itemsDeletedForKey);
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
     * Takes a list of IPersistable objects and return a map of IDataStore
     * objects and a list of IPersistable objects that are stored in that data
     * store.
     * 
     * @param objs
     *            A list of IPersistable objects to get their respsective data
     *            stores.
     * @return
     */
    public Map<IDataStore, List<IPersistable>> getDataStoreMap(
            List<IPersistable> objs) {
        StringBuilder tmp = new StringBuilder(120);

        Map<String, List<IPersistable>> fileMap = new HashMap<String, List<IPersistable>>();

        // group objects by file
        for (IPersistable obj : objs) {
            tmp.setLength(0);
            tmp.append(pathProvider.getHDFPath(this.pluginName, obj));
            tmp.append(File.separatorChar);
            tmp.append(pathProvider.getHDFFileName(this.pluginName, obj));
            String path = tmp.toString();
            List<IPersistable> objsInFile = fileMap.get(path);
            if (objsInFile == null) {
                objsInFile = new ArrayList<IPersistable>();
                fileMap.put(path, objsInFile);
            }
            objsInFile.add(obj);
        }

        Map<IDataStore, List<IPersistable>> dataStoreMap = new HashMap<IDataStore, List<IPersistable>>(
                (int) (fileMap.size() * 1.25) + 1);
        for (Map.Entry<String, List<IPersistable>> entry : fileMap.entrySet()) {
            dataStoreMap.put(
                    DataStoreFactory.getDataStore(new File(PLUGIN_HDF5_DIR
                            + entry.getKey())), entry.getValue());
        }

        return dataStoreMap;
    }

    /**
     * Gets a list of the distinct product key values for this plugin.
     * 
     * @param the
     *            keys to look up values for.
     * @return 2 dimensional array of distinct values for the given keys. First
     *         dimension is the row of data, second dimension actual values.
     * @throws DataAccessLayerException
     *             If errors occur while querying for the data
     */
    @SuppressWarnings("unchecked")
    public String[][] getDistinctProductKeyValues(List<String> keys)
            throws DataAccessLayerException {
        String[][] distinctValues = null;
        if ((keys != null) && !keys.isEmpty()) {
            DatabaseQuery query = new DatabaseQuery(this.daoClass);
            for (int i = 0; i < keys.size(); i++) {
                if (i == 0) {
                    query.addDistinctParameter(keys.get(i));
                } else {
                    query.addReturnedField(keys.get(i));
                }
            }
            if (keys.size() == 1) {
                List<?> results = this.queryByCriteria(query);
                distinctValues = new String[results.size()][];
                int index = 0;
                for (Object obj : results) {
                    distinctValues[index] = new String[1];
                    if (obj != null) {
                        distinctValues[index++][0] = String.valueOf(obj);
                    } else {
                        distinctValues[index++][0] = null;
                    }
                }
            } else {
                List<Object[]> results = (List<Object[]>) this
                        .queryByCriteria(query);
                distinctValues = new String[results.size()][];
                int rIndex = 0;

                for (Object[] result : results) {
                    distinctValues[rIndex] = new String[result.length];
                    int cIndex = 0;

                    for (Object obj : result) {
                        if (obj != null) {
                            distinctValues[rIndex][cIndex++] = String
                                    .valueOf(obj);
                        } else {
                            distinctValues[rIndex][cIndex++] = null;
                        }
                    }

                    rIndex++;
                }
            }
        }
        return distinctValues;
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
     * @param productKeys
     *            The product key/values to get the list of reference times for.
     *            Should be in key value pairs.
     * @return A list of distinct reference times for the given productKey
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<Date> getRefTimesForCriteria(Map<String, String> productKeys)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);

        if ((productKeys != null) && (productKeys.size() > 0)) {
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                query.addQueryParam(pair.getKey(), pair.getValue());
            }
        }

        query.addDistinctParameter(PURGE_VERSION_FIELD);
        query.addOrder(PURGE_VERSION_FIELD, false);
        List<Date> times = (List<Date>) this.queryByCriteria(query);
        return times;
    }

    /**
     * Purges data from the database for this plugin with the given reference
     * time matching the given productKeys. If refTime is null, will purge all
     * data associated with the productKeys. Hdf5 must be purged separately as
     * most hdf5 files can't be purged with a single reference time. Use the
     * passed map to track what needs to be done with hdf5.
     * 
     * @param refTime
     *            The reftime to delete data for. A null will purge all data for
     *            the productKeys.
     * @param productKeys
     *            The product key/values to use as a constraint for deletions.
     *            Should be in key value pairs.
     * @param trackHdf5
     *            If true will use trackToUri to populate hdf5FileToUriPurged
     *            map.
     * @param trackToUri
     *            If true will track each URI that needs to be deleted from
     *            HDF5, if false will only track the hdf5 files that need to be
     *            deleted.
     * @param hdf5FileToUriPurged
     *            Map to be populated by purgeDataByRefTime of all the hdf5
     *            files that need to be updated. If trackToUri is true, each
     *            file will have the exact data URI's to be removed from each
     *            file. If trackToUri is false, the map will have a null entry
     *            for the list and only track the files.
     * @return Number of rows deleted from database.
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public int purgeDataByRefTime(Date refTime,
            Map<String, String> productKeys, boolean trackHdf5,
            boolean trackToUri, Map<String, List<String>> hdf5FileToUriPurged)
            throws DataAccessLayerException {

        int results = 0;

        DatabaseQuery dataQuery = new DatabaseQuery(this.daoClass);

        if (refTime != null) {
            dataQuery.addQueryParam(PURGE_VERSION_FIELD, refTime);
        }

        if ((productKeys != null) && (productKeys.size() > 0)) {
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                dataQuery.addQueryParam(pair.getKey(), pair.getValue());
            }
        }

        List<PluginDataObject> pdos = null;

        dataQuery.setMaxResults(500);

        do {
            pdos = (List<PluginDataObject>) this.queryByCriteria(dataQuery);
            if ((pdos != null) && !pdos.isEmpty()) {
                this.delete(pdos);

                if (trackHdf5 && (hdf5FileToUriPurged != null)) {
                    purgeHdf5ForPdos(trackToUri, hdf5FileToUriPurged, pdos);
                }

                results += pdos.size();
            }

        } while ((pdos != null) && !pdos.isEmpty());

        return results;
    }

    /**
     * Purge HDF5 data for a list of PDOs. Extracted as is from
     * {@link #purgeDataByRefTime} so it can be reused.
     * 
     * @param trackToUri
     *            If true will track each URI that needs to be deleted from
     *            HDF5, if false will only track the hdf5 files that need to be
     *            deleted.
     * @param hdf5FileToUriPurged
     *            Map to be populated by purgeDataByRefTime of all the hdf5
     *            files that need to be updated. If trackToUri is true, each
     *            file will have the exact data URI's to be removed from each
     *            file. If trackToUri is false, the map will have a null entry
     *            for the list and only track the files.
     * @param pdos
     *            the pdos
     */
    protected void purgeHdf5ForPdos(boolean trackToUri,
            Map<String, List<String>> hdf5FileToUriPurged,
            List<PluginDataObject> pdos) {
        // fields for hdf5 purge
        String previousFile = null;
        for (PluginDataObject pdo : pdos) {
            StringBuilder pathBuilder = new StringBuilder();
            IPersistable persist = (IPersistable) pdo;
            pathBuilder
                    .append(PLUGIN_HDF5_DIR)
                    .append(pathProvider.getHDFPath(this.pluginName, persist))
                    .append(File.separatorChar)
                    .append(pathProvider.getHDFFileName(this.pluginName,
                            persist));
            String file = pathBuilder.toString();

            if (trackToUri) {
                List<String> uriList = hdf5FileToUriPurged.get(file);
                if (uriList == null) {
                    // sizing to 50 as most data types have numerous
                    // entries in a file
                    uriList = new ArrayList<String>(50);
                    hdf5FileToUriPurged.put(file, uriList);
                }
                uriList.add(file);
            } else {
                // only need to track file, tracking last file
                // instead of constantly indexing hashMap
                if (!file.equals(previousFile)) {
                    hdf5FileToUriPurged.put(file, null);
                    previousFile = file;
                }
            }
        }
    }

    /**
     * Purges the HDF5 data according to the provided time and key.
     * 
     * @param refTime
     *            The time to delete
     * @param productKey
     *            The key to delete
     */
    protected void purgeHDF5DataByRefTime(Date refTime, String productKey)
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
     * @param productKeys
     *            The product keys to get the maximum reference time for. Should
     *            be in key value pairs.
     * @return Null if this key was not found, else the maximum reference time
     * @throws DataAccessLayerException
     *             If errors occur while querying the database
     */
    @SuppressWarnings("unchecked")
    public Date getMaxRefTime(Map<String, String> productKeys)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addDistinctParameter(PURGE_VERSION_FIELD);

        if ((productKeys != null) && (productKeys.size() > 0)) {
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                query.addQueryParam(pair.getKey(), pair.getValue());
            }
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
    public Date getMaxInsertTime(Map<String, String> productKeys)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        // doing distinct is wasted with a ordered max
        // query.addDistinctParameter("insertTime");
        if ((productKeys != null) && (productKeys.size() > 0)) {
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                query.addQueryParam(pair.getKey(), pair.getValue());
            }
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
     * Gets the minimum insert time contained in the database for the given
     * keys. The keys corresponds to the productKey fields in the data object.
     * 
     * @param productKeys
     *            The product keys to get the minimum insert time for. Should be
     *            in key value pairs.
     * @return Null if this key was not found, else the minimum insert time
     * @throws DataAccessLayerException
     *             If errors occur while querying the database
     */
    @SuppressWarnings("unchecked")
    public Date getMinInsertTime(Map<String, String> productKeys)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        // doing distinct is wasted with a ordered max
        // query.addDistinctParameter("insertTime");

        if ((productKeys != null) && (productKeys.size() > 0)) {
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                query.addQueryParam(pair.getKey(), pair.getValue());
            }
        }

        query.addReturnedField("insertTime");
        query.addOrder("insertTime", true);
        query.setMaxResults(1);
        List<Calendar> result = (List<Calendar>) this.queryByCriteria(query);
        if ((result == null) || result.isEmpty()) {
            return null;
        } else {
            return result.get(0).getTime();
        }
    }

    /**
     * Gets the minimum reference time contained in the database for the given
     * key. The key corresponds to the productKey field in the data object.
     * 
     * @param productKeys
     *            The product keys to get the minimum reference times for.
     *            Should be in key value pairs.
     * @return Null if this key was not found, else the minimum reference time
     * @throws DataAccessLayerException
     *             If errors occur while querying the database
     */
    @SuppressWarnings("unchecked")
    public Date getMinRefTime(Map<String, String> productKeys)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addDistinctParameter(PURGE_VERSION_FIELD);

        if ((productKeys != null) && (productKeys.size() > 0)) {
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                query.addQueryParam(pair.getKey(), pair.getValue());
            }
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
        for (String keyToken : keyTokens) {
            String[] constraintTokens = keyToken.split("=");
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

    private Date roundDateToHour(Date dateToRound) {
        return new Date(dateToRound.getTime()
                - (dateToRound.getTime() % 3600000));
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

    public static PurgeRuleSet getPurgeRulesForPlugin(String pluginName) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> tieredFile = pathMgr
                .getTieredLocalizationFile(LocalizationType.COMMON_STATIC,
                        "purge/" + pluginName + "PurgeRules.xml");
        LocalizationContext[] levelHierarchy = pathMgr
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);
        File rulesFile = null;
        for (LocalizationContext ctx : levelHierarchy) {
            LocalizationFile lFile = tieredFile.get(ctx.getLocalizationLevel());
            if (lFile != null) {
                rulesFile = lFile.getFile();
                break;
            }
        }

        if (rulesFile != null) {
            // allow zero length file to disable purge for this plugin
            if (rulesFile.length() > 0) {
                try {
                    PurgeRuleSet purgeRules = SerializationUtil
                            .jaxbUnmarshalFromXmlFile(PurgeRuleSet.class,
                                    rulesFile);

                    // ensure there's a default rule
                    if (purgeRules.getDefaultRules() == null) {
                        purgeRules.setDefaultRules(loadDefaultPurgeRules());
                    }
                    return purgeRules;
                } catch (SerializationException e) {
                    PurgeLogger
                            .logError(
                                    "Error deserializing purge rules! Data will not be purged. Please define rules.",
                                    pluginName, e);
                }
            }
        } else if (!"default".equals(pluginName)) {
            // no purge rule for this plugin, check base purge rule
            return getPurgeRulesForPlugin("default");
        }
        return null;
    }

    public static List<PurgeRule> loadDefaultPurgeRules() {
        File defaultRule = PathManagerFactory.getPathManager().getStaticFile(
                "purge/defaultPurgeRules.xml");
        if (defaultRule == null) {
            PurgeLogger
                    .logError(
                            "Default purge rule not defined!! Data will not be purged for plugins which do not specify purge rules!",
                            "EDEX");
            return null;
        }

        try {
            PurgeRuleSet purgeRules = SerializationUtil
                    .jaxbUnmarshalFromXmlFile(PurgeRuleSet.class, defaultRule);
            return purgeRules.getDefaultRules();
        } catch (SerializationException e) {
            PurgeLogger.logError("Error deserializing default purge rule!",
                    "DEFAULT");
        }

        return null;
    }

    public int processArchiveRecords(Calendar insertStartTime,
            Calendar insertEndTime, IDatabaseProcessor processor)
            throws DataAccessLayerException {
        DatabaseQuery dbQuery = new DatabaseQuery(this.getDaoClass());
        dbQuery.addQueryParam("insertTime", insertStartTime,
                QueryOperand.GREATERTHANEQUALS);
        dbQuery.addQueryParam("insertTime", insertEndTime,
                QueryOperand.LESSTHAN);
        dbQuery.addOrder("insertTime", true);
        dbQuery.addOrder("dataTime.refTime", true);

        return this.processByCriteria(dbQuery, processor);
    }

    protected static class DuplicateCheckStat {

        // percentage of commits that need to succeed without duplicate checking
        // for a plugin to attempt to skip duplicate checking.
        protected static final float DUPLICATE_CHECK_THRESHOLD = 0.5f;

        // This number controls the maximum number of transactions to
        // "remember" which will affect how difficult it is to change the
        // cumulative rate. The larger the number is, the more successful(or
        // failed) attempts it will take to change the cumulativeRate.
        protected static final int DUPLICATE_MEMORY = 5000;

        protected boolean duplicateCheck = false;

        protected float cumulativeRate = 1.0f;

        protected int total = 0;

        protected boolean isDuplicateCheck() {
            return duplicateCheck;
        }

        protected void updateRate(float rate) {
            cumulativeRate = (rate + (cumulativeRate * total)) / (total + 1);
            duplicateCheck = cumulativeRate < DUPLICATE_CHECK_THRESHOLD;

            if (total < DUPLICATE_MEMORY) {
                total++;
            }
        }
    }
}
