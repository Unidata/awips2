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

package com.raytheon.edex.plugin.gfe.db.dao;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Property;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.edex.plugin.gfe.config.GFESiteActivation;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.LockNotification;
import com.raytheon.uf.common.dataplugin.gfe.type.Pair;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Data access object for manipulating GFE Records
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 05/16/08     #875       bphillip    Added D2D grib querying methods
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 06/17/09     #2380      randerso    Removed purging of grid history.
 *                                     Should cascade when record deleted.
 * 08/07/09     #2763      njensen     Refactored queryByD2DParmId
 * 09/10/12     DR15137    ryu         Changed for MOSGuide D2D mxt/mnt grids for consistency
 *                                     with A1.
 * 10/10/12     #1260       randerso   Added check to ensure db can be created before 
 *                                     adding it to the inventory
 * 12/06/12     #1394      rjpeter     Optimized D2D grid access.
 * 01/21/12     #1504      randerso    Back ported change to use ParameterMapper into 13.1.2
 * 02/10/13     #1603      randerso    Eliminated unnecessary conversion from lists to arrays
 * 02/12/13     #1608      randerso    Changed to use explicit deletes for groups and datasets
 * 03/15/13     #1795      njensen     Added updatePublishTime()
 * 03/21/13     #1774      randerso    Moved D2D routines into {@link com.raytheon.edex.plugin.gfe.db.dao.GFED2DDao}
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GFEDao extends DefaultPluginDao {
    private static final Pattern WIND_PATTERN = Pattern.compile("wind");

    public GFEDao() throws PluginException {
        super("gfe");
    }

    /**
     * Creates a new GFE Dao
     * 
     * @throws PluginException
     */
    public GFEDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public void purgeExpiredData() throws PluginException {
        Set<String> sites = GFESiteActivation.getInstance().getActiveSites();
        for (String siteID : sites) {
            List<GridUpdateNotification> gridNotifcations = new ArrayList<GridUpdateNotification>();
            List<LockNotification> lockNotifications = new ArrayList<LockNotification>();

            try {
                GridParmManager.versionPurge(siteID);
                GridParmManager.gridsPurge(gridNotifcations, lockNotifications,
                        siteID);
                PurgeLogger.logInfo(
                        "Purging Expired pending isc send requests...", "gfe");
                int requestsPurged = new IscSendRecordDao()
                        .purgeExpiredPending();
                PurgeLogger.logInfo("Purged " + requestsPurged
                        + " expired pending isc send requests.", "gfe");
            } catch (DataAccessLayerException e) {
                throw new PluginException(
                        "Error purging expired send ISC records!", e);
            } finally {
                SendNotifications.send(gridNotifcations);
                SendNotifications.send(lockNotifications);
            }
        }
    }

    @SuppressWarnings("unchecked")
    public int purgeDatabaseForSite(final String siteID)
            throws DataAccessLayerException {
        return (Integer) txTemplate.execute(new TransactionCallback() {
            @Override
            public Integer doInTransaction(TransactionStatus status) {
                List<DatabaseID> dbs = getDatabaseInventoryForSite(siteID);
                if (dbs.isEmpty()) {
                    return 0;
                } else {
                    DetachedCriteria criteria = DetachedCriteria.forClass(
                            GFERecord.class).add(
                            Property.forName("dbId").in(dbs));
                    List<GFERecord> list = getHibernateTemplate()
                            .findByCriteria(criteria);
                    if (!list.isEmpty()) {
                        getHibernateTemplate().deleteAll(list);
                    }
                    return list.size();
                }
            }
        });
    }

    private List<DatabaseID> getDatabaseInventoryForSite(String siteID) {
        List<DatabaseID> dbInventory = new ArrayList<DatabaseID>();
        Object[] dbIds = executeSQLQuery("select distinct dbId from awips.gfe where dbId like '"
                + siteID.toUpperCase() + "%'");
        for (Object id : dbIds) {
            dbInventory.add(new DatabaseID((String) id));
        }
        return dbInventory;
    }

    /**
     * Retrieves a GFE Record
     * 
     * @param record
     *            the record
     * @return The record, populated
     */
    public GFERecord getRecord(PluginDataObject record) {
        return (GFERecord) this.queryById(record);
    }

    public List<GFERecord> saveOrUpdate(final List<GFERecord> records) {
        List<GFERecord> failedToSave = new ArrayList<GFERecord>();
        for (GFERecord rec : records) {
            if (rec.getIdentifier() == null) {
                try {
                    rec.constructDataURI();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            if (rec.getInsertTime() == null) {
                rec.setInsertTime(Calendar.getInstance());
            }
        }

        final int batchSize = 100;

        // First, try committing all of the records in batches of size
        // batchSize. If a failure occurs on a batch, add that batch to be
        // retried individually. If the whole commit fails, try saving them all
        // individually.
        Session sess = null;
        Transaction tx = null;
        int commitPoint = 0;
        int index = 0;
        boolean notDone = index < records.size();
        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            boolean persistIndividually = false;
            String sql = "select id from awips." + pluginName
                    + " where dataURI=:dataURI";
            Query q = sess.createSQLQuery(sql);

            while (notDone) {
                GFERecord rec = records.get(index++);
                notDone = index < records.size();
                try {
                    q.setString("dataURI", rec.getDataURI());
                    List<?> list = q.list();
                    if ((list == null) || (list.size() == 0)) {
                        sess.save(rec);
                    } else {
                        rec.setId(((Number) list.get(0)).intValue());
                        sess.update(rec);
                    }
                    if ((index % batchSize == 0) || persistIndividually
                            || !notDone) {
                        sess.flush();
                        sess.clear();
                        tx.commit();
                        tx = null;
                        commitPoint = index;
                        if (persistIndividually && (index % batchSize == 0)) {
                            // batch persisted individually switch back to batch
                            persistIndividually = false;
                        }
                        if (notDone) {
                            tx = sess.beginTransaction();
                            q = sess.createSQLQuery(sql);
                        }
                    }
                } catch (Exception e) {
                    if (tx != null) {
                        try {
                            tx.rollback();
                        } catch (Exception e1) {
                            logger.error(
                                    "Error occurred rolling back transaction",
                                    e1);
                        }
                    }

                    if (persistIndividually) {
                        // log it and continue
                        logger.error(
                                "Error occurred persisting gfe record individually",
                                e);
                        failedToSave.add(rec);
                    } else {
                        // change to persistIndividually and retry from last
                        // commit
                        persistIndividually = true;
                        index = commitPoint;
                        notDone = true;
                    }

                    tx = sess.beginTransaction();
                    q = sess.createSQLQuery(sql);
                }
            }
        } finally {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    statusHandler.error(
                            "Error occurred closing database session", e);
                }
            }
        }

        return failedToSave;

    }

    @Override
    public void delete(final PersistableDataObject obj) {
        GFERecord rec = (GFERecord) obj;
        if (rec.getDataURI() == null) {
            try {
                rec.constructDataURI();
            } catch (PluginException e) {
                logger.error("Unable to construct dataURI for GFE record", e);
            }
        }
        rec = this.getRecord(rec);
        super.delete(rec);
    }

    /**
     * Gets list of all database IDs currently being stored in the database
     * 
     * @return The list of all database IDs currently being stored in the
     *         database
     */
    public List<DatabaseID> getDatabaseInventory() {
        List<DatabaseID> dbInventory = new ArrayList<DatabaseID>();

        Object[] dbIds = executeSQLQuery("select distinct dbId from awips.gfe");
        for (Object id : dbIds) {
            dbInventory.add(new DatabaseID((String) id));
        }
        return dbInventory;
    }

    /**
     * Gets all GFE Records with the specified ParmID
     * 
     * @param parmId
     *            The parmID to query for
     * @return All GFE Records with the specified ParmID
     * @throws DataAccessLayerException
     *             If errors occur during the query
     */
    @SuppressWarnings("unchecked")
    public ArrayList<GFERecord> queryByParmID(ParmID parmId)
            throws DataAccessLayerException {
        return (ArrayList<GFERecord>) this.queryBySingleCriteria("parmId",
                parmId.getParmId());
    }

    public GFERecord getRecord(final ParmID parmId, final TimeRange tr)
            throws DataAccessLayerException {
        GFERecord retVal = (GFERecord) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    @SuppressWarnings("unchecked")
                    public GFERecord doInTransaction(TransactionStatus status) {
                        DetachedCriteria criteria = DetachedCriteria
                                .forClass(GFERecord.class)
                                .add(Property.forName("parmId").eq(parmId))
                                .add(Property.forName("dataTime").eq(
                                        new DataTime(tr.getStart().getTime(),
                                                tr)));
                        return ((List<GFERecord>) getHibernateTemplate()
                                .findByCriteria(criteria)).get(0);
                    }
                });
        return retVal;
    }

    @SuppressWarnings("unchecked")
    public List<GFERecord> getRecords(final ParmID parmId,
            final List<TimeRange> times) {
        if (times.isEmpty()) {
            return Collections.emptyList();
        }
        List<GFERecord> retVal = (List<GFERecord>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public List<GFERecord> doInTransaction(
                            TransactionStatus status) {
                        List<DataTime> dataTimes = new ArrayList<DataTime>();
                        for (TimeRange tr : times) {
                            dataTimes.add(new DataTime(tr.getStart().getTime(),
                                    tr));
                        }

                        DetachedCriteria criteria = DetachedCriteria
                                .forClass(GFERecord.class)
                                .add(Property.forName("parmId").eq(parmId))
                                .add(Property.forName("dataTime").in(dataTimes));
                        List<GFERecord> list = getHibernateTemplate()
                                .findByCriteria(criteria);
                        return list;
                    }
                });
        return retVal;
    }

    public void deleteRecords(final ParmID parmId, final List<TimeRange> times) {
        if (times.isEmpty()) {
            return;
        }
        final List<GFERecord> recordsToDelete = getRecords(parmId, times);
        final List<GridDataHistory> histories = new ArrayList<GridDataHistory>();
        for (GFERecord rec : recordsToDelete) {
            histories.addAll(rec.getGridHistory());
        }
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            public void doInTransactionWithoutResult(TransactionStatus status) {
                getHibernateTemplate().deleteAll(histories);
                getHibernateTemplate().deleteAll(recordsToDelete);
                statusHandler.info("Deleted " + recordsToDelete.size()
                        + " records from the database.");
            }
        });

        Map<File, Pair<List<TimeRange>, String[]>> fileMap = GfeUtil
                .getHdf5FilesAndGroups(GridDatabase.gfeBaseDataDir, parmId,
                        times);
        for (Map.Entry<File, Pair<List<TimeRange>, String[]>> entry : fileMap
                .entrySet()) {
            File hdf5File = entry.getKey();
            IDataStore dataStore = DataStoreFactory.getDataStore(hdf5File);
            String[] groupsToDelete = entry.getValue().getSecond();

            try {
                dataStore.deleteGroups(groupsToDelete);

                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.handle(Priority.DEBUG,
                            "Deleted: " + Arrays.toString(groupsToDelete)
                                    + " from " + hdf5File.getName());
                }
            } catch (Exception e) {
                statusHandler.handle(
                        Priority.WARN,
                        "Error deleting hdf5 record(s) from file: "
                                + hdf5File.getPath(), e);
            }
        }
    }

    @SuppressWarnings("unchecked")
    @Deprecated
    public void updateGridHistories(ParmID parmId,
            Map<TimeRange, List<GridDataHistory>> history)
            throws DataAccessLayerException {
        for (TimeRange range : history.keySet()) {
            DatabaseQuery recordQuery = new DatabaseQuery(GFERecord.class);
            recordQuery.addQueryParam("parmId", parmId);
            recordQuery.addQueryParam("dataTime.validPeriod", range);
            List<GFERecord> result = (List<GFERecord>) this
                    .queryByCriteria(recordQuery);
            if (result.size() == 0) {
                logger.error("No histories were updated for: " + parmId + "::"
                        + range);
            } else if (result.size() == 1) {
                GFERecord returnedRecord = result.get(0);

                List<GridDataHistory> existHist = returnedRecord
                        .getGridHistory();
                List<GridDataHistory> newHist = history.get(range);
                consolidateHistories(existHist, newHist);

                this.update(returnedRecord);
            } else {
                logger.error("MORE THAN 1 RESULT WAS RETURNED: "
                        + result.size());
            }
        }
    }

    public void consolidateHistories(List<GridDataHistory> existHist,
            List<GridDataHistory> newHist) {
        for (int i = 0; i < newHist.size(); i++) {
            if (i < existHist.size()) {
                existHist.get(i).replaceValues(newHist.get(i));
            } else {
                existHist.add(newHist.get(i));
            }
        }

        if (existHist.size() > newHist.size()) {
            // not sure if we will ever have a case where the updated
            // record has fewer history records than existing record

            // log a message as this has the potential to cause orphaned
            // history records
            statusHandler.handle(Priority.WARN,
                    "Updated record has fewer history records.");
            for (int i = newHist.size(); i < existHist.size(); i++) {
                existHist.remove(i);
            }
        }
    }

    /**
     * Gets all GFE Records with the specified DatabaseID
     * 
     * @param dbId
     *            The DatabaseID to query for
     * @return All GFE Records with the specified DatabaseID
     * @throws DataAccessLayerException
     *             If errors occur during the query
     */
    @SuppressWarnings("unchecked")
    public ArrayList<GFERecord> queryByDatabaseID(DatabaseID dbId)
            throws DataAccessLayerException {
        return (ArrayList<GFERecord>) this.queryBySingleCriteria("dbId",
                dbId.toString());
    }

    /**
     * Gets the list of times for a given parmId
     * 
     * @param parmId
     *            The id of the parm
     * @return The list of times for a given parm name and level
     * @throws DataAccessLayerException
     */
    public List<TimeRange> getTimes(ParmID parmId)
            throws DataAccessLayerException {
        List<TimeRange> times = new ArrayList<TimeRange>();
        String timeQuery = "SELECT rangestart,rangeend from awips.gfe where parmid='"
                + parmId + "' ORDER BY rangestart";
        QueryResult result = (QueryResult) executeNativeSql(timeQuery);
        for (int i = 0; i < result.getResultCount(); i++) {
            times.add(new TimeRange((Date) result.getRowColumnValue(i, 0),
                    (Date) result.getRowColumnValue(i, 1)));
        }
        return times;
    }

    /**
     * Retrieves the grid history for the specified parm and time ranges
     * 
     * @param id
     *            The parm id
     * @param trs
     *            The time ranges to search for
     * @return The grid histories
     * @throws DataAccessLayerException
     *             If problems during database interaction occur
     */
    public Map<TimeRange, List<GridDataHistory>> getGridHistory(ParmID id,
            List<TimeRange> trs) throws DataAccessLayerException {

        Map<TimeRange, List<GridDataHistory>> history = new HashMap<TimeRange, List<GridDataHistory>>();
        if (trs.isEmpty()) {
            return history;
        }
        List<GFERecord> records = this.getRecords(id, trs);
        for (GFERecord rec : records) {
            TimeRange tr = rec.getTimeRange();
            history.put(tr, rec.getGridHistory());
        }
        return history;
    }

    public void purgeGFEGrids(final DatabaseID dbId) {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            @SuppressWarnings("unchecked")
            public void doInTransactionWithoutResult(TransactionStatus status) {
                DetachedCriteria criteria = DetachedCriteria.forClass(
                        GFERecord.class).add(Property.forName("dbId").eq(dbId));
                List<GFERecord> list = getHibernateTemplate().findByCriteria(
                        criteria);
                if (!list.isEmpty()) {
                    List<GridDataHistory> histories = new ArrayList<GridDataHistory>();
                    for (GFERecord rec : list) {
                        histories.addAll(rec.getGridHistory());
                    }
                    getHibernateTemplate().deleteAll(histories);
                    getHibernateTemplate().deleteAll(list);
                }
            }
        });
    }

    /**
     * Removes GridParmInfo from the HDF5 file and any data associated with that
     * info
     * 
     * @param parmAndLevel
     *            The parm and level to delete
     * @param dbId
     *            The database to delete from
     * @throws DataAccessLayerException
     *             If errors occur
     */
    public void removeOldParm(String parmAndLevel, DatabaseID dbId)
            throws DataAccessLayerException {

        ParmID pid = new ParmID(parmAndLevel + ":" + dbId.toString());

        try {
            IDataStore ds = DataStoreFactory.getDataStore(GfeUtil
                    .getGridParmHdf5File(GridDatabase.gfeBaseDataDir, dbId));
            ds.deleteDatasets("/GridParmInfo/" + parmAndLevel,
                    "/GridParmStorageInfo/" + parmAndLevel);
        } catch (Exception e1) {
            throw new DataAccessLayerException("Error deleting data from HDF5",
                    e1);
        }
        List<TimeRange> trs = this.getTimes(pid);
        this.deleteRecords(pid, trs);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        return null;
    }

    public Date getLatestDbIdInsertTime(DatabaseID dbId)
            throws DataAccessLayerException {
        QueryResult result = (QueryResult) this
                .executeNativeSql("select max(inserttime) as maxtime from awips.gfe where dbid='"
                        + dbId.toString() + "';");
        if (result.getResultCount() == 0) {
            return null;
        } else {
            return (Date) result.getRowColumnValue(0, "maxtime");
        }
    }

    /**
     * Retrieves the latest database ID for a given a model name and site
     * identifier.
     * 
     * @param siteId
     *            The site's identifier (e.g., "OAX")
     * @param modelName
     *            The name of the model run (e.g., "GFS40" or "RUC13")
     * @return The DatabaseID of the latest model run for the given parameters
     *         or null if no copies of the given model have been ingested for
     *         the given site.
     * @throws DataAccessLayerException
     */
    public DatabaseID getLatestModelDbId(String siteId, String modelName)
            throws DataAccessLayerException {
        QueryResult result = (QueryResult) this
                .executeNativeSql("select max(dbid) as maxdbid from awips.gfe where dbid like '"
                        + siteId + "!_GRID!_!_" + modelName + "!_%' escape '!'");
        if (result.getResultCount() == 0) {
            return null;
        } else {
            String db = (String) result.getRowColumnValue(0, "maxdbid");
            if (db == null) {
                return null;
            }
            return new DatabaseID(db);
        }
    }

    /**
     * Updates the publish times in the database of all provided
     * GridDataHistories. Does not alter the publish times in memory.
     * 
     * @param history
     *            the histories to alter in the database
     * @param publishTime
     *            the publish time to update to
     * @throws DataAccessLayerException
     */
    public void updatePublishTime(List<GridDataHistory> history,
            Date publishTime) throws DataAccessLayerException {
        StringBuilder query = new StringBuilder();
        query.append("update gfe_gridhistory set publishtime=:publishtime where key in (");
        Iterator<GridDataHistory> itr = history.iterator();
        while (itr.hasNext()) {
            query.append(itr.next().getKey());
            if (itr.hasNext()) {
                query.append(",");
            }
        }
        query.append(");");

        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            Query q = sess.createSQLQuery(query.toString());
            q.setTimestamp("publishtime", publishTime);
            q.executeUpdate();
            tx.commit();
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                    tx = null;
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }
            throw new DataAccessLayerException("Error updating history", e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    statusHandler.error(
                            "Error occurred closing database session", e);
                }
            }
        }
    }
}
