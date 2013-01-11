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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Property;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.edex.plugin.gfe.config.GFESiteActivation;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.database.D2DGridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.util.GridTranslator;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.LockNotification;
import com.raytheon.uf.common.dataplugin.gfe.type.Pair;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;
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
 * 10/10/12     #1260      randerso    Added check to ensure db can be created before 
 *                                     adding it to the inventory
 * 12/06/12     #1394      rjpeter     Optimized D2D grid access.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GFEDao extends DefaultPluginDao {
    // hibernate query to find grid info record for the given datasetId and
    // parameter
    private String SQL_D2D_GRID_PARM_QUERY = "select parameter_abbreviation, id "
            + "FROM grid_info WHERE "
            + GridInfoConstants.DATASET_ID
            + " = :"
            + GridInfoConstants.DATASET_ID
            + " AND "
            + "level_id = :level_id  AND "
            + "(lower(parameter_abbreviation) = :abbrev OR lower(parameter_abbreviation) like :hourAbbrev)";

    // hibernate query to find the times for the GridRecord for the given
    // info.id, id returned to allow easy lookup of the record associated with
    // the time
    private static final String HQL_D2D_GRID_TIME_QUERY = "select dataTime, id from GridRecord "
            + "where "
            + GridConstants.INFO_ID
            + " = :info_id AND dataTime.refTime = :refTime order by dataTime.fcstTime";

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

    public GFERecord[] saveOrUpdate(final GFERecord[] records) {
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
        boolean notDone = index < records.length;
        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            boolean persistIndividually = false;
            String sql = "select id from awips." + pluginName
                    + " where dataURI=:dataURI";
            Query q = sess.createSQLQuery(sql);

            while (notDone) {
                GFERecord rec = records[index++];
                notDone = index < records.length;
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

        return failedToSave.toArray(new GFERecord[failedToSave.size()]);

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
                dataStore.delete(groupsToDelete);

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

    /**
     * Retrieves a list of valid times for a specified ParmID from the grib
     * metadata database. The valid time is constructed by adding the forecast
     * time to the reference time.
     * 
     * @param id
     *            The parmID to get the times for
     * @return The list of times associated with the specified ParmID
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    public List<TimeRange> getD2DTimes(ParmID id)
            throws DataAccessLayerException {
        return queryTimeByD2DParmId(id);
    }

    /**
     * Retrieves a list of available forecast times
     * 
     * @param dbId
     *            The database ID to get the times for
     * @return The list of forecast times associated with the specified
     *         DatabaseID
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    @SuppressWarnings("unchecked")
    public List<Integer> getD2DForecastTimes(DatabaseID dbId)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());
        query.addDistinctParameter("dataTime.fcstTime");
        try {
            IFPServerConfig config = IFPServerConfigManager
                    .getServerConfig(dbId.getSiteId());
            query.addQueryParam(GridConstants.DATASET_ID,
                    config.d2dModelNameMapping(dbId.getModelName()));
        } catch (GfeConfigurationException e) {
            throw new DataAccessLayerException(
                    "Error occurred looking up model name mapping", e);
        }
        query.addQueryParam("dataTime.refTime", dbId.getModelTimeAsDate());
        query.addOrder("dataTime.fcstTime", true);
        List<?> vals = this.queryByCriteria(query);
        return (List<Integer>) vals;
    }

    /**
     * Retrieves a GridRecord from the grib metadata database based on a ParmID,
     * TimeRange, and GridParmInfo.
     * 
     * @param id
     *            The parmID of the desired GridRecord
     * @param timeRange
     *            The timeRange of the desired GridRecord
     * @param info
     *            The GridParmInfo for the requested d2d grid.
     * @return The GridRecord from the grib metadata database
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    public GridRecord getD2DGrid(ParmID id, TimeRange timeRange,
            GridParmInfo info) throws DataAccessLayerException {
        Session s = null;

        try {
            s = getHibernateTemplate().getSessionFactory().openSession();
            // TODO: clean up so we only make one db query
            SortedMap<DataTime, Integer> rawTimes = queryByD2DParmId(id, s);
            List<TimeRange> gribTimes = new ArrayList<TimeRange>();
            for (DataTime dt : rawTimes.keySet()) {
                gribTimes.add(dt.getValidPeriod());
            }

            try {
                if (isMos(id)) {
                    for (Map.Entry<DataTime, Integer> timeEntry : rawTimes
                            .entrySet()) {
                        TimeRange gribTime = timeEntry.getKey()
                                .getValidPeriod();
                        TimeRange time = info.getTimeConstraints()
                                .constraintTime(gribTime.getEnd());
                        if (timeRange.getEnd().equals(time.getEnd())
                                || !info.getTimeConstraints().anyConstraints()) {
                            GridRecord retVal = (GridRecord) s.get(
                                    GridRecord.class, timeEntry.getValue());
                            retVal.setPluginName(GridConstants.GRID);
                            return retVal;
                        }
                    }
                } else if (D2DGridDatabase.isNonAccumDuration(id, gribTimes)) {
                    for (Map.Entry<DataTime, Integer> timeEntry : rawTimes
                            .entrySet()) {
                        TimeRange gribTime = timeEntry.getKey()
                                .getValidPeriod();
                        if (timeRange.getStart().equals(gribTime.getEnd())
                                || timeRange.equals(gribTime)) {
                            GridRecord retVal = (GridRecord) s.get(
                                    GridRecord.class, timeEntry.getValue());
                            retVal.setPluginName(GridConstants.GRID);
                            return retVal;
                        }
                    }
                } else {
                    for (Map.Entry<DataTime, Integer> timeEntry : rawTimes
                            .entrySet()) {
                        TimeRange gribTime = timeEntry.getKey()
                                .getValidPeriod();
                        TimeRange time = info.getTimeConstraints()
                                .constraintTime(gribTime.getStart());
                        if ((timeRange.getStart().equals(time.getStart()) || !info
                                .getTimeConstraints().anyConstraints())) {
                            GridRecord retVal = (GridRecord) s.get(
                                    GridRecord.class, timeEntry.getValue());
                            retVal.setPluginName(GridConstants.GRID);
                            return retVal;
                        }
                    }
                }
            } catch (GfeConfigurationException e) {
                throw new DataAccessLayerException(
                        "Error getting configuration for "
                                + id.getDbId().getSiteId(), e);
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (Exception e) {
                    statusHandler.error(
                            "Error occurred closing database session", e);
                }
            }
        }

        return null;
    }

    /**
     * Gets a SortedMap of DataTime and GridRecord ids from the grib metadata
     * database which match the given ParmID. Session passed to allow reuse
     * across multiple calls.
     * 
     * @param id
     *            The ParmID to search with
     * @param s
     *            The database session to use
     * @return The list of GridRecords from the grib metadata database which
     *         match the given ParmID
     * @throws DataAccessLayerException
     *             If errors occur while querying the metadata database
     */
    @SuppressWarnings("unchecked")
    public SortedMap<DataTime, Integer> queryByD2DParmId(ParmID id, Session s)
            throws DataAccessLayerException {
        String levelName = GridTranslator.getLevelName(id.getParmLevel());

        double[] levelValues = GridTranslator.getLevelValue(id.getParmLevel());
        boolean levelOnePresent = (levelValues[0] != Level
                .getInvalidLevelValue());
        boolean levelTwoPresent = (levelValues[1] != Level
                .getInvalidLevelValue());
        Level level = null;

        // to have a level 2, must have a level one
        try {
            if (levelOnePresent && levelTwoPresent) {
                level = LevelFactory.getInstance().getLevel(levelName,
                        levelValues[0], levelValues[1]);
            } else if (levelOnePresent) {
                level = LevelFactory.getInstance().getLevel(levelName,
                        levelValues[0]);
            } else {
                level = LevelFactory.getInstance().getLevel(levelName, 0.0);
            }
        } catch (CommunicationException e) {
            logger.error(e.getLocalizedMessage(), e);
        }
        if (level == null) {
            logger.warn("Unable to query D2D parms, ParmID " + id
                    + " does not map to a level");
            return new TreeMap<DataTime, Integer>();
        }

        SQLQuery modelQuery = s.createSQLQuery(SQL_D2D_GRID_PARM_QUERY);
        modelQuery.setLong("level_id", level.getId());
        DatabaseID dbId = id.getDbId();

        try {
            IFPServerConfig config = IFPServerConfigManager
                    .getServerConfig(dbId.getSiteId());
            modelQuery.setString(GridInfoConstants.DATASET_ID,
                    config.d2dModelNameMapping(dbId.getModelName()));
        } catch (GfeConfigurationException e) {
            throw new DataAccessLayerException(
                    "Error occurred looking up model name mapping", e);
        }

        String abbreviation = null;
        try {
            abbreviation = ParameterMapper.getInstance().lookupBaseName(
                    id.getParmName(), "gfeParamName");
        } catch (MultipleMappingException e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            abbreviation = e.getArbitraryMapping();
        }

        abbreviation = abbreviation.toLowerCase();
        modelQuery.setString("abbrev", abbreviation);
        modelQuery.setString("hourAbbrev", abbreviation + "%hr");
        List<?> results = modelQuery.list();
        Integer modelId = null;
        if (results.size() == 0) {
            return new TreeMap<DataTime, Integer>();
        } else if (results.size() > 1) {
            // hours matched, take hour with least number that matches exact
            // param
            Pattern p = Pattern.compile("^" + abbreviation + "(\\d+)hr$");
            int lowestHr = -1;
            for (Object[] rows : (List<Object[]>) results) {
                String param = ((String) rows[0]).toLowerCase();
                if (param.equals(abbreviation) && (lowestHr < 0)) {
                    modelId = (Integer) rows[1];
                } else {
                    Matcher matcher = p.matcher(param);
                    if (matcher.matches()) {
                        int hr = Integer.parseInt(matcher.group(1));
                        if ((lowestHr < 0) || (hr < lowestHr)) {
                            modelId = (Integer) rows[1];
                            lowestHr = hr;
                        }
                    }
                }
            }
        } else {
            modelId = (Integer) ((Object[]) results.get(0))[1];
        }

        Query timeQuery = s.createQuery(HQL_D2D_GRID_TIME_QUERY);
        timeQuery.setInteger("info_id", modelId);
        timeQuery.setParameter("refTime", dbId.getModelTimeAsDate());
        List<Object[]> timeResults = timeQuery.list();
        if (timeResults.isEmpty()) {
            return new TreeMap<DataTime, Integer>();
        }

        SortedMap<DataTime, Integer> dataTimes = new TreeMap<DataTime, Integer>();
        for (Object[] rows : timeResults) {
            dataTimes.put((DataTime) rows[0], (Integer) rows[1]);
        }
        return dataTimes;
    }

    public List<TimeRange> queryTimeByD2DParmId(ParmID id)
            throws DataAccessLayerException {
        List<TimeRange> timeList = new ArrayList<TimeRange>();
        Session s = null;
        try {
            s = getHibernateTemplate().getSessionFactory().openSession();

            if (id.getParmName().equalsIgnoreCase("wind")) {
                String idString = id.toString();
                Matcher idWindMatcher = WIND_PATTERN.matcher(idString);

                ParmID uWindId = new ParmID(idWindMatcher.replaceAll("uW"));
                SortedMap<DataTime, Integer> results = queryByD2DParmId(
                        uWindId, s);
                List<TimeRange> uTimeList = new ArrayList<TimeRange>(
                        results.size());
                for (DataTime o : results.keySet()) {
                    uTimeList.add(new TimeRange(o.getValidPeriod().getStart(),
                            3600 * 1000));
                }

                ParmID vWindId = new ParmID(idWindMatcher.replaceAll("vW"));
                results = queryByD2DParmId(vWindId, s);
                Set<TimeRange> vTimeList = new HashSet<TimeRange>(
                        results.size(), 1);
                for (DataTime o : results.keySet()) {
                    vTimeList.add(new TimeRange(o.getValidPeriod().getStart(),
                            3600 * 1000));
                }

                for (TimeRange tr : uTimeList) {
                    if (vTimeList.contains(tr)) {
                        timeList.add(new TimeRange(tr.getStart(), tr.getStart()));
                    }
                }

                if (!timeList.isEmpty()) {
                    return timeList;
                }

                ParmID sWindId = new ParmID(idWindMatcher.replaceAll("ws"));
                results = queryByD2DParmId(sWindId, s);
                List<TimeRange> sTimeList = new ArrayList<TimeRange>(
                        results.size());
                for (DataTime o : results.keySet()) {
                    sTimeList.add(new TimeRange(o.getValidPeriod().getStart(),
                            3600 * 1000));
                }

                ParmID dWindId = new ParmID(idWindMatcher.replaceAll("wd"));
                results = queryByD2DParmId(dWindId, s);
                Set<TimeRange> dTimeList = new HashSet<TimeRange>(
                        results.size(), 1);
                for (DataTime o : results.keySet()) {
                    dTimeList.add(new TimeRange(o.getValidPeriod().getStart(),
                            3600 * 1000));
                }

                for (TimeRange tr : sTimeList) {
                    if (dTimeList.contains(tr)) {
                        timeList.add(new TimeRange(tr.getStart(), tr.getStart()));
                    }
                }
            } else {
                SortedMap<DataTime, Integer> results = queryByD2DParmId(id, s);
                if (isMos(id)) {
                    for (DataTime o : results.keySet()) {
                        timeList.add(new TimeRange(o.getValidPeriod().getEnd(),
                                o.getValidPeriod().getDuration()));
                    }
                } else {
                    for (DataTime o : results.keySet()) {
                        timeList.add(o.getValidPeriod());
                    }
                }
            }
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (Exception e) {
                    statusHandler.error(
                            "Error occurred closing database session", e);
                }
            }
        }

        return timeList;
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

    public List<DatabaseID> getD2DDatabaseIdsFromDb(String d2dModelName,
            String gfeModel, String siteID) throws DataAccessLayerException {
        return getD2DDatabaseIdsFromDb(d2dModelName, gfeModel, siteID, -1);
    }

    public List<DatabaseID> getD2DDatabaseIdsFromDb(String d2dModelName,
            String gfeModel, String siteID, int maxRecords)
            throws DataAccessLayerException {
        List<DatabaseID> dbInventory = new ArrayList<DatabaseID>();

        DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());
        query.addDistinctParameter("dataTime.refTime");
        query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
        query.addOrder("dataTime.refTime", false);
        if (maxRecords > 0) {
            query.setMaxResults(maxRecords);
        }
        List<?> result = this.queryByCriteria(query);

        for (Object obj : result) {
            DatabaseID dbId = null;
            dbId = new DatabaseID(siteID, DataType.GRID, "D2D", gfeModel,
                    (Date) obj);
            try {
                GridDatabase db = GridParmManager.getDb(dbId);
                if ((db != null) && !dbInventory.contains(dbId)) {
                    dbInventory.add(dbId);
                }
            } catch (GfeException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return dbInventory;
    }

    /**
     * Retrieves the latest (or newest) model run for the given site and model
     * name.
     * 
     * @param d2dModel
     *            A GridModel object that contains the D2D model name.
     * @param gfeModel
     *            The GFE model name that corresponds to d2dModel.
     * @param siteID
     *            The site to retrieve the data for.
     * @return The DatabaseID of the newest D2D model, or null if no models can
     *         be found.
     * @throws DataAccessLayerException
     */
    public DatabaseID getLatestD2DDatabaseIdsFromDb(String d2dModelName,
            String gfeModel, String siteID) throws DataAccessLayerException {
        List<DatabaseID> dbIds = getD2DDatabaseIdsFromDb(d2dModelName,
                gfeModel, siteID, 1);
        if (!dbIds.isEmpty()) {
            return dbIds.get(0);
        } else {
            return null;
        }
    }

    public Set<ParmID> getD2DParmIdsFromDb(String d2dModelName, DatabaseID dbId)
            throws DataAccessLayerException {

        Set<ParmID> parmIds = new HashSet<ParmID>();

        DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());
        query.addDistinctParameter(GridConstants.PARAMETER_ABBREVIATION);
        query.addDistinctParameter(GridConstants.MASTER_LEVEL_NAME);
        query.addDistinctParameter(GridConstants.LEVEL_ONE);
        query.addDistinctParameter(GridConstants.LEVEL_TWO);
        query.addQueryParam(GridConstants.DATASET_ID, d2dModelName);
        query.addQueryParam(
                "dataTime.refTime",
                TimeUtil.formatDate(dbId.getModelTimeAsDate()).replaceAll("_",
                        " "));

        List<?> result = this.queryByCriteria(query);

        for (Object obj : result) {
            Object[] objArr = (Object[]) obj;
            String levelName = GridTranslator.getShortLevelName(
                    (String) objArr[1], (Double) objArr[2], (Double) objArr[3]);
            if (!levelName.equals(LevelFactory.UNKNOWN_LEVEL)) {
                String abbrev = (String) objArr[0];
                try {
                    abbrev = ParameterMapper.getInstance().lookupAlias(abbrev,
                            "gfeParamName");
                } catch (MultipleMappingException e) {
                    statusHandler.handle(Priority.WARN,
                            e.getLocalizedMessage(), e);
                    abbrev = e.getArbitraryMapping();
                }
                ParmID newParmId = new ParmID(abbrev, dbId, levelName);
                parmIds.add(newParmId);
            }

        }
        return parmIds;
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
            ds.delete("/GridParmInfo/" + parmAndLevel);
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

    public static boolean isMos(ParmID id) {
        return id.getDbId().getModelName().equals("MOSGuide")
                && (id.getParmName().startsWith("mxt") || id.getParmName()
                        .startsWith("mnt"));
    }
}
