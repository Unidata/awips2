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
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.hibernate.LockOptions;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;
import org.hibernate.exception.ConstraintViolationException;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.edex.plugin.gfe.config.GFESiteActivation;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
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
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.CollectionUtil;
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
 * 04/08/13     #1949      rjpeter     Normalized GFE Database.
 * 05/22/13     #2025      dgilling    Re-implement functions needed by 
 *                                     GetLatestDbTimeRequest and GetLatestModelDbIdRequest.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GFEDao extends DefaultPluginDao {
    // Number of retries on insert of a new DatabaseID
    private static final int QUERY_RETRY = 2;

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

    /**
     * Returns the database row for the passed dbId. If the row does not exist,
     * the row will be created.
     * 
     * @param dbId
     * @return
     */
    public DatabaseID getDatabaseId(DatabaseID dbId)
            throws DataAccessLayerException {
        DatabaseID rval = null;
        Session sess = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            int tries = 0;
            Transaction tx = null;
            while ((rval == null) && (tries < QUERY_RETRY)) {
                try {
                    tx = sess.beginTransaction();
                    rval = getDatabaseId(sess, dbId);

                    if (rval == null) {
                        sess.save(dbId);
                    }

                    tx.commit();

                    if (rval == null) {
                        rval = dbId;
                    }
                } catch (ConstraintViolationException e) {
                    if (tx != null) {
                        try {
                            tx.rollback();
                        } catch (Exception e1) {
                            logger.error(
                                    "Error occurred rolling back transaction",
                                    e1);
                        }
                    }

                    rval = null;
                    // database may have been inserted on another process, redo
                    // the look up
                    if (tries < 2) {
                        logger.info("Constraint violation on save, attempting to look up database id again");
                    } else {
                        throw new DataAccessLayerException(
                                "Unable to look up DatabaseID: "
                                        + dbId.toString(), e);
                    }
                }
            }
        } catch (Exception e) {
            throw new DataAccessLayerException("Unable to look up DatabaseID: "
                    + dbId.toString(), e);
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

        return rval;
    }

    /**
     * Looks up a database id from database. Session must have already started a
     * transaction.
     * 
     * @param sess
     * @param dbId
     * @return
     */
    private DatabaseID getDatabaseId(Session sess, DatabaseID dbId) {
        Query query = sess
                .createQuery("from DatabaseID where siteId = ? and modelName = ? and modelTime = ? and dbType = ?");
        query.setString(0, dbId.getSiteId());
        query.setString(1, dbId.getModelName());
        query.setString(2, dbId.getModelTime());
        query.setString(3, dbId.getDbType());
        return (DatabaseID) query.uniqueResult();
    }

    /**
     * Retrieves all known parm ids for the given database id.
     * 
     * @param dbId
     * @return
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<ParmID> getParmIds(final DatabaseID dbId)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();

            // reattach so dbId doesn't requery
            sess.buildLockRequest(LockOptions.NONE).lock(dbId);

            Query query = sess.createQuery("FROM ParmID WHERE dbId = ?");
            query.setParameter(0, dbId);
            List<ParmID> list = query.list();
            tx.commit();
            return list;
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException(
                    "Unable to look up parm id inventory for database id "
                            + dbId, e);
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

    /**
     * Returns the database row for the passed parmId. If the row does not
     * exist, the row will be created.
     * 
     * @param dbId
     * @return
     */
    public ParmID getParmId(final ParmID parmId)
            throws DataAccessLayerException {
        ParmID rval = null;
        Session sess = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();

            // reattach so dbId doesn't requery
            sess.buildLockRequest(LockOptions.NONE).lock(parmId.getDbId());

            int tries = 0;
            Transaction tx = null;
            while ((rval == null) && (tries < QUERY_RETRY)) {
                try {
                    tx = sess.beginTransaction();
                    rval = getParmId(sess, parmId);

                    if (rval == null) {
                        sess.save(parmId);
                    }

                    tx.commit();

                    if (rval == null) {
                        rval = parmId;
                    }
                } catch (ConstraintViolationException e) {
                    if (tx != null) {
                        try {
                            tx.rollback();
                        } catch (Exception e1) {
                            logger.error(
                                    "Error occurred rolling back transaction",
                                    e1);
                        }
                    }

                    rval = null;
                    // database may have been inserted on another process, redo
                    // the look up
                    if (tries < 2) {
                        logger.info("Constraint violation on save, attempting to look up parm id again");
                    } else {
                        throw new DataAccessLayerException(
                                "Unable to look up ParmID: "
                                        + parmId.toString(), e);
                    }
                }
            }
        } catch (Exception e) {
            throw new DataAccessLayerException("Unable to look up ParmID: "
                    + parmId.toString(), e);
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

        return rval;
    }

    /**
     * Looks up a parm id from database. Session must have already started a
     * transaction.
     * 
     * @param sess
     * @param dbId
     * @return
     */
    private ParmID getParmId(Session sess, ParmID parmId) {
        Query query = sess
                .createQuery("FROM ParmID WHERE dbId = ? AND parmName = ? AND parmLevel = ?");
        query.setParameter(0, parmId.getDbId());
        query.setString(1, parmId.getParmName());
        query.setString(2, parmId.getParmLevel());
        return (ParmID) query.uniqueResult();
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

    public int purgeDatabaseForSite(final String siteID)
            throws DataAccessLayerException {
        return (Integer) txTemplate.execute(new TransactionCallback() {
            @Override
            public Integer doInTransaction(TransactionStatus status) {
                return getHibernateTemplate().bulkUpdate(
                        "DELETE FROM DatabaseID WHERE siteId = ?", siteID);
            }
        });
    }

    /**
     * 
     * @param records
     * @return
     */
    public void save(final Collection<GFERecord> records)
            throws DataAccessLayerException {
        // validate fields
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

        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory()
                    .openStatelessSession();
            tx = sess.beginTransaction();
            for (GFERecord rec : records) {
                // TODO: Update saving a record, currently causes 2 inserts and
                // 2 updates to happen, 1 for the record, 1 for the history,
                // updates the whole history, then updates parent reference.
                sess.insert(rec);
                for (GridDataHistory hist : rec.getGridHistory()) {
                    sess.insert(hist);
                }
            }
            tx.commit();
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException("Unable to save GFERecords: "
                    + records, e);
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

    /**
     * Update the GFERecords. Updating a record will update insert time only on
     * the record, update all existing histories, and insert all new histories.
     * 
     * @param existingRecords
     */
    public void update(final Collection<GFERecord> existingRecords)
            throws DataAccessLayerException {
        StatelessSession sess = null;
        Transaction tx = null;
        List<Integer> ids = new ArrayList<Integer>(existingRecords.size());
        for (GFERecord rec : existingRecords) {
            ids.add(rec.getId());
        }

        try {
            sess = getHibernateTemplate().getSessionFactory()
                    .openStatelessSession();
            tx = sess.beginTransaction();

            // Update insert time
            Query query = sess
                    .createQuery("UPDATE GFERecord SET insertTime = ? WHERE id IN (:ids)");
            query.setCalendar(0, Calendar.getInstance());
            query.setParameterList("ids", ids);

            query.executeUpdate();

            // TODO: do smart history diffing as most cases only a few columns
            // update, if done remove parent calling consolidate and just
            // passing existing through
            List<Integer> histToDelete = new ArrayList<Integer>();

            // handle histories
            for (GFERecord rec : existingRecords) {
                for (GridDataHistory hist : rec.getGridHistory()) {
                    if (hist.getId() != 0) {
                        sess.update(hist);
                    } else {
                        sess.insert(hist);
                    }
                }
                List<GridDataHistory> oldHists = rec.getOldHistory();
                if (!CollectionUtil.isNullOrEmpty(oldHists)) {
                    for (GridDataHistory oldHist : oldHists) {
                        histToDelete.add(oldHist.getId());
                    }
                }
            }

            if (!histToDelete.isEmpty()) {
                query = sess
                        .createQuery("DELETE FROM GridDataHistory WHERE id in (:ids)");
                query.setParameterList("ids", histToDelete);
                query.executeUpdate();
            }

            tx.commit();
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException("Unable to update GFERecords: "
                    + existingRecords, e);
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

    /**
     * Gets list of all database IDs currently being stored in the database for
     * a given site.
     * 
     * @param siteId
     *            The siteId to look up databases for.
     * 
     * @return The list of all database IDs currently being stored in the
     *         database
     */
    @SuppressWarnings("unchecked")
    public List<DatabaseID> getDatabaseInventory(final String siteId)
            throws DataAccessLayerException {
        // TODO: Should this be done from GridParmManager?
        try {
            return (List<DatabaseID>) txTemplate
                    .execute(new TransactionCallback() {
                        @Override
                        public List<DatabaseID> doInTransaction(
                                TransactionStatus status) {
                            return getHibernateTemplate().find(
                                    "FROM DatabaseID WHERE siteId = ?", siteId);
                        }
                    });
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to look up database inventory for site " + siteId,
                    e);
        }
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
    public List<GFERecord> queryByParmID(final ParmID parmId)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();

            // reattach so parmId doesn't requery
            sess.buildLockRequest(LockOptions.NONE).lock(parmId);

            Query query = sess.createQuery("FROM GFERecord WHERE parmId = ?");
            query.setParameter(0, parmId);
            List<GFERecord> list = query.list();
            tx.commit();
            return list;
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException(
                    "Unable to look up records for parmId " + parmId, e);
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

    /**
     * 
     * @param parmId
     * @param tr
     * @return
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public Map<TimeRange, GFERecord> getOverlappingRecords(final ParmID parmId,
            final TimeRange tr) throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;
        List<GFERecord> records = null;

        try {
            // stateless session so we can bulk query histories instead of once
            // per record via hibernate
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();

            // reattach so parmId doesn't requery
            sess.buildLockRequest(LockOptions.NONE).lock(parmId);

            // start and end specifically reversed as we want
            // every record that has a start before our end and
            // and an end after our start
            Query query = sess.createQuery("FROM GFERecord WHERE parmId = ?"
                    + " AND dataTime.validPeriod.start < ?"
                    + " AND dataTime.validPeriod.end > ?");
            query.setParameter(0, parmId);
            query.setTimestamp(1, tr.getEnd());
            query.setTimestamp(2, tr.getStart());
            records = query.list();
            tx.commit();
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException(
                    "Unable to look up records for parmId " + parmId
                            + " overlapping timeRange " + tr, e);
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

        Map<TimeRange, GFERecord> recordsByTimeRange = new HashMap<TimeRange, GFERecord>(
                records.size(), 1);
        for (GFERecord rec : records) {
            recordsByTimeRange.put(rec.getTimeRange(), rec);
        }

        return recordsByTimeRange;
    }

    /**
     * Deletes records that have the specified parmId and time range.
     * 
     * @param parmId
     * @param times
     */
    public void deleteRecords(final ParmID parmId, final List<TimeRange> times) {
        if (times.isEmpty()) {
            return;
        }

        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            Query query = sess
                    .createQuery("DELETE FROM GFERecord WHERE parmId = :parmId"
                            + " AND dataTime.validPeriod IN (:times)");
            query.setParameter("parmId", parmId);
            query.setParameterList("times", times);
            int rowsDeleted = query.executeUpdate();
            tx.commit();
            statusHandler.info("Deleted " + rowsDeleted
                    + " records from the database.");

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
                        statusHandler.handle(Priority.DEBUG, "Deleted: "
                                + Arrays.toString(groupsToDelete) + " from "
                                + hdf5File.getName());
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.WARN,
                            "Error deleting hdf5 record(s) from file: "
                                    + hdf5File.getPath(), e);
                }
            }
        } catch (Exception e) {
            statusHandler.error("Error deleting database record(s) for parmId "
                    + parmId + " timeRanges " + times, e);

            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }
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

    /**
     * Gets the list of times for a given parmId
     * 
     * @param parmId
     *            The id of the parm
     * @return The list of times for a given parm name and level
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<TimeRange> getTimes(final ParmID parmId)
            throws DataAccessLayerException {
        try {
            return (List<TimeRange>) txTemplate
                    .execute(new TransactionCallback() {
                        @Override
                        public List<TimeRange> doInTransaction(
                                TransactionStatus status) {
                            return getHibernateTemplate()
                                    .find("SELECT dataTime.validPeriod FROM GFERecord WHERE parmId = ? ORDER BY dataTime.validPeriod.start",
                                            parmId);
                        }
                    });
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unabled to look up times for parm id " + parmId, e);
        }
    }

    /**
     * Gets the list of times for a given parmId that overlap with the given
     * time range.
     * 
     * @param parmId
     *            The id of the parm
     * @param tr
     *            The time range to pull time ranges for
     * @return The list of times for a given parm name and level
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<TimeRange> getOverlappingTimes(final ParmID parmId,
            final TimeRange tr) throws DataAccessLayerException {
        try {
            return (List<TimeRange>) txTemplate
                    .execute(new TransactionCallback() {
                        @Override
                        public List<TimeRange> doInTransaction(
                                TransactionStatus status) {
                            List<TimeRange> rval = getHibernateTemplate()
                                    .find("SELECT dataTime.validPeriod"
                                            + " FROM GFERecord WHERE parmId = ?"
                                            + " AND dataTime.validPeriod.start < ?"
                                            + " AND dataTime.validPeriod.end > ?",
                                            new Object[] { parmId, tr.getEnd(),
                                                    tr.getStart() });
                            return rval;
                        }
                    });
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unabled to look up record inventory for parm id " + parmId,
                    e);
        }
    }

    /**
     * Retrieves the grid history for the specified parm and time ranges
     * 
     * @param parmId
     *            The parm id
     * @param trs
     *            The time ranges to search for
     * @return The grid histories
     * @throws DataAccessLayerException
     *             If problems during database interaction occur
     */
    public Map<TimeRange, List<GridDataHistory>> getGridHistory(
            final ParmID parmId, final List<TimeRange> trs)
            throws DataAccessLayerException {
        // TODO: This would be better of using a single time range to do the
        // select on
        Map<TimeRange, List<GridDataHistory>> history = new HashMap<TimeRange, List<GridDataHistory>>(
                trs.size(), 1);
        if (trs.isEmpty()) {
            return history;
        }

        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory()
                    .openStatelessSession();
            tx = sess.beginTransaction();

            Query query = sess
                    .createQuery("SELECT hist.parent.dataTime.validPeriod, hist "
                            + "FROM GridDataHistory hist WHERE hist.parent.parmId = ? AND hist.parent.dataTime.validPeriod IN (:periods) ORDER BY hist.id");
            query.setParameter(0, parmId);
            query.setParameterList("periods", trs);
            @SuppressWarnings("unchecked")
            List<Object[]> rows = query.list();
            tx.commit();

            for (Object[] cols : rows) {
                TimeRange tr = (TimeRange) cols[0];
                List<GridDataHistory> histForTime = history.get(tr);
                if (histForTime == null) {
                    histForTime = new ArrayList<GridDataHistory>(1);
                    history.put(tr, histForTime);
                }
                histForTime.add((GridDataHistory) cols[1]);
            }
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException(
                    "Unable to look up record inventory for parm id " + parmId,
                    e);
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

        return history;
    }

    public void purgeGFEGrids(final DatabaseID dbId) {
        delete(dbId);
    }

    /**
     * Removes GridParmInfo from the HDF5 file and any data associated with that
     * info
     * 
     * @param parmId
     *            The parm to delete data for
     * @throws DataAccessLayerException
     *             If errors occur
     */
    public void removeParmData(ParmID parmId) throws DataAccessLayerException {

        try {
            IDataStore ds = DataStoreFactory.getDataStore(GfeUtil
                    .getGridParmHdf5File(GridDatabase.gfeBaseDataDir,
                            parmId.getDbId()));
            ds.deleteDatasets("/GridParmInfo/" + parmId.getCompositeName(),
                    "/GridParmStorageInfo/" + parmId.getCompositeName());
        } catch (Exception e1) {
            throw new DataAccessLayerException("Error deleting data from HDF5",
                    e1);
        }
        List<TimeRange> trs = this.getTimes(parmId);
        this.deleteRecords(parmId, trs);
        // TODO: Remove all Locks??
    }

    /**
     * Removes any data associated with a given parm and the parm itself
     * 
     * @param parmId
     *            The parm to delete data for
     * @throws DataAccessLayerException
     *             If errors occur
     */
    public void removeParm(ParmID parmId) throws DataAccessLayerException {
        removeParmData(parmId);
        delete(parmId);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        return null;
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
        List<Integer> ids = new ArrayList<Integer>(history.size());
        for (GridDataHistory hist : history) {
            ids.add(hist.getId());
        }

        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            Query q = sess
                    .createSQLQuery("UPDATE gfe_gridhistory SET publishtime=:publishtime WHERE id IN (:ids)");
            q.setTimestamp("publishtime", publishTime);
            q.setParameterList("ids", ids);
            q.executeUpdate();
            tx.commit();
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
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

    /**
     * Updates the sent time for all histories of passed parmId during the
     * timeRange. The histories are then returned in a map by timeRange.
     * 
     * @param parmId
     * @param tr
     * @param sentTime
     * @return
     */
    @SuppressWarnings("unchecked")
    public Map<TimeRange, List<GridDataHistory>> updateSentTime(
            final ParmID parmId, TimeRange tr, Date sentTime)
            throws DataAccessLayerException {
        StatelessSession sess = null;
        Transaction tx = null;
        List<Object[]> rows = null;

        try {
            sess = getHibernateTemplate().getSessionFactory()
                    .openStatelessSession();
            tx = sess.beginTransaction();
            // use intersection of time range, UPDATE statement don't auto join
            // table so have to manually select the id
            Query query = sess
                    .createQuery("UPDATE GridDataHistory SET lastSentTime = ?"
                            + " WHERE parent.id in (SELECT id FROM GFERecord "
                            + " WHERE parmId = ?"
                            + " AND dataTime.validPeriod.start < ?"
                            + " AND dataTime.validPeriod.end > ?)");
            query.setTimestamp(0, sentTime);
            query.setParameter(1, parmId);
            query.setTimestamp(2, tr.getEnd());
            query.setTimestamp(3, tr.getStart());
            query.executeUpdate();

            // use intersection of time range
            query = sess
                    .createQuery("SELECT hist.parent.dataTime.validPeriod, hist "
                            + "FROM GridDataHistory hist"
                            + " WHERE hist.parent.parmId = ? AND hist.parent.dataTime.validPeriod.start < ?"
                            + " AND hist.parent.dataTime.validPeriod.end > ?");
            query.setParameter(0, parmId);
            query.setTimestamp(1, tr.getEnd());
            query.setTimestamp(2, tr.getStart());
            rows = query.list();
            tx.commit();
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException(
                    "Unable to look up record inventory for parm id " + parmId,
                    e);
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

        Map<TimeRange, List<GridDataHistory>> history = new HashMap<TimeRange, List<GridDataHistory>>(
                rows.size(), 1);

        for (Object[] cols : rows) {
            TimeRange curTr = (TimeRange) cols[0];
            List<GridDataHistory> histForTime = history.get(curTr);
            if (histForTime == null) {
                histForTime = new ArrayList<GridDataHistory>(1);
                history.put(curTr, histForTime);
            }
            histForTime.add((GridDataHistory) cols[1]);
        }

        return history;
    }

    public int deleteRecords(Collection<GFERecord> records)
            throws DataAccessLayerException {
        List<Integer> ids = new ArrayList<Integer>(records.size());
        for (GFERecord rec : records) {
            ids.add(rec.getId());
        }

        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            Query q = sess
                    .createQuery("DELETE FROM GFERecord WHERE id in (:ids)");
            q.setParameterList("ids", ids);
            int rowsDeleted = q.executeUpdate();
            tx.commit();
            return rowsDeleted;
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }
            throw new DataAccessLayerException("Error deleting records", e);
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

    @SuppressWarnings("unchecked")
    public Date getMaxInsertTimeByDbId(final DatabaseID dbId)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addQueryParam("parmId.dbId", getDatabaseId(dbId),
                QueryOperand.EQUALS);
        query.addReturnedField("insertTime");
        query.addOrder("insertTime", false);
        query.setMaxResults(1);

        List<Calendar> result = (List<Calendar>) this.queryByCriteria(query);
        if (!result.isEmpty()) {
            return result.get(0).getTime();
        } else {
            return null;
        }
    }

    @SuppressWarnings("unchecked")
    public DatabaseID getLatestDbIdByModelName(final String siteId,
            final String modelName) throws DataAccessLayerException {
        // TODO: Should this be done from GridParmManager?
        List<DatabaseID> results = Collections.emptyList();
        try {
            final String[] queryParams = { siteId, modelName };
            results = (List<DatabaseID>) txTemplate
                    .execute(new TransactionCallback() {
                        @Override
                        public List<DatabaseID> doInTransaction(
                                TransactionStatus status) {
                            return getHibernateTemplate()
                                    .find("FROM DatabaseID WHERE siteId = ? AND modelName = ? ORDER BY modelTime DESC LIMIT 1",
                                            queryParams);
                        }
                    });
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to look up database inventory for site " + siteId,
                    e);
        }

        if (!results.isEmpty()) {
            return results.get(0);
        } else {
            return null;
        }
    }
}
