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

import javax.persistence.PersistenceException;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.hibernate.LockOptions;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;
import org.hibernate.exception.ConstraintViolationException;
import org.hibernate.query.Query;
import org.hibernate.type.CalendarType;
import org.hibernate.type.TimestampType;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmStorageInfo;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.LockNotification;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.audit.DataStorageAuditUtils;
import com.raytheon.uf.common.datastorage.audit.MetadataStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Data access object for manipulating GFE Records
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 08, 2008  875      bphillip  Initial Creation
 * May 16, 2008  875      bphillip  Added D2D grib querying methods
 * Jun 17, 2008  940      bphillip  Implemented GFE Locking
 * Jun 17, 2009  2380     randerso  Removed purging of grid history. Should
 *                                  cascade when record deleted.
 * Aug 07, 2009  2763     njensen   Refactored queryByD2DParmId
 * Sep 10, 2012  15137    ryu       Changed for MOSGuide D2D mxt/mnt grids for
 *                                  consistency with A1.
 * Oct 10, 2012  1260     randerso  Added check to ensure db can be created
 *                                  before adding it to the inventory
 * Dec 06, 2012  1394     rjpeter   Optimized D2D grid access.
 * Jan 21, 2012  1504     randerso  Back ported change to use ParameterMapper
 *                                  into 13.1.2
 * Feb 10, 2013  1603     randerso  Eliminated unnecessary conversion from lists
 *                                  to arrays
 * Feb 12, 2013  1608     randerso  Changed to use explicit deletes for groups
 *                                  and datasets
 * Mar 15, 2013  1795     njensen   Added updatePublishTime()
 * Mar 21, 2013  1774     randerso  Moved D2D routines into GFED2DDao
 * Apr 08, 2013  1949     rjpeter   Normalized GFE Database.
 * May 22, 2013  2025     dgilling  Re-implement functions needed by
 *                                  GetLatestDbTimeRequest and
 *                                  GetLatestModelDbIdRequest.
 * May 20, 2013  2127     rjpeter   Set session's to read only and switched to
 *                                  stateless where possible.
 * Jun 13, 2013  2044     randerso  Refactored to use IFPServer, code cleanup
 * Jul 30, 2013  2057     randerso  Added support marking and eventually purging
 *                                  obsolete databases
 * Aug 08, 2013  16485    ryu       Remove call to getDatabaseId() from
 *                                  getMaxInsertTimeByDbId() so new GFE
 *                                  databases aren't accidentally created.
 * Aug 05, 2013  1571     randerso  Added support for storing GridLocation and
 *                                  ParmStorageInfo in database
 * Sep 30, 2013  2147     rferrel   Changes to archive hdf5 files.
 * Oct 15, 2013  2446     randerso  Added ORDER BY clause to getOverlappingTimes
 * Jun 12, 2014  3244     randerso  Improved error handling
 * Sep 21, 2014  3648     randerso  Changed to do version purging when new
 *                                  databases are added
 * Oct 16, 2014  3454     bphillip  Upgrading to Hibernate 4
 * Apr 28, 2015  17435    randerso  Fix getLatestDbIdByModelName().
 * Aug 10, 2015  1574     nabowle   Override getMinRefTime to ignore Topo
 *                                  Databases
 * Aug 21, 2019  6140     dgilling  Upgrading to Hibernate 5.
 * Mar 25, 2020  8103     randerso  Fixed ContraintViolationException handling
 * Sep 23, 2021  8608     mapeters  Audit metadata storage status
 * Jan 18, 2022  8740     randerso  Improve exception handling
 * Feb 16, 2022  8608     mapeters  Use DataStorageAuditUtils
 * Jun 22, 2022  8865     mapeters  Update populateDataStore to return boolean
 *
 * </pre>
 */
public class GFEDao extends DefaultPluginDao {
    /** Removed DB purge time in days */
    public static final int REMOVED_DB_PURGE_TIME = 7;

    // Number of retries on insert of a new DatabaseID
    private static final int QUERY_RETRY = 2;

    /**
     * @throws PluginException
     */
    public GFEDao() throws PluginException {
        super("gfe");
        this.pathProvider = new GFEPathProvider();
    }

    /**
     * Creates a new GFE Dao
     *
     * @param pluginName
     *
     * @throws PluginException
     */
    public GFEDao(String pluginName) throws PluginException {
        super(pluginName);
        this.pathProvider = new GFEPathProvider();
    }

    /**
     * Returns the database row for the passed dbId. If the row does not exist,
     * the row will be created.
     *
     * @param dbId
     * @return a DatabaseID with id field initialized
     * @throws DataAccessLayerException
     */
    public DatabaseID getDatabaseId(DatabaseID dbId)
            throws DataAccessLayerException {
        DatabaseID rval = null;
        Session sess = null;

        try {
            sess = getSession();
            sess.setDefaultReadOnly(true);
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
                } catch (PersistenceException e) {
                    if (e.getCause() instanceof ConstraintViolationException) {
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
                        /*
                         * database may have been inserted on another process,
                         * redo the look up
                         */
                        if (tries < 2) {
                            logger.info(
                                    "Constraint violation on save, attempting to look up database id again");
                        } else {
                            throw new DataAccessLayerException(
                                    "Unable to look up DatabaseID: "
                                            + dbId.toString(),
                                    e);
                        }
                    } else {
                        throw e;
                    }
                }
            }
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to look up DatabaseID: " + dbId.toString(), e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
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
        CriteriaBuilder cb = sess.getCriteriaBuilder();
        CriteriaQuery<DatabaseID> criteriaQuery = cb
                .createQuery(DatabaseID.class);
        Root<DatabaseID> root = criteriaQuery.from(DatabaseID.class);
        criteriaQuery.select(root)
                .where(cb.and(cb.equal(root.get("siteId"), dbId.getSiteId()),
                        cb.equal(root.get("modelTime"), dbId.getModelTime()),
                        cb.equal(root.get("modelName"), dbId.getModelName()),
                        cb.equal(root.get("dbType"), dbId.getDbType())));
        Query<DatabaseID> query = sess.createQuery(criteriaQuery);
        return query.uniqueResult();
    }

    /**
     * Retrieves ParmStorageInfo for all known ParmIDs for the given DatabaseID.
     *
     * @param dbId
     * @return the list of ParmStorageInfo for the database
     * @throws DataAccessLayerException
     */
    public List<ParmStorageInfo> getParmStorageInfo(final DatabaseID dbId)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getSession();
            sess.setDefaultReadOnly(true);
            tx = sess.beginTransaction();

            /*
             * reattach so dbId doesn't requery
             *
             * Only safe because DatabaseID has no OneToMany or ManyToMany
             * relations
             */
            sess.buildLockRequest(LockOptions.NONE).lock(dbId);

            Query<ParmStorageInfo> query = sess.createQuery(
                    "FROM ParmStorageInfo psi WHERE psi.gridParmInfo.parmID.dbId = :dbId",
                    ParmStorageInfo.class);
            query.setParameter("dbId", dbId);
            List<ParmStorageInfo> list = query.list();
            tx.commit();

            // initialize the grid location objects
            for (ParmStorageInfo psi : list) {
                psi.getGridParmInfo().getGridLoc().init();
            }
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
                    "Unable to retrieve ParmStorageInfos for DatabaseID: "
                            + dbId,
                    e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Stores a list of ParmStorageInfo
     *
     * @param psiList
     * @throws DataAccessLayerException
     */
    public void saveParmStorageInfo(List<ParmStorageInfo> psiList)
            throws DataAccessLayerException {

        if (psiList.isEmpty()) {
            return;
        }

        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();

            for (ParmStorageInfo psi : psiList) {
                sess.insert(psi);
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

            throw new DataAccessLayerException(
                    "Unable to save ParmStorageInfos for "
                            + psiList.get(0).getParmID().getDbId(),
                    e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Update a ParmStorageInfo
     *
     * @param psi
     * @throws DataAccessLayerException
     */
    public void updateParmStorageInfo(ParmStorageInfo psi)
            throws DataAccessLayerException {

        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();
            sess.update(psi);
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
                    "Unable to update ParmStorageInfos for " + psi.getParmID(),
                    e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Returns the database row for the passed parmId. If the row does not
     * exist, the row will be created.
     *
     * @param parmId
     * @return the ParmID from the database with id field initialized
     * @throws DataAccessLayerException
     */
    public ParmID getParmId(final ParmID parmId)
            throws DataAccessLayerException {
        ParmID rval = null;
        Session sess = null;

        try {
            sess = getSession();
            sess.setDefaultReadOnly(true);

            /*
             * reattach so dbId doesn't requery
             *
             * Only safe because DatabaseID has no OneToMany or ManyToMany
             * relations
             */
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
                } catch (PersistenceException e) {
                    if (e.getCause() instanceof ConstraintViolationException) {
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
                        /*
                         * database may have been inserted on another process,
                         * redo the look up
                         */
                        if (tries < 2) {
                            logger.info(
                                    "Constraint violation on save, attempting to look up parm id again");
                        } else {
                            throw new DataAccessLayerException(
                                    "Unable to look up ParmID: "
                                            + parmId.toString(),
                                    e);
                        }
                    } else {
                        throw e;
                    }
                }
            }
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to look up ParmID: " + parmId.toString(), e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
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
     * @param parmId
     * @return
     */
    private ParmID getParmId(Session sess, ParmID parmId) {
        CriteriaBuilder cb = sess.getCriteriaBuilder();
        CriteriaQuery<ParmID> criteriaQuery = cb.createQuery(ParmID.class);
        Root<ParmID> root = criteriaQuery.from(ParmID.class);
        criteriaQuery.select(root).where(cb.and(
                cb.equal(root.get("dbId"), parmId.getDbId()),
                cb.equal(root.get("parmName"), parmId.getParmName()),
                cb.equal(root.get("parmLevel"), parmId.getParmLevel())));
        Query<ParmID> query = sess.createQuery(criteriaQuery);
        return query.uniqueResult();
    }

    @Override
    public void purgeExpiredData() throws PluginException {
        List<IFPServer> ifpServers = IFPServer.getActiveServers();
        for (IFPServer ifpServer : ifpServers) {
            List<GridUpdateNotification> gridNotifcations = new ArrayList<>();
            List<LockNotification> lockNotifications = new ArrayList<>();

            try {
                GridParmManager gridParmMgr = ifpServer.getGridParmMgr();

                PurgeLogger.logInfo("Purging expired grids...", "gfe");
                ServerResponse<?> sr = gridParmMgr.gridsPurge(gridNotifcations,
                        lockNotifications);
                if (!sr.isOkay()) {
                    PurgeLogger.logError(sr.message(), "gfe");
                }

                PurgeLogger.logInfo(
                        "Purging Expired pending isc send requests...", "gfe");
                int requestsPurged = new IscSendRecordDao()
                        .purgeExpiredPending();
                PurgeLogger.logInfo("Purged " + requestsPurged
                        + " expired pending isc send requests.", "gfe");
                purgeRemovedDbs();
            } catch (DataAccessLayerException e) {
                throw new PluginException(
                        "Error purging expired send ISC records!", e);
            } finally {
                SendNotifications.send(gridNotifcations);
                SendNotifications.send(lockNotifications);
            }
        }
    }

    private void purgeRemovedDbs() throws DataAccessLayerException {
        List<DatabaseID> removed = null;
        try {
            removed = txTemplate
                    .execute(new TransactionCallback<List<DatabaseID>>() {
                        @SuppressWarnings("unchecked")
                        @Override
                        public List<DatabaseID> doInTransaction(
                                TransactionStatus status) {
                            Date purgeDate = new Date(System.currentTimeMillis()
                                    - (REMOVED_DB_PURGE_TIME
                                            * TimeUtil.MILLIS_PER_DAY));
                            return getCurrentSession().createQuery(
                                    "FROM DatabaseID where removedDate < :removedDate")
                                    .setParameter("removedDate", purgeDate)
                                    .list();
                        }
                    });
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Error purging removed databases", e);
        }

        if (removed != null) {
            for (DatabaseID dbId : removed) {
                IFPGridDatabase.deleteDatabase(dbId);
                PurgeLogger.logInfo("Purging removed database: " + dbId, "gfe");
            }
        }

    }

    /**
     * Purge all DatabaseIDs for a site
     *
     * @param siteID
     * @return number of rows purged
     * @throws DataAccessLayerException
     */
    public int purgeDatabaseForSite(final String siteID)
            throws DataAccessLayerException {
        return txTemplate.execute(status -> getCurrentSession()
                .createQuery("DELETE FROM DatabaseID WHERE siteId = :siteId")
                .setParameter("siteId", siteID).executeUpdate());
    }

    /**
     *
     * @param records
     * @throws DataAccessLayerException
     */
    public void save(final Collection<GFERecord> records)
            throws DataAccessLayerException {
        // validate fields
        for (GFERecord rec : records) {
            if (rec.getInsertTime() == null) {
                rec.setInsertTime(Calendar.getInstance());
            }
        }

        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();
            for (GFERecord rec : records) {
                /*
                 * TODO: Update saving a record, currently causes 2 inserts and
                 * 2 updates to happen, 1 for the record, 1 for the history,
                 * updates the whole history, then updates parent reference.
                 */
                sess.insert(rec);
                for (GridDataHistory hist : rec.getGridHistory()) {
                    sess.insert(hist);
                }
            }
            tx.commit();

            DataStorageAuditUtils.auditMetadataStatuses(MetadataStatus.SUCCESS,
                    records);
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            DataStorageAuditUtils.auditMetadataStatuses(MetadataStatus.FAILURE,
                    records);

            throw new DataAccessLayerException(
                    "Unable to save GFERecords: " + records, e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Update the GFERecords. Updating a record will update insert time only on
     * the record, update all existing histories, and insert all new histories.
     *
     * @param existingRecords
     * @throws DataAccessLayerException
     */
    public void update(final Collection<GFERecord> existingRecords)
            throws DataAccessLayerException {
        StatelessSession sess = null;
        Transaction tx = null;
        List<Integer> ids = new ArrayList<>(existingRecords.size());
        for (GFERecord rec : existingRecords) {
            ids.add(rec.getId());
        }

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();

            // Update insert time
            sess.createQuery(
                    "UPDATE GFERecord r SET r.insertTime = :insertTime WHERE r.id IN (:ids)")
                    .setParameter("insertTime", TimeUtil.newGmtCalendar(),
                            CalendarType.INSTANCE)
                    .setParameterList("ids", ids).executeUpdate();

            /*
             * TODO: do smart history diffing as most cases only a few columns
             * update, if done remove parent calling consolidate and just
             * passing existing through
             */
            List<Integer> histToDelete = new ArrayList<>();

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
                sess.createQuery(
                        "DELETE FROM GridDataHistory h WHERE h.id in (:ids)")
                        .setParameterList("ids", histToDelete).executeUpdate();
            }

            tx.commit();

            DataStorageAuditUtils.auditMetadataStatuses(MetadataStatus.SUCCESS,
                    existingRecords);
        } catch (Exception e) {
            logger.error("Error occurred during update", e);
            if (tx != null && tx.isActive()) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            DataStorageAuditUtils.auditMetadataStatuses(MetadataStatus.FAILURE,
                    existingRecords);

            throw new DataAccessLayerException(
                    "Unable to update GFERecords: " + existingRecords, e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
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
     * @throws DataAccessLayerException
     */
    public List<DatabaseID> getDatabaseInventory(final String siteId)
            throws DataAccessLayerException {
        try {
            return txTemplate.execute((status) -> {
                return getCurrentSession().createQuery(
                        "FROM DatabaseID d WHERE d.siteId = :siteId AND d.removedDate is null",
                        DatabaseID.class).setParameter("siteId", siteId).list();
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
    public List<GFERecord> queryByParmID(final ParmID parmId)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getSession();
            sess.setDefaultReadOnly(true);
            tx = sess.beginTransaction();

            /*
             * reattach so parmId doesn't requery
             *
             * Only safe because ParmID has no OneToMany or ManyToMany relations
             */
            sess.buildLockRequest(LockOptions.NONE).lock(parmId);

            CriteriaBuilder cb = sess.getCriteriaBuilder();
            CriteriaQuery<GFERecord> criteriaQuery = cb
                    .createQuery(GFERecord.class);
            Root<GFERecord> root = criteriaQuery.from(GFERecord.class);
            criteriaQuery.select(root)
                    .where(cb.equal(root.get("parmId"), parmId));
            Query<GFERecord> query = sess.createQuery(criteriaQuery);
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
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Get all GFERecords whose time ranges overlap the specificed time range
     *
     * @param parmId
     * @param tr
     * @return map of TimeRanges to GFERecords
     * @throws DataAccessLayerException
     */
    public Map<TimeRange, GFERecord> getOverlappingRecords(final ParmID parmId,
            final TimeRange tr) throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;
        List<GFERecord> records = null;

        try {
            /*
             * stateless session so we can bulk query histories instead of once
             * per record via hibernate
             */
            sess = getSession();
            sess.setDefaultReadOnly(true);
            tx = sess.beginTransaction();

            /*
             * reattach so parmId doesn't requery
             *
             * Only safe because ParmID has no OneToMany or ManyToMany relations
             */
            sess.buildLockRequest(LockOptions.NONE).lock(parmId);

            /*
             * start and end specifically reversed as we want every record that
             * has a start before our end and and an end after our start
             */
            Query<GFERecord> query = sess.createQuery(
                    "FROM GFERecord r WHERE r.parmId = :parmId"
                            + " AND r.dataTime.validPeriod.start < :startTime"
                            + " AND r.dataTime.validPeriod.end > :endTime",
                    GFERecord.class);
            query.setParameter("parmId", parmId);
            query.setParameter("startTime", tr.getEnd(),
                    TimestampType.INSTANCE);
            query.setParameter("endTime", tr.getStart(),
                    TimestampType.INSTANCE);
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
                            + " overlapping timeRange " + tr,
                    e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }

        Map<TimeRange, GFERecord> recordsByTimeRange = new HashMap<>(
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
    public void deleteRecords(final ParmID parmId,
            final List<TimeRange> times) {
        if (times.isEmpty()) {
            return;
        }

        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();
            Query<?> query = sess
                    .createQuery("DELETE FROM GFERecord WHERE parmId = :parmId"
                            + " AND dataTime.validPeriod IN (:times)");
            query.setParameter("parmId", parmId);
            query.setParameterList("times", times);
            int rowsDeleted = query.executeUpdate();
            tx.commit();
            tx = null;
            logger.info(
                    "Deleted " + rowsDeleted + " records from the database.");

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

                    if (logger.isPriorityEnabled(Priority.DEBUG)) {
                        logger.handle(Priority.DEBUG,
                                "Deleted: " + Arrays.toString(groupsToDelete)
                                        + " from " + hdf5File.getName());
                    }
                } catch (Exception e) {
                    logger.handle(Priority.WARN,
                            "Error deleting hdf5 record(s) from file: "
                                    + hdf5File.getPath(),
                            e);
                }
            }
        } catch (Exception e) {
            logger.error("Error deleting database record(s) for parmId "
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
                    logger.error("Error occurred closing database session", e);
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
    public List<TimeRange> getTimes(final ParmID parmId)
            throws DataAccessLayerException {
        try {
            return txTemplate.execute((status) -> {
                return getCurrentSession().createQuery(
                        "SELECT r.dataTime.validPeriod FROM GFERecord r WHERE r.parmId = :parmId ORDER BY r.dataTime.validPeriod.start",
                        TimeRange.class).setParameter("parmId", parmId).list();
            });
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to look up times for parm id " + parmId, e);
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
    public List<TimeRange> getOverlappingTimes(final ParmID parmId,
            final TimeRange tr) throws DataAccessLayerException {
        try {
            return txTemplate.execute((status) -> {
                Query<TimeRange> query = getCurrentSession().createQuery(
                        "SELECT dataTime.validPeriod"
                                + " FROM GFERecord WHERE parmId = :parmId"
                                + " AND dataTime.validPeriod.start < :start"
                                + " AND dataTime.validPeriod.end > :end"
                                + " ORDER BY dataTime.validPeriod.start",
                        TimeRange.class);
                query.setParameter("parmId", parmId);
                query.setParameter("start", tr.getEnd());
                query.setParameter("end", tr.getStart());
                return query.list();
            });
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to look up record inventory for parm id " + parmId,
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
        /*
         * TODO: This would be better of using a single time range to do the
         * select on
         */
        Map<TimeRange, List<GridDataHistory>> history = new HashMap<>(
                trs.size(), 1);
        if (trs.isEmpty()) {
            return history;
        }

        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();

            Query<Object[]> query = sess.createQuery(
                    "SELECT hist.parent.dataTime.validPeriod, hist "
                            + "FROM GridDataHistory hist WHERE hist.parent.parmId = :parmId AND hist.parent.dataTime.validPeriod IN (:periods) ORDER BY hist.id",
                    Object[].class);
            query.setParameter("parmId", parmId);
            query.setParameterList("periods", trs);
            List<Object[]> rows = query.list();
            tx.commit();

            for (Object[] cols : rows) {
                TimeRange tr = (TimeRange) cols[0];
                List<GridDataHistory> histForTime = history.get(tr);
                if (histForTime == null) {
                    histForTime = new ArrayList<>(1);
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
                    logger.error("Error occurred closing database session", e);
                }
            }
        }

        return history;
    }

    /**
     * Remove all GFE records for a particular DatabaseID
     *
     * @param dbId
     *            database to be purged
     * @return true if database was removed, false if not found (already
     *         removed)
     */
    public boolean purgeGFEGrids(final DatabaseID dbId) {
        Session sess = null;
        boolean purged = false;
        try {
            sess = getSessionFactory().openSession();
            Transaction tx = sess.beginTransaction();
            Object toDelete = sess.get(DatabaseID.class, dbId.getId(),
                    LockOptions.UPGRADE);

            if (toDelete != null) {
                sess.delete(toDelete);
            }

            tx.commit();
            purged = true;
        } catch (Exception e) {
            logger.error("Error purging " + dbId, e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
        return purged;
    }

    /**
     * Removes all grids associated with a parm
     *
     * @param parmId
     *            The parm to delete data for
     * @throws DataAccessLayerException
     *             If errors occur
     */
    public void removeParmData(ParmID parmId) throws DataAccessLayerException {
        List<TimeRange> trs = this.getTimes(parmId);
        this.deleteRecords(parmId, trs);
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
    protected boolean populateDataStore(IDataStore dataStore, IPersistable obj)
            throws Exception {
        /*
         * Override to prevent auditing that is done in the super method and is
         * intended for database-only records. GFE records aren't database-only;
         * the HDF5 storage is just handled differently.
         */
        return false;
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
        List<Integer> ids = new ArrayList<>(history.size());
        for (GridDataHistory hist : history) {
            ids.add(hist.getId());
        }

        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();
            sess.createQuery(
                    "UPDATE GridDataHistory h SET h.publishTime=:publishtime WHERE h.id in (:ids)")
                    .setParameter("publishtime", publishTime,
                            TimestampType.INSTANCE)
                    .setParameterList("ids", ids).executeUpdate();
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
                    logger.error("Error occurred closing database session", e);
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
     * @return the histories that were updated
     * @throws DataAccessLayerException
     */
    public Map<TimeRange, List<GridDataHistory>> updateSentTime(
            final ParmID parmId, TimeRange tr, Date sentTime)
            throws DataAccessLayerException {
        StatelessSession sess = null;
        Transaction tx = null;
        List<Object[]> rows = null;

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();
            /*
             * use intersection of time range,
             *
             * UPDATE statement don't auto join table so have to manually select
             * the id
             */
            sess.createQuery(
                    "UPDATE GridDataHistory h SET h.lastSentTime = :sentTime "
                            + " WHERE h.parent.id in (SELECT id FROM GFERecord r "
                            + " WHERE r.parmId = :parmId"
                            + " AND r.dataTime.validPeriod.start < :startTime"
                            + " AND r.dataTime.validPeriod.end > :endTime)")
                    .setParameter("sentTime", sentTime, TimestampType.INSTANCE)
                    .setParameter("parmId", parmId)
                    .setParameter("startTime", tr.getEnd(),
                            TimestampType.INSTANCE)
                    .setParameter("endTime", tr.getStart(),
                            TimestampType.INSTANCE)
                    .executeUpdate();

            // use intersection of time range
            Query<Object[]> query = sess.createQuery(
                    "SELECT hist.parent.dataTime.validPeriod, hist "
                            + "FROM GridDataHistory hist"
                            + " WHERE hist.parent.parmId = :parmId AND hist.parent.dataTime.validPeriod.start < :startTime"
                            + " AND hist.parent.dataTime.validPeriod.end > :endTime",
                    Object[].class);
            query.setParameter("parmId", parmId);
            query.setParameter("startTime", tr.getEnd(),
                    TimestampType.INSTANCE);
            query.setParameter("endTime", tr.getStart(),
                    TimestampType.INSTANCE);
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
                    logger.error("Error occurred closing database session", e);
                }
            }
        }

        Map<TimeRange, List<GridDataHistory>> history = new HashMap<>(
                rows.size(), 1);

        for (Object[] cols : rows) {
            TimeRange curTr = (TimeRange) cols[0];
            List<GridDataHistory> histForTime = history.get(curTr);
            if (histForTime == null) {
                histForTime = new ArrayList<>(1);
                history.put(curTr, histForTime);
            }
            histForTime.add((GridDataHistory) cols[1]);
        }

        return history;
    }

    /**
     * Delete a list of records from the database
     *
     * @param records
     * @return number of records deleted
     * @throws DataAccessLayerException
     */
    public int deleteRecords(Collection<GFERecord> records)
            throws DataAccessLayerException {
        List<Integer> ids = new ArrayList<>(records.size());
        for (GFERecord rec : records) {
            ids.add(rec.getId());
        }

        StatelessSession sess = null;
        Transaction tx = null;

        try {
            sess = getSessionFactory().openStatelessSession();
            tx = sess.beginTransaction();
            Query<?> q = sess
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
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Return the latest insert time for a database
     *
     * @param dbId
     * @return latest insert time or null if no database has no records
     * @throws DataAccessLayerException
     */
    public Date getMaxInsertTimeByDbId(final DatabaseID dbId)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addQueryParam("parmId.dbId.siteId", dbId.getSiteId(),
                QueryOperand.EQUALS);
        query.addQueryParam("parmId.dbId.dbType", dbId.getDbType(),
                QueryOperand.EQUALS);
        query.addQueryParam("parmId.dbId.modelName", dbId.getModelName(),
                QueryOperand.EQUALS);
        query.addQueryParam("parmId.dbId.modelTime", dbId.getModelTime(),
                QueryOperand.EQUALS);
        query.addReturnedField("insertTime");
        query.addOrder("insertTime", false);
        query.setMaxResults(1);

        @SuppressWarnings("unchecked")
        List<Calendar> result = (List<Calendar>) this.queryByCriteria(query);
        if (!result.isEmpty()) {
            return result.get(0).getTime();
        } else {
            return null;
        }
    }

    /**
     * Find DatabaseID of latest model run
     *
     * @param siteId
     * @param modelName
     *            the name of the desired model
     * @return the DatabaseID or null if none found
     * @throws DataAccessLayerException
     */
    public DatabaseID getLatestDbIdByModelName(final String siteId,
            final String modelName) throws DataAccessLayerException {
        // TODO: Should this be done from GridParmManager?
        List<DatabaseID> results = Collections.emptyList();
        try {
            results = txTemplate.execute((status) -> {
                CriteriaBuilder cb = getCurrentSession().getCriteriaBuilder();
                CriteriaQuery<DatabaseID> criteriaQuery = cb
                        .createQuery(DatabaseID.class);
                Root<DatabaseID> root = criteriaQuery.from(DatabaseID.class);
                criteriaQuery.select(root)
                        .where(cb.and(cb.equal(root.get("siteId"), siteId),
                                cb.equal(root.get("modelName"), modelName)))
                        .orderBy(cb.desc(root.get("modelTime")));
                Query<DatabaseID> query = getCurrentSession()
                        .createQuery(criteriaQuery);
                query.setMaxResults(1);
                return query.list();
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

    /**
     * Set the database removed date
     *
     * @param dbId
     *            databaseID to be updated
     * @param removedDate
     *            date database was removed or null if not removed (restored)
     * @throws DataAccessLayerException
     */
    public void setDatabaseRemovedDate(DatabaseID dbId, Date removedDate)
            throws DataAccessLayerException {

        dbId.setRemovedDate(removedDate);
        Session sess = null;
        try {
            sess = getSession();
            int tries = 0;
            Transaction tx = null;
            try {
                tx = sess.beginTransaction();
                sess.update(dbId);
                tx.commit();

            } catch (PersistenceException e) {
                if (e.getCause() instanceof ConstraintViolationException) {
                    if (tx != null) {
                        try {
                            tx.rollback();
                        } catch (Exception e1) {
                            logger.error(
                                    "Error occurred rolling back transaction",
                                    e1);
                        }
                    }

                    /*
                     * database may have been inserted on another process, redo
                     * the look up
                     */
                    if (tries < 2) {
                        logger.info(
                                "Constraint violation on save, attempting to look up database id again");
                    } else {
                        throw new DataAccessLayerException(
                                "Unable to look up DatabaseID: "
                                        + dbId.toString(),
                                e);
                    }
                } else {
                    throw e;
                }
            }
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to look up DatabaseID: " + dbId.toString(), e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Save or update a GridLocation object
     *
     * @param gloc
     *            the GridLocation object
     * @throws DataAccessLayerException
     */
    public void saveOrUpdateGridLocation(GridLocation gloc)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getSession();
            tx = sess.beginTransaction();
            sess.saveOrUpdate(gloc);
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
                    "Unable to save GridLocation for " + gloc.getSiteId(), e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing database session", e);
                }
            }
        }
    }

    @Override
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
        // Topo records have refTime=1970-01-01 00:00:00, which we can ignore
        query.addQueryParam("parmId.parmName", "Topo", QueryOperand.NOTEQUALS);

        query.addOrder(PURGE_VERSION_FIELD, true);
        query.setMaxResults(1);
        List<Date> result = (List<Date>) this.queryByCriteria(query);
        if (result.isEmpty()) {
            return null;
        } else {
            return result.get(0);
        }
    }
}
