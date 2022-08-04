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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.LockOptions;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;
import org.hibernate.query.Query;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Data access object for manipulating locks
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/17/08     #940       bphillip    Initial Creation
 * 04/19/13     #1949      rjpeter     Normalized GFE Database.
 * 06/20/13     #2127      rjpeter     Set session to read only.
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * 03/03/15      629    mgamazaychikov Add getAllLocks method.
 * 05/21/2019   6140       dgilling    Upgrading to Hibernate 5.
 * </pre>
 *
 * @author bphillip
 */
public class GFELockDao extends CoreDao {

    public GFELockDao() {
        super(DaoConfig.forClass(Lock.class));
    }

    /**
     * Gets locks for the provided list of ParmIDs. The locks are retrieved,
     * lock tables are constructed and assigned the provided workstation ID
     *
     * @param parmIds
     *            The database ParmIDs to get the lock tables for
     * @param wsId
     *            The workstation ID to assign to the lock tables
     * @return A map of the ParmID and its associated lock table
     * @throws DataAccessLayerException
     *             If errors occur during database interaction
     */
    public Map<ParmID, LockTable> getLocks(final Collection<ParmID> parmIds,
            WsId wsId) throws DataAccessLayerException {
        // Return if no parmIDs are provided
        if (parmIds.isEmpty()) {
            return Collections.emptyMap();
        }

        // The return variable
        Map<ParmID, LockTable> lockMap = new HashMap<>(
                parmIds.size(), 1);

        // create a blank lock table for each parmId ensuring all parms are
        // covered
        for (ParmID requiredParmId : parmIds) {
            lockMap.put(requiredParmId, new LockTable(requiredParmId,
                    new ArrayList<Lock>(), wsId));
        }

        Session sess = null;
        Transaction tx = null;

        try {
            sess = getSession();
            sess.setDefaultReadOnly(true);
            tx = sess.beginTransaction();

            // reattach object so any parmIds found don't requery
            for (ParmID requiredParmId : parmIds) {
                // Only safe because ParmID has no OneToMany or ManyToMany
                // relations
                sess.buildLockRequest(LockOptions.NONE).lock(requiredParmId);
            }

            Query<Lock> query = sess
                    .createQuery("FROM Lock l WHERE l.parmId IN (:parmIds)",
                            Lock.class);
            query.setParameterList("parmIds", parmIds);
            List<Lock> locks = query.list();
            tx.commit();

            // populate Lock table
            for (Lock lock : locks) {
                lockMap.get(lock.getParmId()).addLock(lock);
            }

            return lockMap;
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException(
                    "Unable to look up locks for parmIds " + parmIds, e);
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error(
                            "Error occurred closing database session", e);
                }
            }
        }
    }

    /**
     * Adds and removes the passed locks.
     *
     * @param locksToDelete
     *            The locks to delete
     * @param locksToAdd
     *            The locks to add
     */
    public void addRemoveLocks(final Collection<Lock> locksToAdd,
            final Collection<Integer> locksToDelete)
            throws DataAccessLayerException {
        StatelessSession s = null;
        Transaction tx = null;

        try {
            s = getSessionFactory()
                    .openStatelessSession();
            tx = s.beginTransaction();

            if (!CollectionUtil.isNullOrEmpty(locksToDelete)) {
                s.createQuery(
                        "DELETE FROM Lock l WHERE l.id IN (:locksToDelete)")
                        .setParameterList("locksToDelete", locksToDelete)
                        .executeUpdate();
            }

            if (!CollectionUtil.isNullOrEmpty(locksToAdd)) {
                for (Lock lock : locksToAdd) {
                    s.insert(lock);
                }
            }

            tx.commit();
        } catch (Throwable e) {
            tx.rollback();
            throw new DataAccessLayerException("Error combining locks", e);
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (Exception e) {
                    logger.error(
                            "Error occurred closing database session", e);
                }
            }
        }
    }

    public List<Lock> getAllLocks(final String siteId)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getSession();
            sess.setDefaultReadOnly(true);
            tx = sess.beginTransaction();
            Query<Lock> query = sess
                    .createQuery(
                            "FROM Lock l WHERE l.parmId in (SELECT p.id FROM ParmID p WHERE p.dbId in (SELECT d.id FROM DatabaseID d WHERE d.siteId = :siteId))",
                            Lock.class);
            query.setParameter("siteId", siteId);
            List<Lock> locks = query.list();
            tx.commit();
            return locks;
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e1);
                }
            }
            throw new DataAccessLayerException("Unable to look up locks for site " + siteId, e);
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
}
