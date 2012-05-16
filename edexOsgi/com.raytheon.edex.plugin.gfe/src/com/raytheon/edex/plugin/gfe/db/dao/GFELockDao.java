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

import org.hibernate.Session;
import org.hibernate.Transaction;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Data access object for manipulating locks
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/17/08     #940       bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GFELockDao extends CoreDao {

    public GFELockDao() {
        super(DaoConfig.forClass(Lock.class));
    }

    /**
     * Gets all locks held by a specified user
     * 
     * @param wsId
     *            The workstation ID of the user
     * @return All locks held by a specified user
     * @throws DataAccessLayerException
     *             If database errors occur
     */
    @SuppressWarnings("unchecked")
    public List<Lock> getLocksByOwner(String wsId)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(daoClass.getName());
        query.addQueryParam("wsId", wsId);
        List<Lock> locks = (List<Lock>) queryByCriteria(query);
        return locks;
    }

    /**
     * Gets all locks in the specified time range
     * 
     * @param parmId
     *            The parmId of the locks
     * @param timeRange
     *            The time range to examine
     * @return All locks in the specified time range
     * @throws DataAccessLayerException
     *             If database errors occur
     */
    @SuppressWarnings("unchecked")
    public List<Lock> getLocksInRange(ParmID parmId, TimeRange timeRange)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(daoClass.getName());
        query.addQueryParam("startTime", timeRange.getStart().getTime(),
                QueryOperand.GREATERTHANEQUALS);
        query.addQueryParam("endTime", timeRange.getEnd().getTime(),
                QueryOperand.LESSTHANEQUALS);
        query.addQueryParam("parmId", parmId);
        List<Lock> locks = (List<Lock>) queryByCriteria(query);
        return locks;
    }

    /**
     * Gets a specific lock
     * 
     * @param parmId
     *            The parmId of the lock
     * @param timeRange
     *            The time range of the lock
     * @param wsId
     *            The workstation ID of the lock holder
     * @return A specific lock
     * @throws DataAccessLayerException
     *             If database errors occur
     */
    @SuppressWarnings("unchecked")
    public Lock getLock(ParmID parmId, TimeRange timeRange, WsId wsId)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(daoClass.getName());
        query.addQueryParam("startTime", timeRange.getStart().getTime());
        query.addQueryParam("endTime", timeRange.getEnd().getTime());
        query.addQueryParam("parmId", parmId);
        List<Lock> locks = (List<Lock>) queryByCriteria(query);

        if (locks.isEmpty()) {
            logger.info("No locks returned for -- ParmID: " + parmId
                    + " TimeRange: " + timeRange + " wsId: " + wsId);
            return null;
        } else if (locks.size() > 1) {
            logger.info("Duplicate locks detected for -- ParmID: " + parmId
                    + " TimeRange: " + timeRange + " wsId: " + wsId);
            return locks.get(0);
        } else {
            return locks.get(0);
        }
    }

    /**
     * Gets locks for the provided list of ParmIDs. The locks are retrieved,
     * lock tables are constructed and assigned the provided workstation ID
     * 
     * @param parmIds
     *            The ParmIDs to get the lock tables for
     * @param wsId
     *            The workstation ID to assign to the lock tables
     * @return A map of the ParmID and its associated lock table
     * @throws DataAccessLayerException
     *             If errors occur during database interaction
     */
    @SuppressWarnings("unchecked")
    public Map<ParmID, LockTable> getLocks(List<ParmID> parmIds, WsId wsId)
            throws DataAccessLayerException {
        // The return variable
        Map<ParmID, LockTable> lockMap = new HashMap<ParmID, LockTable>();

        // Variable to hold the results of the lock table query
        List<Lock> queryResult = null;

        // Return if no parmIDs are provided
        if (parmIds.isEmpty()) {
            return Collections.emptyMap();
        }

        DatabaseQuery query = new DatabaseQuery(daoClass.getName());
        query.addQueryParam("parmId", parmIds, QueryOperand.IN);
        queryResult = (List<Lock>) queryByCriteria(query);

        ParmID lockParmID = null;
        for (Lock lock : queryResult) {
            lockParmID = lock.getParmId();
            LockTable lockTable = lockMap.get(lockParmID);
            if (lockTable == null) {
                lockTable = new LockTable(lockParmID, new ArrayList<Lock>(),
                        wsId);
                lockMap.put(lockParmID, lockTable);
            }
            lockTable.addLock(lock);
        }
        /*
         * Do a check to make sure all required lock tables are present in the
         * map
         */
        if (parmIds != null) {
            for (ParmID requiredParmId : parmIds) {
                if (!lockMap.containsKey(requiredParmId)) {
                    lockMap.put(requiredParmId, new LockTable(requiredParmId,
                            new ArrayList<Lock>(0), wsId));
                }
            }
        }
        return lockMap;
    }

    /**
     * Updates additions and deletions to the lock table in a single transaction
     * 
     * @param locksToDelete
     *            The locks to delete
     * @param locksToAdd
     *            The locks to add
     */
    public void updateCombinedLocks(Collection<Lock> locksToDelete,
            Collection<Lock> locksToAdd) throws DataAccessLayerException {
        if (!locksToDelete.isEmpty() || !locksToAdd.isEmpty()) {
            Session s = this.getHibernateTemplate().getSessionFactory()
                    .openSession();
            Transaction tx = s.beginTransaction();
            try {
                for (Lock lock : locksToAdd) {
                    s.save(lock);
                }
                for (Lock lock : locksToDelete) {
                    s.delete(lock);
                }
                tx.commit();
            } catch (Throwable e) {
                tx.rollback();
                throw new DataAccessLayerException("Error combining locks", e);
            } finally {
                if (s != null) {
                    s.close();
                }
            }
        }
    }
}
