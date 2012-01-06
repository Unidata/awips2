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
import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
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
     * Gets all locks
     * 
     * @return All locks currently held
     * @throws DataAccessLayerException
     *             If database errors occur
     */
    @SuppressWarnings("unchecked")
    public List<Lock> getAllLocks() throws DataAccessLayerException {
        Session s = this.getHibernateTemplate().getSessionFactory()
                .openSession();
        List<Lock> locks = null;
        try {
            // Until cluster-safe caching is in place, this line is necessary to
            // maintain consistency in a cluster and prevent phantom locks
            // s.setCacheMode(CacheMode.IGNORE);

            Criteria c = s.createCriteria(daoClass);
            locks = c.list();
            if (locks == null) {
                locks = new ArrayList<Lock>();
            }
        } finally {
            if (s != null) {
                s.close();
            }
        }
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
}
