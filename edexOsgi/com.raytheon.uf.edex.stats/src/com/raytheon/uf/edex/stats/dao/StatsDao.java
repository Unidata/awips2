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
package com.raytheon.uf.edex.stats.dao;

import java.util.Calendar;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.StatelessSession;

import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.stats.StatsRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Data access object for raw statistics.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez    Initial creation
 * May 22, 2013 1917       rjpeter     Added reclaimSpace.
 * </pre>
 * 
 * @author jsanchez
 */
public class StatsDao extends CoreDao {
    /**
     * Creates a new data access object
     */
    public StatsDao() {
        super(DaoConfig.forClass("metadata", StatsRecord.class));
    }

    /**
     * Retrieves stat records that has a date before the limit.
     * 
     * @param limit
     * @param eventType
     * @param maxResults
     *            if greater than 0 will limit database results to maxResults
     * @return an array of stat records. If an error occurs, then an array of
     *         size 0 will be returned.
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public List<StatsRecord> retrieveRecords(Calendar limit, String eventType,
            int maxResults) throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(StatsRecord.class);
        query.addQueryParam("eventType", eventType, QueryOperand.EQUALS);
        query.addQueryParam("date", limit, QueryOperand.LESSTHAN);
        query.addOrder("date", true);

        if (maxResults > 0) {
            query.setMaxResults(maxResults);
        }

        return (List<StatsRecord>) queryByCriteria(query);
    }

    /**
     * Manually runs vacuum due to large numbers of inserts and deletes to keep
     * table size to a minimum.
     */
    public void reclaimSpace() {
        StatelessSession sess = null;

        try {
            sess = getHibernateTemplate().getSessionFactory()
                    .openStatelessSession();
            // vacuum can't run within a transaction, hack to allow vacuum to
            // run from within hibernate
            Query query = sess
                    .createSQLQuery("rollback; VACUUM ANALYZE events.stats");
            query.executeUpdate();
            statusHandler.info("stats vacuumed");
        } catch (Exception e) {
            statusHandler.error(
                    "Error occurred running VACUUM on events.stats", e);
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
