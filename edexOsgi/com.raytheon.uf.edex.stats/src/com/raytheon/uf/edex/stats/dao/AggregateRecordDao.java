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
import java.util.Date;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.Transaction;

import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Record class for stats waiting to be stored in the appropriate bucket.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez    Initial creation
 * May 22, 2013 1917       rjpeter     Added query methods for retrieving data about aggregates.
 * </pre>
 * 
 * @author jsanchez
 */
public class AggregateRecordDao extends CoreDao {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AggregateRecordDao.class);

    /**
     * Creates a new data access object
     */
    public AggregateRecordDao() {
        super(DaoConfig.forClass("metadata", AggregateRecord.class));
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
    public void mergeRecord(AggregateRecord newRecord)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(AggregateRecord.class);
        query.addQueryParam("eventType", newRecord.getEventType(),
                QueryOperand.EQUALS);
        query.addQueryParam("field", newRecord.getField(), QueryOperand.EQUALS);
        query.addQueryParam("grouping", newRecord.getGrouping(),
                QueryOperand.EQUALS);
        query.addQueryParam("startDate", newRecord.getStartDate(),
                QueryOperand.EQUALS);
        query.addQueryParam("endDate", newRecord.getEndDate(),
                QueryOperand.EQUALS);

        List<AggregateRecord> results = (List<AggregateRecord>) queryByCriteria(query);
        if (!CollectionUtil.isNullOrEmpty(results)) {
            // shouldn't be able to get multiple results, just merge with first
            // and update
            AggregateRecord prevRecord = results.get(0);
            prevRecord.setCount(prevRecord.getCount() + newRecord.getCount());
            prevRecord.setSum(prevRecord.getSum() + newRecord.getSum());
            if (newRecord.getMin() < prevRecord.getMin()) {
                prevRecord.setMin(newRecord.getMin());
            }
            if (newRecord.getMax() > prevRecord.getMax()) {
                prevRecord.setMax(newRecord.getMax());
            }
            update(prevRecord);
        } else {
            persist(newRecord);
        }
    }

    /**
     * Returns the oldest start date for a given aggregate eventType.
     * 
     * @param eventType
     * @return
     * @throws DataAccessLayerException
     */
    public Date getOldestAggregateDate(final String eventType)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();

            Query query = sess
                    .createQuery("SELECT MIN(startDate) FROM AggregateRecord WHERE eventType = ?");
            query.setString(0, eventType);
            Calendar rval = (Calendar) query.uniqueResult();
            tx.commit();
            if (rval != null) {
                return rval.getTime();
            }

            return null;
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    statusHandler.error(
                            "Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException(
                    "Unable to look up min start date for event [" + eventType
                            + "]", e);
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
     * Returns all aggregates of a given type and such that startDate >=
     * event.startDate < endDate.
     * 
     * @param eventType
     * @param startDate
     * @param endDate
     * @return
     * @throws DataAccessLayerException
     */
    public List<AggregateRecord> getAggregates(final String eventType,
            final Date startDate, final Date endDate)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();

            Query query = sess
                    .createQuery("FROM AggregateRecord WHERE eventType = ? AND startDate >= ? AND startDate < ? ORDER BY startDate");
            query.setString(0, eventType);
            query.setTimestamp(1, startDate);
            query.setTimestamp(2, endDate);
            @SuppressWarnings("unchecked")
            List<AggregateRecord> rval = query.list();
            tx.commit();
            return rval;
        } catch (Exception e) {
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception e1) {
                    statusHandler.error(
                            "Error occurred rolling back transaction", e1);
                }
            }

            throw new DataAccessLayerException(
                    "Unable to look up aggregates for event [" + eventType
                            + "]", e);
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
