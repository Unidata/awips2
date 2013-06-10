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

import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;

/**
 * Record class for stats waiting to be stored in the appropriate bucket.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez    Initial creation
 * Mar 18, 2013 1082       bphillip    Modified to extend sessionmanagedDao and use spring injection
 * May 22, 2013 1917       rjpeter     Added query methods for retrieving data about aggregates.
 * </pre>
 * 
 * @author jsanchez
 */
public class AggregateRecordDao extends
        SessionManagedDao<Integer, AggregateRecord> {
    /**
     * Creates a new data access object
     */
    public AggregateRecordDao() {

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
     */
    public void mergeRecord(AggregateRecord newRecord) {
        String hql = "from AggregateRecord rec where rec.eventType = :eventType and rec.field = :field"
                + " and rec.grouping = :grouping and rec.startDate = :startDate and rec.endDate = :endDate";

        List<AggregateRecord> results = this.executeHQLQuery(hql, "eventType",
                newRecord.getEventType(), "field", newRecord.getField(),
                "grouping", newRecord.getGrouping(), "startDate",
                newRecord.getStartDate(), "endDate", newRecord.getEndDate());
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
            this.update(prevRecord);
        } else {
            this.createOrUpdate(newRecord);
        }
    }

    @Override
    public AggregateRecord getById(Integer id) {
        return super.getById(id);
    }

    @Override
    protected Class<AggregateRecord> getEntityClass() {
        return AggregateRecord.class;
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
        String hql = "SELECT MIN(startDate) FROM AggregateRecord WHERE eventType = :eventType";

        try {
            List<Calendar> results = this.executeHQLQuery(hql, "eventType",
                    eventType);
            if (!CollectionUtil.isNullOrEmpty(results)) {
                Calendar minTime = results.get(0);
                if (minTime != null) {
                    return minTime.getTime();
                }
            }

            return null;
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to look up min start date for event [" + eventType
                            + "]", e);

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
        String hql = "FROM AggregateRecord WHERE eventType = :eventType AND startDate >= minStart AND startDate < maxStart ORDER BY startDate";
        try {
            List<AggregateRecord> results = this.executeHQLQuery(hql,
                    "eventType", eventType, "minStart", startDate, "maxStart",
                    endDate);
            return results;
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to look up aggregates for event [" + eventType
                            + "]", e);

        }
    }
}
