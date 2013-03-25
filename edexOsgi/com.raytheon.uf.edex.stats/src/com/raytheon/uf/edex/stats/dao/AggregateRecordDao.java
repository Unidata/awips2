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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;

/**
 * Stats object data access object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/18/2013    1082       bphillip     Modified to extend sessionmanagedDao and use spring injection
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
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
     * @throws DataAccessLayerException
     */
    public void mergeRecord(AggregateRecord newRecord)
            throws DataAccessLayerException {
        String hql = "from AggregateRecord rec where rec.eventType = :eventType and rec.field = :field and rec.grouping = :grouping and rec.startDate = :startDate and rec.endDate = :endDate";
        Map<String, Object> parameters = new HashMap<String, Object>();
        parameters.put("eventType", newRecord.getEventType());
        parameters.put("field", newRecord.getField());
        parameters.put("grouping", newRecord.getGrouping());
        parameters.put("startDate", newRecord.getStartDate());
        parameters.put("endDate", newRecord.getEndDate());

        List<AggregateRecord> results = this.executeHQLQuery(hql, parameters);
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
}
