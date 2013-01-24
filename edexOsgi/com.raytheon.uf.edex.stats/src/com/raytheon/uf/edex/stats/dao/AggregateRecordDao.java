package com.raytheon.uf.edex.stats.dao;

import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class AggregateRecordDao extends CoreDao {
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
}
