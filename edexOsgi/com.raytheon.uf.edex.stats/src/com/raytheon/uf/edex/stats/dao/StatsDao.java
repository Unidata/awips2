package com.raytheon.uf.edex.stats.dao;

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.stats.StatsRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

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
}
