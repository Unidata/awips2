package com.raytheon.uf.edex.stats.dao;

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.stats.StatsRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class StatsDao extends CoreDao {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StatsDao.class);

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
     * @return an array of stat records. If an error occurs, then an array of
     *         size 0 will be returned.
     * @throws DataAccessLayerException
     */
    public StatsRecord[] retrieveRecords(Calendar limit, String eventType) {
        DatabaseQuery query = new DatabaseQuery(StatsRecord.class);
        query.addQueryParam("eventType", eventType, QueryOperand.EQUALS);
        query.addQueryParam("date", limit, QueryOperand.LESSTHAN);
        query.setMaxResults(1000);

        // TODO Need to make StatsDao to keep track to determine next 1000
        // results.
        StatsRecord[] records = null;
        try {
            List<?> objects = queryByCriteria(query);
            records = new StatsRecord[objects.size()];
            for (int i = 0; i < records.length; i++) {
                records[i] = (StatsRecord) objects.get(i);
            }
        } catch (DataAccessLayerException e) {
            records = new StatsRecord[0];
            statusHandler.error("Error querying the stats table", e);
        }

        return records;
    }
}
