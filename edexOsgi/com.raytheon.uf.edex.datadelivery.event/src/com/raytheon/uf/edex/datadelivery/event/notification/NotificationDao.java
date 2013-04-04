package com.raytheon.uf.edex.datadelivery.event.notification;

import java.util.ArrayList;
import java.util.Calendar;

import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * 
 * Data access object to access Notification records from the database
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class NotificationDao extends CoreDao {

    /**
     * Creates a new data access object
     */
    public NotificationDao() {
        super(DaoConfig.forClass("metadata", NotificationRecord.class));
    }

    /**
     * Retrieves Notification records
     * 
     * @param username
     *            Retrieves records that have only these usernames
     * @param hours
     *            The number of hours back from the current of hold old records
     *            can be to be retrieved
     * @param maxResults
     *            The max result of records to retrieve
     * @return the Notificaion records based on passed contraints. The records
     *         are in descending order based on date
     */
    public ArrayList<NotificationRecord> lookupNotifications(String username,
            Integer hours, Integer maxResults) {
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        if (username != null) {
            query.addQueryParam("username", username, QueryOperand.IN);
        }

        if (hours != null) {
            Calendar latestTime = Calendar.getInstance();
            latestTime.add(Calendar.HOUR, -hours);
            query.addQueryParam("date", latestTime,
                    QueryOperand.GREATERTHANEQUALS);
        }

        if (maxResults != null) {
            query.setMaxResults(maxResults);
        }

        query.addOrder("date", false);

        ArrayList<NotificationRecord> result = null;
        try {
            result = (ArrayList<NotificationRecord>) queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error querying notification table", e);
        }

        return result;
    }

    /**
     * Purges the notification table that have records older than the expiration
     * date.
     * 
     * @param expirationDate
     *            the expiration date
     * @return the number of rows deleted
     */
    public int purgeExpiredData(Calendar expiration)
            throws DataAccessLayerException {
        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        deleteStmt.addQueryParam("date", expiration, QueryOperand.LESSTHAN);
        return this.deleteByCriteria(deleteStmt);
    }

    /**
     * Deletes notificaion record from the db based on the ids
     * 
     * @param ids
     *            the notification ids
     * @return the number of rows deleted
     */
    public int deleteRecords(ArrayList<Integer> ids)
            throws DataAccessLayerException {
        StringBuffer sb = new StringBuffer();
        for (Integer id : ids) {
            if (sb.length() != 0) {
                sb.append(",");
            }
            sb.append(String.valueOf(id.intValue()));
        }

        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        deleteStmt.addQueryParam("id", sb.toString(), QueryOperand.IN);
        return this.deleteByCriteria(deleteStmt);
    }
}
