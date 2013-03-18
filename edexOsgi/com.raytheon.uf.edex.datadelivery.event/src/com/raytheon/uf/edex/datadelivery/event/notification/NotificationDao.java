package com.raytheon.uf.edex.datadelivery.event.notification;

import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;

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
 * 3/18/2013    1802       bphillip    Modified to use transactional boundaries and spring injection of daos
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class NotificationDao extends
        SessionManagedDao<Integer, NotificationRecord> {

    /**
     * Creates a new data access object
     */
    public NotificationDao() {

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
    public List<NotificationRecord> lookupNotifications(String username,
            Integer hours, Integer maxResults) {
        Map<String, Object> params = new HashMap<String, Object>();
        String hql = "from NotificationRecord rec";
        String nameClause = " rec.username=:userName ";
        String dateClause = " rec.date >= :date ";
        Calendar latestTime = null;
        if (hours != null) {
            latestTime = Calendar.getInstance();
            latestTime.add(Calendar.HOUR, -hours);
        }

        if (username == null && hours != null) {
            hql += " where " + dateClause;
            params.put("date", latestTime);
        } else if (username != null && hours == null) {
            hql += " where " + nameClause;
            params.put("userName", username);
        } else if (username != null && hours != null) {
            hql += " where " + nameClause + " and " + dateClause;
            params.put("date", latestTime);
            params.put("userName", username);
        }
        hql += " order by rec.date desc";

        if (maxResults == null) {
            return this.query(hql, params);
        } else {
            return this.query(hql, params, maxResults);
        }
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
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("date", expiration);
        String hqlStatement = "delete NotificationRecord r where r.date < :date";
        return this.executeHQLStatement(hqlStatement, params);
    }

    /**
     * Deletes notificaion record from the db based on the ids
     * 
     * @param ids
     *            the notification ids
     * @return the number of rows deleted
     */
    public int deleteRecords(List<Integer> ids) throws DataAccessLayerException {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("ids", ids);
        String hqlStatement = "delete NotificationRecord r where r.id in :ids";
        return this.executeHQLStatement(hqlStatement, params);
    }

    @Override
    public NotificationRecord getById(Integer id) {
        return super.getById(id);
    }

    @Override
    protected Class<NotificationRecord> getEntityClass() {
        return NotificationRecord.class;
    }
}
