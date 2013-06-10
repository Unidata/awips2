package com.raytheon.uf.edex.datadelivery.event.notification;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

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
 * 4/9/2013     1802       bphillip    Changed to use new query method signatures in SessionManagedDao
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
        String hql = "from NotificationRecord rec";
        String nameClause = " rec.username=:userName ";
        String dateClause = " rec.date >= :date ";
        Calendar latestTime = null;
        if (hours != null) {
            latestTime = Calendar.getInstance();
            latestTime.add(Calendar.HOUR, -hours);
        }

        List<Object> params = new ArrayList<Object>();
        if (username == null && hours != null) {
            hql += " where " + dateClause;
            params.add("date");
            params.add(latestTime);
        } else if (username != null && hours == null) {
            hql += " where " + nameClause;
            params.add("userName");
            params.add(username);
        } else if (username != null && hours != null) {
            hql += " where " + nameClause + " and " + dateClause;
            params.add("date");
            params.add(latestTime);
            params.add("userName");
            params.add(username);
        }
        hql += " order by rec.date desc";

        if (maxResults == null) {
            return this.query(hql, params.toArray(new Object[params.size()]));
        } else {
            return this.query(hql, maxResults,
                    params.toArray(new Object[params.size()]));
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
        String hqlStatement = "delete NotificationRecord r where r.date < :date";
        return this.executeHQLStatement(hqlStatement, "date", expiration);
    }

    /**
     * Deletes notificaion record from the db based on the ids
     * 
     * @param ids
     *            the notification ids
     * @return the number of rows deleted
     */
    public int deleteRecords(List<Integer> ids) throws DataAccessLayerException {
        String hqlStatement = "delete NotificationRecord r where r.id in :ids";
        return this.executeHQLStatement(hqlStatement, "ids", ids);
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
