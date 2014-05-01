package com.raytheon.uf.edex.datadelivery.event.notification;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Pattern;

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
 * 09/05/2013   2314       mpduff      Change query to an "in" query for user names.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class NotificationDao extends
        SessionManagedDao<Integer, NotificationRecord> {
    private static final Pattern SPLIT_PATTERN = Pattern.compile(",");

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
     * @param loadAll
     *            Load all messages flag
     * @return the Notification records based on passed constraints. The records
     *         are in descending order based on date
     */
    public List<NotificationRecord> lookupNotifications(String username,
            Integer hours, Integer maxResults, Boolean loadAll) {
        StringBuilder hql = new StringBuilder("from NotificationRecord rec");
        String nameClause = " rec.username in (:userNames) ";
        String dateClause = " rec.date >= :date ";
        Calendar latestTime = null;
        List<String> userNames = null;

        if (hours != null) {
            latestTime = Calendar.getInstance();
            latestTime.add(Calendar.HOUR, -hours);
        }

        if (username != null) {
            String[] users = SPLIT_PATTERN.split(username);
            userNames = Arrays.asList(users);
        }
        List<Object> params = new ArrayList<Object>();
        if (userNames == null && hours != null) {
            if (!loadAll) {
                hql.append(" where ").append(dateClause);
                params.add("date");
                params.add(latestTime);
            }
        } else if (userNames != null && hours == null) {
            hql.append(" where ").append(nameClause);
            params.add("userNames");
            params.add(userNames);
        } else if (userNames != null && hours != null) {
            hql.append(" where ").append(nameClause);
            if (!loadAll) {
                hql.append(" and ").append(dateClause);
                params.add("date");
                params.add(latestTime);
            }
            params.add("userNames");
            params.add(userNames);
        }
        hql.append(" order by rec.date desc");

        if (maxResults == null) {
            return this.query(hql.toString(),
                    params.toArray(new Object[params.size()]));
        } else {
            return this.query(hql.toString(), maxResults,
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
