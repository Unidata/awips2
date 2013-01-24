package com.raytheon.uf.viz.datadelivery.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import com.raytheon.uf.common.datadelivery.event.notification.DeleteNotificationRequest;
import com.raytheon.uf.common.datadelivery.event.notification.DeleteNotificationResponse;
import com.raytheon.uf.common.datadelivery.event.notification.GetNotificationRequest;
import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.datadelivery.notification.xml.MessageLoadXML;

/**
 * 
 * Manages the retrieval of current and arriving notification records
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2012            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class NotificationHandler implements INotificationObserver {

    public static interface INotificationArrivedListener {
        public void handleNotification(
                ArrayList<NotificationRecord> notificationRecords);

        public void deleteNotification(ArrayList<Integer> ids);
    }

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NotificationHandler.class);

    private static Set<INotificationArrivedListener> listeners = new CopyOnWriteArraySet<INotificationArrivedListener>();

    /**
     * Add a notifications arrived listener, listeners will get notified when
     * new notifications have been ingested in edex
     * 
     * @param listener
     */
    public static void addListener(INotificationArrivedListener listener) {
        listeners.add(listener);
    }

    /**
     * Remove the listener
     * 
     * @param listener
     */
    public static void removeListener(INotificationArrivedListener listener) {
        listeners.remove(listener);
    }

    /**
     * Request the notifications from the notification database.Reads in the
     * default configuration.
     * 
     * @param messageLoad
     *            messages to load
     * @param users
     *            user list
     * @return List of notification records
     */
    @SuppressWarnings("unchecked")
    public List<NotificationRecord> intialLoad(MessageLoadXML messageLoad,
            ArrayList<String> users) {
        int loadAmount;
        String username = null;
        Integer hours = null;
        Integer maxResults = null;

        // Retrieve the message load configuration
        if (messageLoad != null) {
            loadAmount = messageLoad.getLoadLast();

            if (messageLoad.isNumHours()) {
                hours = loadAmount;
            } else if (messageLoad.isNumMessages()) {
                maxResults = loadAmount;
            }
        }

        // Set usernames from filter
        if (users != null && users.isEmpty() == false) {
            StringBuffer sb = new StringBuffer();
            for (String user : users) {
                if (sb.length() > 0) {
                    sb.append(",");
                }
                sb.append(user);
            }
            username = sb.toString();
        }

        // Request data from the notification table.
        try {
            GetNotificationRequest request = new GetNotificationRequest();
            request.setUsername(username);
            request.setHours(hours);
            request.setMaxResults(maxResults);
            ArrayList<NotificationRecord> response = (ArrayList<NotificationRecord>) ThriftClient
                    .sendRequest(request);
            return response;
        } catch (VizException e) {
            statusHandler.error(
                    "Error trying to retrieve notifications from database", e);
        }

        return new ArrayList<NotificationRecord>();
    }

    /**
     * Deletes records from the notification table
     * 
     * @param ids
     *            The record ids to be deleted
     * @return the number of rows deleted from the table
     */
    public int delete(ArrayList<Integer> ids) {
        int rowsDeleted = 0;

        try {
            DeleteNotificationRequest request = new DeleteNotificationRequest();
            request.setIds(ids);
            DeleteNotificationResponse response = (DeleteNotificationResponse) ThriftClient
                    .sendRequest(request);
            rowsDeleted = response.getRowsDeleted();
        } catch (VizException e) {
            statusHandler.error(
                    "Error trying to delete notification(s) from database", e);
        }

        return rowsDeleted;
    }

    /**
     * Processes an arriving NotificationRecord and notifies all listeners (i.e.
     * dialog)
     * 
     * @param messages
     *            The array of messages being sent from 'notify.msg'
     */
    @Override
    @SuppressWarnings("unchecked")
    public void notificationArrived(NotificationMessage[] messages) {
        ArrayList<Integer> deleteRecordIds = new ArrayList<Integer>();
        ArrayList<NotificationRecord> notificationRecords = new ArrayList<NotificationRecord>();

        try {
            for (NotificationMessage msg : messages) {
                Object obj = msg.getMessagePayload();
                if (obj instanceof NotificationRecord) {
                    notificationRecords.add((NotificationRecord) obj);
                } else if (obj instanceof DeleteNotificationResponse) {
                    DeleteNotificationResponse response = (DeleteNotificationResponse) obj;
                    deleteRecordIds.addAll(response.getIds());
                }
            }
        } catch (NotificationException e) {
            statusHandler.error("Error when receiving notification", e);
        }

        if (notificationRecords.isEmpty() == false) {
            for (INotificationArrivedListener listener : listeners) {
                listener.handleNotification(notificationRecords);
            }
        }

        if (deleteRecordIds.isEmpty() == false) {
            for (INotificationArrivedListener listener : listeners) {
                listener.deleteNotification(deleteRecordIds);
            }
        }
    }
}
