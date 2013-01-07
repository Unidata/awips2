package com.raytheon.uf.edex.datadelivery.event.handler;

import java.util.ArrayList;

import com.raytheon.uf.common.datadelivery.event.notification.GetNotificationRequest;
import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.datadelivery.event.notification.NotificationDao;

/**
 * 
 * Handles thrift request to retrieve data from the notification table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2012            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class GetNotificationHandler extends AbstractHandler implements
        IRequestHandler<GetNotificationRequest> {

    /**
     * Handles request to retrieve Notification records
     */
    @Override
    public ArrayList<NotificationRecord> handleRequest(
            GetNotificationRequest request) throws Exception {
        NotificationDao dao = new NotificationDao();
        ArrayList<NotificationRecord> notifications = dao.lookupNotifications(
                request.getUsername(), request.getHours(),
                request.getMaxResults());
        return notifications;
    }

}
