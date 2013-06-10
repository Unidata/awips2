package com.raytheon.uf.edex.datadelivery.event.handler;

import java.util.List;

import com.raytheon.uf.common.datadelivery.event.notification.GetNotificationRequest;
import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

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
 * 3/18/2013    1802       bphillip    Modified to use transactional boundaries and spring injection of daos
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class GetNotificationHandler extends AbstractHandler implements
        IRequestHandler<GetNotificationRequest> {

    public GetNotificationHandler() {
        super();
    }

    /**
     * Handles request to retrieve Notification records
     */
    @Override
    public List<NotificationRecord> handleRequest(GetNotificationRequest request)
            throws Exception {
        List<NotificationRecord> notifications = notificationDao
                .lookupNotifications(request.getUsername(), request.getHours(),
                        request.getMaxResults());
        return notifications;
    }

}
