package com.raytheon.uf.edex.datadelivery.event.handler;

import com.raytheon.uf.common.datadelivery.event.notification.DeleteNotificationRequest;
import com.raytheon.uf.common.datadelivery.event.notification.DeleteNotificationResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.event.notification.NotificationDao;

/**
 * 
 * Handles thrift request to delete data from the notification table
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
public class DeleteNotificationHandler extends AbstractHandler implements
        IRequestHandler<DeleteNotificationRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DeleteNotificationHandler.class);

    private String uri;

    /**
     * Create a new object
     * 
     * @param uri
     *            the jms uri to send the response
     */
    public DeleteNotificationHandler(String uri) {
        this.uri = uri;
    }

    /**
     * Handles request to retrieve Notification records
     */
    @Override
    public DeleteNotificationResponse handleRequest(
            DeleteNotificationRequest request) throws Exception {
        NotificationDao dao = new NotificationDao();
        int rowsDeleted = dao.deleteRecords(request.getIds());

        DeleteNotificationResponse response = new DeleteNotificationResponse();
        response.setIds(request.getIds());
        response.setRowsDeleted(rowsDeleted);

        send(response, uri);

        return response;
    }

}
