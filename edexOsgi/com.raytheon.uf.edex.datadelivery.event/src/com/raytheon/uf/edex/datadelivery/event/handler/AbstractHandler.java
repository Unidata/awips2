package com.raytheon.uf.edex.datadelivery.event.handler;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.datadelivery.event.notification.NotificationDao;

/**
 * 
 * Abstract class to provide the send and store capabilities to subclasses.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/18/2013    1802       bphillip    Implemented transactional boundaries
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Service
@Transactional
public abstract class AbstractHandler {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractHandler.class);

    protected NotificationDao notificationDao;

    protected AbstractHandler() {

    }

    /**
     * Sends the object to 'notifyRoute'.
     * 
     * @param obj
     */
    public void send(Object obj, String endpoint) {
        try {
            byte[] bytes = SerializationUtil.transformToThrift(obj);
            EDEXUtil.getMessageProducer().sendAsyncUri(endpoint, bytes);
        } catch (EdexException e) {
            statusHandler.error("Error sending record to " + endpoint, e);
        } catch (SerializationException e) {
            statusHandler.error("Error serializing record to " + endpoint, e);
        }
    }

    /**
     * Stores the record in the notification table.
     * 
     * @param record
     */
    void storeAndSend(NotificationRecord record, String endpoint) {
        if (record != null) {
            notificationDao.createOrUpdate(record);
            send(record, endpoint);
        }
    }

    public void setNotificationDao(NotificationDao notificationDao) {
        this.notificationDao = notificationDao;
    }

}
