package com.raytheon.uf.edex.datadelivery.event.handler;

import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.datadelivery.event.notification.NotificationDao;

/**
 * Abstract class to provide the send and store capabilities to subclasses.
 * 
 * @author jsanchez
 * 
 */
public abstract class AbstractHandler {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractHandler.class);

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
            NotificationDao dao = new NotificationDao();
            dao.persist(record);
            send(record, endpoint);
        }
    }
}
