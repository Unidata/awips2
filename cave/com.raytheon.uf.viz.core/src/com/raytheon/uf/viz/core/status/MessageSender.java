/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.core.status;

import java.util.Map;

import javax.jms.BytesMessage;
import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.Session;

import com.raytheon.uf.common.message.IMessage;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.comm.JMSConnection;
import com.raytheon.uf.viz.core.preferences.JMSPreferences;

/**
 * Sends messages to jms endpoints programmatically.
 */

public class MessageSender {

    private enum DestinationType {
        TOPIC, QUEUE
    };

    public static void sendToQueue(String endpoint, Object message)
            throws JMSException, SerializationException {
        send(endpoint, DestinationType.QUEUE, message);
    }

    public static void sendToTopic(String endpoint, Object message)
            throws JMSException, SerializationException {
        send(endpoint, DestinationType.TOPIC, message);
    }

    private static void send(String endpoint, DestinationType type,
            Object message) throws JMSException, SerializationException {
        Connection connection = null;

        try {
            connection = JMSConnection.getInstance().getFactory()
                    .createConnection();
            Session session = connection.createSession(false,
                    Session.AUTO_ACKNOWLEDGE);
            Destination dest = getDestination(type, endpoint, session);
            MessageProducer messProducer = session.createProducer(dest);
            BytesMessage bytesMess = session.createBytesMessage();

            bytesMess.writeBytes(SerializationUtil.transformToThrift(message));
            addAdditionalInfo(message, bytesMess);
            messProducer.send(bytesMess);
        } finally {
            if (connection != null) {
                try {
                    connection.close();
                    connection = null;
                } catch (Exception e) {
                    connection = null;
                }
            }
        }
    }

    private static Destination getDestination(DestinationType type,
            String endpoint, Session session) throws JMSException {
        Destination dest = null;
        if (DestinationType.QUEUE.equals(type)) {
            dest = session.createQueue(endpoint);
        }
        if (DestinationType.TOPIC.equals(type)) {
            dest = session
                    .createTopic(JMSPreferences.getPolicyString(endpoint));
        }
        return dest;

    }

    private static void addAdditionalInfo(Object message, BytesMessage bytesMess)
            throws JMSException {
        bytesMess.setJMSType(message.getClass().getName());
        if (message instanceof IMessage) {
            Map<String, Object> headers = ((IMessage) message).getHeaders();
            for (String key : headers.keySet()) {
                bytesMess.setObjectProperty(key, headers.get(key).toString());
            }
        }
    }

}
