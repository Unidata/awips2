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
package com.raytheon.edex.rpgenvdata;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.Properties;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueConnectionFactory;
import javax.jms.QueueReceiver;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.jms.TemporaryQueue;
import javax.jms.TextMessage;
import javax.naming.InitialContext;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.rcm.mqsrvr.EventObj;
import com.raytheon.rcm.mqsrvr.ReplyObj;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.rcm.rmr.RmrEvent;

public class RcmClient {
    private Properties properties;

    private QueueConnectionFactory connectionFactory;

    private Queue destination;

    private TemporaryQueue replyQueue;

    private QueueConnection queueConn;

    private QueueSession queueSession;

    private QueueSender queueSender;

    private QueueReceiver queueReceiver;

    private static JAXBContext jaxbCtx;

    private Marshaller marshaller;

    private Unmarshaller unmarshaller;

    private static final long RESPONSE_TIMEOUT = 30 * 1000;

    public RcmClient() {

    }

    public RcmClient(Properties properties) {
        this.properties = properties;
    }

    public void initialize() throws Exception {
        if (properties == null) {
            properties = new Properties();
        }

        if (jaxbCtx == null) {
            synchronized (RcmClient.class) {
                if (jaxbCtx == null) {
                    jaxbCtx = JAXBContext.newInstance(ReqObj.class,
                            ReplyObj.class, EventObj.class, RmrEvent.class);
                }
            }
        }

        marshaller = jaxbCtx.createMarshaller();
        unmarshaller = jaxbCtx.createUnmarshaller();

        properties.setProperty("queue.RadarServer", "RadarServer");
        properties.setProperty("java.naming.factory.initial",
                "org.apache.activemq.jndi.ActiveMQInitialContextFactory");
        properties.setProperty("connectionFactoryNames",
                "queueConnectionFactory");
        properties.setProperty("java.naming.provider.url",
                System.getenv("RADAR_SERVER"));
        javax.naming.Context ctx = new InitialContext(properties);
        connectionFactory = (QueueConnectionFactory) ctx
                .lookup("queueConnectionFactory");

        destination = (Queue) ctx.lookup("RadarServer");
    }

    public void close() {
        teardown();
    }

    public void onFailure(String detail, Exception e) {
        // This implementation does nothing. Subclasses may override.
    }

    public ReplyObj sendRequest(ReqObj req) {
        if (!getConnection()) {
            return null;
        }

        StringWriter sw = new StringWriter();
        try {
            marshaller.marshal(req, sw);
        } catch (JAXBException e) {
            onFailure("Error creating message", e);
            return null;
        }

        String messageText = null;

        try {
            TextMessage msg = queueSession.createTextMessage(sw.toString());
            msg.setJMSReplyTo(replyQueue);
            queueSender.send(msg);
            String msgId = msg.getJMSMessageID();

            while (true) {
                // TODO: diminishing timeout if ignored messages?
                Message m2 = queueReceiver.receive(RESPONSE_TIMEOUT);
                if (m2 == null) {
                    onFailure(
                            "Timed out waiting for response from RadarServer",
                            null);
                    return null;
                }
                String corrId = m2.getJMSCorrelationID();
                if (corrId != null && corrId.equals(msgId)) {
                    if (m2 instanceof TextMessage) {
                        messageText = ((TextMessage) m2).getText();
                    }
                    break;
                } else {
                    // TODO: really "failure"?
                    onFailure("Ignoring message from RadarServer", null);
                }
            }
        } catch (JMSException e) {
            onFailure("Error sending message to RadarServer", e);
            teardown();
            return null;
        }

        ReplyObj ro = null;
        try {
            if (messageText == null) {
                throw new Exception("Error decoding reply message");
            }
            StringReader sr = new StringReader(messageText);
            Object o = unmarshaller.unmarshal(sr);
            ro = (ReplyObj) o;
        } catch (Exception e) {
            // Catches JAXB exceptions and cast exception for assuming ReplyObj
            ro = new ReplyObj(e.toString());
        }

        return ro;
    }

    public <T> T sendCheckedAndHandled(ReqObj req, Class<T> replyClass) {
        ReplyObj ro = sendRequest(req);
        if (ro != null && ro.error != null) {
            onFailure("Received error from RadarServer: " + ro.error, null);
            return null;
        } else if (ro != null && !replyClass.isAssignableFrom(ro.getClass())) {
            onFailure(String.format("Received unexpected response of type %s",
                    replyClass.getSimpleName()), null);
            return null;
        }
        return replyClass.cast(ro);
    }

    public ReplyObj sendCheckedAndHandled(ReqObj req) {
        return sendCheckedAndHandled(req, ReplyObj.class);
    }

    private boolean getConnection() {
        if (queueConn == null) {
            try {
                queueConn = connectionFactory.createQueueConnection();
                queueSession = queueConn.createQueueSession(false,
                        Session.AUTO_ACKNOWLEDGE);
                // queueSender.setDisableMessageID(false);
                queueSender = queueSession.createSender(destination);
                replyQueue = queueSession.createTemporaryQueue();
                queueReceiver = queueSession.createReceiver(replyQueue);

                queueConn.start();
            } catch (Exception e) {
                teardown();
                onFailure("Error connecting to RadarServer", e);
            }
        }
        return queueConn != null;
    }

    private void teardown() {
        QueueConnection aConn = queueConn;
        queueConn = null;
        if (aConn != null) {
            try {
                aConn.close();
            } catch (Exception e2) {
                // nothing
            }
        }
    }

}
