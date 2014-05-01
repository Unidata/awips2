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
package com.raytheon.uf.viz.alertviz;

import java.io.PrintStream;
import java.io.StringWriter;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.activemq.command.ActiveMQTextMessage;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.internal.JMSConnectionManager;

/**
 * Client features for AlertViz
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class AlertVizClient implements MessageListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertVizClient.class, "GDN_ADMIN", "GDN_ADMIN");

    private Queue intoAlertVizQueue;

    private Queue outOfAlertVizQueue;

    private MessageProducer producer;

    private MessageConsumer consumer;

    private JMSConnectionManager connectionManager;

    private boolean local;

    private int port;

    private String host = null;

    private boolean reconnect = false;

    private long lastReconnectTime = 0;

    private boolean retryOnExceptions;

    private CopyOnWriteArrayList<IAlertVizMessageCallback> listeners;

    private static int POOL_SIZE = 5;

    private java.util.Queue<Marshaller> marshallers = new ConcurrentLinkedQueue<Marshaller>();

    private JAXBContext jaxbContext;

    private static AlertVizClient instance;

    public AlertVizClient(String host, boolean retry) {
        this.host = host;
        this.listeners = new CopyOnWriteArrayList<IAlertVizMessageCallback>();
        this.local = false;
        this.retryOnExceptions = retry;
        instance = this;
    }

    public AlertVizClient(boolean local, int port) {
        this.local = local;
        this.port = port;
        this.listeners = new CopyOnWriteArrayList<IAlertVizMessageCallback>();
        instance = this;
    }

    public void start() throws AlertvizException {
        try {
            if (host != null) {
                connectionManager = JMSConnectionManager
                        .getConnectionManager(host);
            } else {
                connectionManager = JMSConnectionManager.getConnectionManager(
                        local, port);
            }
            Session session = connectionManager.getSession();
            if (host != null || !local) {
                connectionManager.getConnection().setExceptionListener(
                        new ExceptionListener() {
                            @Override
                            public void onException(JMSException arg0) {
                                reconnect = true;
                            }
                        });
            }
            this.intoAlertVizQueue = session.createQueue("messages");
            this.producer = session.createProducer(intoAlertVizQueue);
            this.outOfAlertVizQueue = session.createQueue("events");
            this.consumer = session.createConsumer(outOfAlertVizQueue);

            this.consumer.setMessageListener(this);
            reconnect = false;
            lastReconnectTime = System.currentTimeMillis();
            jaxbContext = JAXBContext.newInstance(StatusMessage.class);
        } catch (JMSException e) {
            reconnect = true;
            throw new AlertvizException("Unable to connect to notification", e);
        } catch (JAXBException e) {
            reconnect = true;
            throw new AlertvizException("Unable to connect to notification", e);
        }
    }

    public static void sendMessage(StatusMessage statusMessage)
            throws AlertvizException {

        if (instance == null)
            throw new IllegalStateException("AlertvizJob not started");

        instance.send(statusMessage);
    }

    private void send(StatusMessage statusMessage) throws AlertvizException {
        if (retryOnExceptions == false && reconnect == true) {
            printToConsole(statusMessage);
        } else {
            Marshaller marshaller = null;

            try {
                StringWriter sw = new StringWriter();
                marshaller = getMarshaller();
                marshaller.marshal(statusMessage, sw);
                ActiveMQTextMessage message = new ActiveMQTextMessage();
                message.setText(sw.toString());
                if (connectionManager.isConnectionClosed()) {
                    connectionManager.resetConnection();
                    start();
                }

                if (reconnect
                        && (System.currentTimeMillis() - lastReconnectTime) > (10 * 1000)) {
                    connectionManager.closeSession();
                    start();
                }

                producer.send(message);
            } catch (Exception e) {
                throw new AlertvizException("Error sending message", e);
            }

            if (marshaller != null && marshallers.size() < POOL_SIZE) {
                marshallers.add(marshaller);
            }
        }
    }

    private Marshaller getMarshaller() throws JAXBException {
        Marshaller m = marshallers.poll();
        if (m == null) {
            m = jaxbContext.createMarshaller();
        }
        return m;
    }

    /**
     * @param statusMessage
     */
    private void printToConsole(StatusMessage statusMessage) {
        PrintStream out = System.err;
        switch (statusMessage.getPriority()) {
        case CRITICAL:
        case PROBLEM:
        case SIGNIFICANT: {
            out.println(statusMessage.getCategory() + ":"
                    + statusMessage.getSourceKey() != null ? statusMessage
                    .getSourceKey() + ":" : "" + statusMessage.getPriority()
                    + ": " + statusMessage.getMessage());
            out.println(statusMessage.getDetails());
            break;
        }
        }
    }

    public MessageConsumer getInboundConsumer() throws JMSException {
        return connectionManager.getSession().createConsumer(intoAlertVizQueue);
    }

    @Override
    public void onMessage(Message arg0) {
        if (arg0 instanceof TextMessage) {
            try {
                String str = ((TextMessage) arg0).getText();
                for (IAlertVizMessageCallback c : listeners) {
                    c.messageArrived(str);
                }
            } catch (JMSException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error processing alertviz callback", e);
            }
        }
    }

    public static void addAlertVizCallbackListener(
            IAlertVizMessageCallback alertVizMessageCallback) {
        instance.listeners.add(alertVizMessageCallback);
    }

    public static void removeAlertVizCallbackListener(
            IAlertVizMessageCallback alertVizMessageCallback) {
        instance.listeners.remove(alertVizMessageCallback);
    }

    public static void sendCallbackMessage(String str) throws JMSException {
        MessageProducer producer = instance.connectionManager.getSession()
                .createProducer(instance.outOfAlertVizQueue);
        TextMessage tm = instance.connectionManager.getSession()
                .createTextMessage(str);
        producer.send(tm);

    }

    public JMSConnectionManager getConnectionManager() {
        return connectionManager;
    }

    public static interface IAlertVizMessageCallback {
        public void messageArrived(String msg);
    }

}
