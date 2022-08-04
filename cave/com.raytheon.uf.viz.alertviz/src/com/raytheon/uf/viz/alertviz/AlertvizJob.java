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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Date;
import java.util.Deque;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.jms.BytesMessage;
import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.xml.bind.JAXBException;

import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.activemq.broker.BrokerService;
import org.apache.activemq.broker.TransportConnector;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.viz.core.VizApp;

/**
 * The AlertvizJob is a singleton Job that acts as a receiver for AlertViz and
 * is created through OSGi services. When scheduled and running, it receives
 * StatusMessages for AlertViz by consuming the messages from a JMS queue. It
 * can receive XML TextMessages over JMS through the tcp and stomp protocols, or
 * from within its own JVM as StatusMessage objects. The messages are processed
 * asynchronously on the job thread to ensure that the JMS queue does not get
 * backed up.
 *
 * There should only ever be one AlertvizJob running per machine per port. In
 * general there will only be one per machine, except in special circumstances.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 04, 2008  1433     chammack  Initial creation
 * Jun 03, 2013  2026     randerso  Improve error handling
 * May 05, 2015  4473     mschenke  Major refactor
 * Jun 01, 2015  4473     njensen   Major refactor, removed send ability
 * Jun 29, 2015  4473     njensen   Register notification observer on start
 * Jul 04, 2015  17167    dhuffman  Remove nulls from incoming xml
 * Mar 05, 2018  6899     dgilling  Support incoming messages of type
 *                                  BytesMessage.
 * Mar 20, 2018  7096     randerso  Remove call to StatusMessage.setEventTime()
 * Apr 19, 2018  7013     tgurney   Add "canExit" field and getter/setter
 * Sep 14, 2018  7464     dgilling  Set replacement message if received message
 *                                  is empty.
 * Oct 04, 2018  7484     randerso  Changed to use AV_ADMIN for internal errors
 * Oct 08, 2018  7515     randerso  Adjusted priorities of AlertViz internal
 *                                  errors.
 *
 * </pre>
 *
 * @author chammack
 *
 */

public class AlertvizJob extends Job implements AlertService {

    private static final String AV_ADMIN = "AV_ADMIN";

    private static final int MAX_CONNECTION_ATTEMPTS = 10;

    public static final String TCP_CONNECTION = "tcp://0.0.0.0:";

    public static final String STOMP_CONNECTION = "stomp://0.0.0.0:";

    public static final String LOCAL_SERVICE_NAME = "notification";

    public static interface AlertVizJobListener {

        public void receiverConnected();

        public void receiverDisconnected();
    }

    private final SingleTypeJAXBManager<StatusMessage> jaxb;

    private boolean embedded = true;

    private boolean canExit = false;

    private int exitStatus;

    private final Object waiter = new Object();

    private Set<IAlertArrivedCallback> callbacks = new CopyOnWriteArraySet<>();

    private Set<AlertVizJobListener> receiverListeners = new CopyOnWriteArraySet<>();

    private int port;

    private ConnectionFactory factory;

    private BrokerService broker;

    /** actually stores the log messages */
    private Container container;

    private Connection connection;

    private int connectionAttempts = 0;

    private boolean connectionStarted = false;

    private final ExceptionListener exceptionListener = new ExceptionListener() {
        @Override
        public void onException(JMSException e) {
            connectionException(e);
        }
    };

    private Session session;

    private MessageConsumer receiver;

    private Deque<StatusMessage> receivedMessages = new ArrayDeque<>();

    private final MessageListener jmsListener = new MessageListener() {
        @Override
        public void onMessage(Message m) {
            receive(m);
        }
    };

    public AlertvizJob() {
        super("AlertViz Receiver");
        setSystem(true);
        setPriority(Job.INTERACTIVE);
        this.jaxb = SingleTypeJAXBManager
                .createWithoutException(StatusMessage.class);
    }

    /**
     * Starts the broker and JMS services to act as the receiver for AlertViz,
     * then schedules the job to process any messages.
     *
     * @param port
     */
    public void start(int port) {
        this.broker = new BrokerService();
        this.broker.setBrokerName(LOCAL_SERVICE_NAME);
        this.broker.setPersistent(false);
        this.broker.setUseJmx(false);
        this.port = port;
        AlertVizNotificationObserver.registerAlertVizNotificationObserver();

        String localIP = SystemUtil.getHostName();
        String jmsURI = "tcp://" + localIP + ":" + port;
        this.factory = new ActiveMQConnectionFactory(jmsURI);
        this.schedule();
    }

    /**
     * @return The main {@link AlertvizJob} started by the plugin's activator
     */
    public static AlertvizJob getInstance() {
        return Activator.getDefault().getAlertvizJob();
    }

    /**
     * Adds a callback for getting notified when an alert is received from the
     * receiver.
     *
     * @param callback
     */
    public void addAlertArrivedCallback(IAlertArrivedCallback callback) {
        this.callbacks.add(callback);
    }

    /**
     * Removes a callback for getting notified when an alert is received from
     * the receiver.
     *
     * @param callback
     */
    public void removeAlertArrivedCallback(IAlertArrivedCallback callback) {
        this.callbacks.remove(callback);
    }

    /**
     * Adds a general job status listener for getting notified when state
     * changes for receiver
     *
     * @param listener
     */
    public void addAlertVizJobListener(AlertVizJobListener listener) {
        this.receiverListeners.add(listener);
        if (broker != null && broker.isStarted()) {
            listener.receiverConnected();
        }
    }

    /**
     * Removes a general job status listener for getting notified when state
     * changes for receiver
     *
     * @param listener
     */
    public void removeAlertVizJobListener(AlertVizJobListener listener) {
        this.receiverListeners.remove(listener);
    }

    private void connectionException(JMSException e) {
        logInternalError(
                "Exception occurred on AlertViz connection, will attempt reconnect",
                e);
        synchronized (this) {
            disconnect();
        }

        // Wake up thread so it reconnects
        synchronized (waiter) {
            waiter.notify();
        }
    }

    private void receive(Message message) {
        String xml = null;
        if (message instanceof TextMessage) {
            try {
                xml = ((TextMessage) message).getText();
            } catch (JMSException e) {
                logInternalError(
                        "Error getting XML text from JMS Message object", e);
            }
        } else if (message instanceof BytesMessage) {
            /*
             * BytesMessage has a convenience method readUTF which is supposed
             * to read the entire message body and convert to a UTF-8 string.
             * For whatever reason, this always throws an EOFException. Maybe a
             * bug in stomp.py?
             */
            try {
                BytesMessage byteMessage = ((BytesMessage) message);
                byte[] bytes = new byte[(int) byteMessage.getBodyLength()];
                byteMessage.readBytes(bytes);
                xml = new String(bytes, StandardCharsets.UTF_8);
            } catch (JMSException e) {
                logInternalError(
                        "Error getting XML text from JMS Message object", e);
            }
        } else {
            logInternalError(
                    "Unable to process JMS Message of type: "
                            + (message != null ? message.getClass() : "null"),
                    null);
        }

        if (xml != null) {
            try {
                xml = xml.replace("\u0000", "");
                StatusMessage sm = jaxb.unmarshalFromXml(xml);
                if (sm.getEventTime() == null) {
                    sm.setEventTime(new Date());
                }
                receive(sm);
            } catch (JAXBException e) {
                logInternalError(
                        "Error unmarshalling text message received with body:\n"
                                + xml,
                        e);
            }
        }
    }

    public void receive(StatusMessage sm) {
        if (sm != null) {
            if (StringUtils.isBlank(sm.getMessage())) {
                sm.setMessage("(Empty message. Click Show Log for details.)");
            }

            synchronized (receivedMessages) {
                receivedMessages.add(sm);
            }

            // Notify we have data
            synchronized (waiter) {
                waiter.notify();
            }
        }
    }

    /**
     * Logs an internal error when AlertViz itself has problems.
     *
     * @param message
     * @param t
     */
    private void logInternalError(String message, Throwable t) {
        StatusMessage sm = new StatusMessage();
        sm.setCategory(AV_ADMIN);
        sm.setSourceKey(AV_ADMIN);
        sm.setMessage(message);
        if (t != null) {
            StringWriter sw = new StringWriter();
            PrintWriter toString = new PrintWriter(sw);
            t.printStackTrace(toString);
            sm.setDetails(toString.toString());
        }
        sm.setMachineToCurrent();
        sm.setPlugin(Activator.PLUGIN_ID);
        sm.setPriority(Priority.CRITICAL);
        Container.logInternal(sm);
    }

    private void connect() {
        try {
            // attempt to create broker
            try {
                broker.addConnector(getTcpConnectionURI());
                broker.addConnector(getStompConnectionURI());
                broker.start();
            } catch (Exception e) {
                // There is probably another broker running on those ports.
                // If not, it will fail to make the connection and we'll log
                // the error there.
            }

            connection = factory.createConnection();
            connection.setExceptionListener(exceptionListener);
            session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
            Queue alertQueue = session.createQueue("messages");

            if (broker.isStarted()) {
                // Create message consumer
                receiver = session.createConsumer(alertQueue);
                receiver.setMessageListener(jmsListener);
                for (AlertVizJobListener listener : receiverListeners) {
                    listener.receiverConnected();
                }
            }

            // Start processing
            connection.start();
            connectionStarted = true;

            if (broker.isStarted() && this.container == null) {
                this.container = new Container(this.callbacks);
            }
            this.connectionAttempts = 0;
        } catch (JMSException e) {
            logInternalError("Error starting receiver connection", e);
            disconnect();
        }
    }

    private void disconnect() {
        if (receiver != null) {
            try {
                receiver.close();
            } catch (JMSException e) {
                // Ignore error
            }
            receiver = null;
            for (AlertVizJobListener listener : receiverListeners) {
                listener.receiverDisconnected();
            }
        }

        if (session != null) {
            try {
                session.close();
            } catch (JMSException e) {
                // Ignore error
            }
            this.session = null;
        }

        if (connectionStarted) {
            try {
                connection.stop();
            } catch (JMSException e) {
                // Ignore error
            }
            this.connectionStarted = false;
        }

        if (connection != null) {
            try {
                connection.close();
            } catch (JMSException e) {
                // Ignore error
            }
            this.connection = null;
        }

        if (broker.isStarted()) {
            try {
                broker.stop();
            } catch (Exception e) {
                // Ignore error
            }
        }

        // copy to avoid concurrent mod error
        List<TransportConnector> copy = new ArrayList<>(
                broker.getTransportConnectors());
        for (TransportConnector connector : copy) {
            try {
                broker.removeConnector(connector);
            } catch (Exception e) {
                // Ignore error
            }
        }

        // just in case a few messages snuck in right before disconnect
        synchronized (receivedMessages) {
            if (!receivedMessages.isEmpty()) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        Deque<StatusMessage> toProcess = new ArrayDeque<>(
                                receivedMessages);
                        receivedMessages.clear();
                        for (StatusMessage sm : toProcess) {
                            container.messageReceived(sm);
                        }
                    }
                });
                receivedMessages.clear();
            }
        }

        container = null;
    }

    private String getTcpConnectionURI() {
        return TCP_CONNECTION + port;
    }

    private String getStompConnectionURI() {
        return STOMP_CONNECTION + (port + 1);
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        while (!monitor.isCanceled()) {
            if (connection == null
                    && connectionAttempts < MAX_CONNECTION_ATTEMPTS) {
                connectionAttempts += 1;
                synchronized (this) {
                    connect();
                }
                if (connection == null) {
                    try {
                        Thread.sleep(10 * 1000L);
                    } catch (InterruptedException e) {
                        // Ignore
                    }
                }
            } else {
                ArrayDeque<StatusMessage> toProcess;
                do {
                    synchronized (receivedMessages) {
                        toProcess = new ArrayDeque<>(receivedMessages);
                        receivedMessages.clear();
                    }

                    // Receive messages
                    if (this.container != null) {
                        final ArrayDeque<StatusMessage> sendToContainer = toProcess;
                        VizApp.runAsync(new Runnable() {
                            @Override
                            public void run() {
                                for (StatusMessage sm : sendToContainer) {
                                    container.messageReceived(sm);
                                }
                            }
                        });
                    }
                } while ((!receivedMessages.isEmpty())
                        && (!monitor.isCanceled()));

                synchronized (waiter) {
                    try {
                        waiter.wait();
                    } catch (InterruptedException e) {
                        // Ignore interruption
                    }
                }
            }
        }

        try {
            this.connection.setExceptionListener(null);
        } catch (JMSException e) {
            logInternalError("Error removing exception listener", e);
        }
        disconnect();

        return Status.OK_STATUS;
    }

    @Override
    protected void canceling() {
        // Wake up the thread
        synchronized (waiter) {
            waiter.notify();
        }
    }

    @Override
    public boolean isEmbedded() {
        return embedded;
    }

    public void setEmbedded(boolean embedded) {
        this.embedded = embedded;
    }

    public int getExitStatus() {
        return exitStatus;
    }

    @Override
    public void setExitStatus(int exitStatus) {
        this.exitStatus = exitStatus;
    }

    public void setCanExit(boolean canExit) {
        this.canExit = canExit;
    }

    @Override
    public boolean isCanExit() {
        return canExit;
    }

}
