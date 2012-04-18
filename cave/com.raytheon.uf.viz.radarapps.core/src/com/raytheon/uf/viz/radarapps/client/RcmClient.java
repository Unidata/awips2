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
package com.raytheon.uf.viz.radarapps.client;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;

import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueConnectionFactory;
import javax.jms.QueueReceiver;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.jms.TemporaryQueue;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.activemq.ActiveMQConnectionFactory;

import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.NotificationEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.mqsrvr.EventObj;
import com.raytheon.rcm.mqsrvr.ReplyObj;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.rcm.rmr.RmrEvent;
import com.raytheon.uf.common.dataplugin.radar.request.RadarServerConnectionRequest;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.preferences.JMSPreferences;
import com.raytheon.uf.viz.core.requests.ThriftClient;

// TODO: use of queueSession outside synchronized(stateLock) could cause
// NullPointerExceptions...

// TODO: conflicts over setting fatalMsg

public class RcmClient implements MessageListener, ExceptionListener {

    private String connectionURL;

    Queue destination;

    private TemporaryQueue replyQueue;

    QueueConnection queueConn;

    QueueSession queueSession;

    QueueSender queueSender;

    TopicConnection topicConn;

    TopicSession topicSession;

    Topic topic;

    TopicSubscriber topicSubscriber;

    private enum State {
        UNCONNECTED, CONNECTING, CONNECTED
    }

    private State state = State.UNCONNECTED;

    private String fatalMsg;

    private Object stateLock = new Object();

    ArrayList<RcmClientExceptionListener> exceptionListeners = new ArrayList<RcmClientExceptionListener>();

    ArrayList<RadarEventListener> radarEventListeners = new ArrayList<RadarEventListener>();

    HashMap<String, RcmClientHandler> handlers = new HashMap<String, RcmClientHandler>();

    public RcmClient() {
        RadarServerConnectionRequest request = new RadarServerConnectionRequest();
        try {
            connectionURL = (String) ThriftClient.sendRequest(request);
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    public void addRcmClientListener(RcmClientExceptionListener l) {
        synchronized (exceptionListeners) {
            exceptionListeners.add(l);
        }
    }

    public synchronized void removeRcmClientListener(
            RcmClientExceptionListener l) {
        synchronized (exceptionListeners) {
            exceptionListeners.remove(l);
        }
    }

    public void addEventListener(RadarEventListener l) {
        synchronized (radarEventListeners) {
            radarEventListeners.add(l);
        }
        subscribeToEvents();
    }

    public void removeEventListener(RadarEventListener l) {
        boolean unsubscribe = false;
        synchronized (radarEventListeners) {
            radarEventListeners.remove(l);
            if (radarEventListeners.size() == 0)
                unsubscribe = true;
        }
        if (unsubscribe) {

            synchronized (stateLock) {
                if (topicSubscriber != null) {
                    try {
                        topicSubscriber.close();
                    } catch (JMSException e) {
                        // nothing
                    }
                    topicSubscriber = null;
                }
            }
        }
    }

    private void subscribeToEvents() {
        synchronized (stateLock) {
            if (state == State.UNCONNECTED) {
                start();
                return;
            } else if (state != State.CONNECTED || topicSubscriber != null)
                return;
            try {
                topicSubscriber = topicSession.createSubscriber(topic);
                topicSubscriber.setMessageListener(new MessageListener() {
                    @Override
                    public void onMessage(Message msg) {
                        onEventMessage(msg);
                    }
                });
            } catch (JMSException e) {
                // TODO
                e.printStackTrace(System.err);
            }
        }
    }

    public void start() {
        synchronized (stateLock) {
            if (state == State.UNCONNECTED) {
                fatalMsg = null;
                state = State.CONNECTING;
                Thread t = new Thread(new Runnable() {
                    @Override
                    public void run() {
                        startInternal();
                    }
                });
                t.setDaemon(true);
                t.start();
            }
        }
    }

    private void startInternal() {
        synchronized (stateLock) {
            if (state != State.CONNECTING)
                return;
        }

        URI uri;
        try {
            uri = new URI(connectionURL);
        } catch (URISyntaxException e) {
            onRcmException(e);
            return;
        }

        ActiveMQConnectionFactory connFac = new ActiveMQConnectionFactory(uri);
        // This stuff can block...
        try {
            // TODO: need atomic set state and better cleanup

            QueueConnectionFactory qConnFac = connFac;
            // TopicConnectionFactory tConnFac = connFac;

            // System.out.println("connecting...");
            queueConn = qConnFac.createQueueConnection();
            // queueConn = conn;

            queueConn.setExceptionListener(this);
            // System.out.println("ok!");
            queueSession = queueConn.createQueueSession(false,
                    Session.AUTO_ACKNOWLEDGE);
            destination = queueSession.createQueue("RadarServer");
            queueSender = queueSession.createSender(destination);

            replyQueue = queueSession.createTemporaryQueue();
            QueueReceiver qr = queueSession.createReceiver(replyQueue);
            qr.setMessageListener(RcmClient.this);

            topicConn = connFac.createTopicConnection();
            topicConn.setExceptionListener(this);
            topicSession = topicConn.createTopicSession(false,
                    Session.AUTO_ACKNOWLEDGE);
            topic = topicSession.createTopic(JMSPreferences
                    .getPolicyString("RadarEvents"));

            queueConn.start();
            topicConn.start();

            synchronized (stateLock) {
                state = State.CONNECTED;
                stateLock.notifyAll();
            }

            boolean subscribe = false;
            synchronized (radarEventListeners) {
                if (radarEventListeners.size() > 0)
                    subscribe = true;
            }
            if (subscribe)
                subscribeToEvents();

        } catch (JMSException e) {
            shutdown(e.getMessage());
            onRcmException(e);
            return;
        }
        onReady();
    }

    @Override
    public void onMessage(Message msg) {
        String id;
        RcmClientHandler handler;

        try {
            id = msg.getJMSCorrelationID();
            handler = handlers.get(id);
        } catch (JMSException e) {
            // TODO:...
            System.err.println(e.toString());
            return;
        }

        ReplyObj ro;

        try {
            handlers.remove(id);

            if (msg instanceof TextMessage) {
                TextMessage tms = (TextMessage) msg;
                StringReader sr = new StringReader(tms.getText());
                Object o = null;
                try {
                    synchronized (u) {
                        o = u.unmarshal(sr);
                    }

                    if (o instanceof ReplyObj) {
                        ro = (ReplyObj) o;
                    } else {
                        handleEventObject(o);
                        return;
                    }
                } catch (JAXBException e) {
                    ro = new ReplyObj(e.toString());
                }
            } else {
                ro = new ReplyObj("Reply message is not a JMS text message");
            }
        } catch (JMSException e) {
            ro = new ReplyObj(e.toString());
        }

        if (handler == null)
            return;

        try {
            handler.onReply(ro);
        } catch (Exception e) {
            // TODO: ...
            e.printStackTrace(System.err);
        }

    }

    protected void onEventMessage(Message msg) {
        Object o = null;

        if (msg instanceof TextMessage) {
            TextMessage tms = (TextMessage) msg;
            try {
                StringReader sr = new StringReader(tms.getText());
                try {
                    synchronized (u) {
                        o = u.unmarshal(sr);
                    }
                } catch (JAXBException e) {
                    // nothing
                }
            } catch (JMSException e) {
                // nothing
            }
        }

        if (o != null)
            handleEventObject(o);
    }

    protected void handleEventObject(Object o) {
        RadarEventListener[] ls;
        synchronized (radarEventListeners) {
            ls = radarEventListeners
                    .toArray(new RadarEventListener[radarEventListeners.size()]);
        }
        for (RadarEventListener l : ls) {
            try {
                if (o instanceof RadarEvent)
                    l.handleRadarEvent((RadarEvent) o);
                else if (o instanceof ConfigEvent)
                    l.handleConfigEvent((ConfigEvent) o);
                else if (o instanceof NotificationEvent)
                    l.handleNotificationEvent((NotificationEvent) o);
            } catch (Exception e) {
                // TODO: ...
                e.printStackTrace(System.err);
            }
        }

    }

    @Override
    public void onException(JMSException exc) {
        shutdown(exc.getMessage());
    }

    private void onRcmException(Exception exc) {
        synchronized (stateLock) {
            state = State.UNCONNECTED;
            fatalMsg = exc.getMessage();
            stateLock.notifyAll();
        }

        RcmClientExceptionListener[] ls;
        synchronized (exceptionListeners) {
            ls = exceptionListeners
                    .toArray(new RcmClientExceptionListener[exceptionListeners
                            .size()]);
        }
        for (RcmClientExceptionListener l : ls) {
            try {
                l.onRcmException(exc);
            } catch (Exception e) {
                // TODO: ...
                e.printStackTrace(System.err);
            }
        }
    }

    protected void onReady() {
        RcmClientExceptionListener[] ls;
        synchronized (exceptionListeners) {
            ls = exceptionListeners
                    .toArray(new RcmClientExceptionListener[exceptionListeners
                            .size()]);
        }
        for (RcmClientExceptionListener l : ls) {
            try {
                l.onRcmClientReady();
            } catch (Exception e) {
                // TODO: ...
                e.printStackTrace(System.err);
            }
        }
    }

    /*
     * Should use this... public boolean isReady() { return state ==
     * State.CONNECTED; }
     */
    // TODO Auto-generated method stub
    public void shutdown() {
        shutdown("Disconnected.");
    }

    private void shutdown(String message) {
        QueueConnection qc = null;
        TopicConnection tc = null;
        synchronized (stateLock) {
            if (state != State.UNCONNECTED) {
                // if connecting ... no way to interrupt
                // factory.createqueueconnn... TODO:
                qc = queueConn;
                tc = topicConn;
                queueConn = null;
                queueSession = null;
                topicSubscriber = null;
                fatalMsg = (state == State.CONNECTED ? "Lost connection to RadarServer: "
                        : "Could not connect to RadarServer: ")
                        + message;
                state = State.UNCONNECTED;
                stateLock.notifyAll();
            } else
                return;
        }

        boolean retry;
        synchronized (radarEventListeners) {
            retry = radarEventListeners.size() > 0;
        }
        if (retry)
            retryConnect();

        HashMap<String, RcmClientHandler> hs;
        synchronized (handlers) {
            hs = new HashMap<String, RcmClientHandler>(handlers);
        }
        ReplyObj ro = new ReplyObj(fatalMsg);
        for (RcmClientHandler handler : hs.values()) {
            try {
                handler.onReply(ro);
            } catch (Exception e) {
                // nothing
            }
        }

        if (qc != null || tc != null) {
            final QueueConnection qcToClose = qc;
            final TopicConnection tcToClose = tc;
            Thread t = new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        if (qcToClose != null)
                            qcToClose.close();
                    } catch (JMSException e) {
                        e.printStackTrace(System.err); // TODO:...
                    }
                    try {
                        if (tcToClose != null)
                            tcToClose.close();
                    } catch (JMSException e) {
                        e.printStackTrace(System.err); // TODO:...
                    }
                }
            });
            t.setDaemon(true);
            t.start();
        }
    }

    public void sendRequest(final ReqObj req, final RcmClientHandler handler) {
        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    waitReady(-1);
                    sendBlocking(req, handler);
                } catch (IOException e) {
                    handleException(handler, e);
                    return;
                }
            }
        });
        t.setDaemon(true);
        t.start();
    }

    private void handleException(RcmClientHandler handler, IOException e) {
        handler.onReply(new ReplyObj(e.getMessage()));
    }

    /**
     * @param millis
     * @return true if the client is ready, false if the client is still trying
     *         to connect after the specified timeout
     * @throws IOException
     *             if a fatal error occurs while connecting
     */
    private boolean waitReady(long millis) throws IOException {
        boolean tried = false;
        long then = System.currentTimeMillis() + millis;
        synchronized (stateLock) {
            do {
                if (state == State.CONNECTED)
                    return true;
                else if (state == State.UNCONNECTED) {
                    if (!tried) {
                        tried = true;
                        start();
                    } else {
                        throw new IOException(getFatalMsg());
                    }
                }
                long left;
                if (millis > 0) {
                    left = then - System.currentTimeMillis();
                    if (left <= 0)
                        return false;
                } else
                    left = 0;
                try {
                    stateLock.wait(left);
                } catch (InterruptedException e) {
                    throw new IOException(e);
                }
            } while (true);
        }
    }

    private String getFatalMsg() {
        return fatalMsg != null ? fatalMsg : "Unknown error";
    }

    public void sendBlocking(ReqObj req, RcmClientHandler handler)
            throws IOException {
        StringWriter sw = new StringWriter();
        try {
            synchronized (m) {
                m.marshal(req, sw);
            }
        } catch (JAXBException e) {
            throw new IOException(e);
        }
        try {
            synchronized (stateLock) {
                if (state != State.CONNECTED)
                    throw new IOException("Not connected to RadarServer");
            }
            TextMessage msg = queueSession.createTextMessage(sw.toString());
            msg.setJMSReplyTo(replyQueue);
            // queueConn.stop();//cant get message id until its sent...// TODO:
            // abandoned message list?..
            queueSender.send(msg);
            handlers.put(msg.getJMSMessageID(), handler);
            // queueConn.start();
        } catch (JMSException e) {
            throw new IOException(e);
        }
    }

    private static class WaitHandler implements RcmClientHandler {
        ReplyObj reply;

        @Override
        public void onReply(ReplyObj obj) {
            synchronized (this) {
                reply = obj;
                this.notifyAll();
            }
        }
    }

    public ReplyObj sendRequest(final ReqObj req, final long timeout)
            throws IOException {

        final WaitHandler handler = new WaitHandler();

        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    if (!waitReady(timeout)) {
                        handler.onReply(null);
                        return;
                    }
                    sendBlocking(req, handler);
                } catch (IOException e) {
                    handleException(handler, e);
                    return;
                }
            }
        });
        t.setDaemon(true);

        synchronized (handler) {
            t.start();
            try {
                handler.wait(timeout * 10);
            } catch (InterruptedException e) {
                throw new IOException(e);
            }
            if (handler.reply == null)
                // TODO: or if never connected... "while waitint to connect"
                throw new IOException("Timed out waiting for reply");
        }
        return handler.reply;
    }

    private Thread retryThread;

    private void retryConnect() {
        synchronized (this) {
            if (retryThread != null)
                return;
            Thread t = new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        while (true) {
                            try {
                                Thread.sleep(4000);
                            } catch (InterruptedException e) {
                                // nothing
                            }
                            synchronized (stateLock) {
                                if (state == State.CONNECTED)
                                    return;
                            }
                            synchronized (radarEventListeners) {
                                if (radarEventListeners.size() < 1)
                                    return;
                            }
                            start();
                        }
                    } finally {
                        synchronized (RcmClient.this) {
                            retryThread = null;
                        }
                    }
                }
            });
            retryThread = t;
        }
        retryThread.setDaemon(true);
        retryThread.run();
    }

    static JAXBContext jaxbCtx;

    static Marshaller m;

    static Unmarshaller u;

    static {
        try {
            jaxbCtx = JAXBContext.newInstance(ReqObj.class, ReplyObj.class,
                    EventObj.class, RmrEvent.class);
            m = jaxbCtx.createMarshaller();
            u = jaxbCtx.createUnmarshaller();
        } catch (JAXBException e) {
            // TODO:
        }
    }

    // TODO: should use this...
    public boolean isConnected() {
        return state == State.CONNECTED;
    }

    public void setConnectionURL(String connectionURL) {
        this.connectionURL = connectionURL;
    }

    public String getConnectionURL() {
        return connectionURL;
    }
}
