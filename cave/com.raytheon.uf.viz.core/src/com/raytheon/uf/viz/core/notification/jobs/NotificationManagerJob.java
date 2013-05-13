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
package com.raytheon.uf.viz.core.notification.jobs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

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
import javax.jms.Topic;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.services.IDisposable;
import org.eclipse.ui.statushandlers.StatusManager;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.comm.JMSConnection;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationMessage;

/**
 * Job to monitor the JMS topic containing notification messages, delegating the
 * messages to the appropriate listeners. Listener execution is performed in
 * isolated threads, so users may perform appropriate operations inside the
 * listener method.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/08/08		1127		randerso    Initial Creation
 * 09/03/08     1448        chammack    Refactored notification observer interface
 * 04/23/13     1939        randerso    Add separate connect method to allow application
 *                                      to complete initialization before connecting to JMS
 * </pre>
 * 
 * @author randerso
 * @version 1
 */

public class NotificationManagerJob implements ExceptionListener, IDisposable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NotificationManagerJob.class, "CAVE");

    private static final int IN_MEM_MESSAGE_LIMIT = 5000;

    private static class ListenerKey {
        private final String topic;

        private final String queryString;

        /**
         * @param topic
         * @param queryString
         */
        public ListenerKey(String topic, String queryString) {
            super();
            this.topic = topic;
            this.queryString = queryString;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((queryString == null) ? 0 : queryString.hashCode());
            result = prime * result + ((topic == null) ? 0 : topic.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            ListenerKey other = (ListenerKey) obj;
            if (queryString == null) {
                if (other.queryString != null) {
                    return false;
                }
            } else if (!queryString.equals(other.queryString)) {
                return false;
            }
            if (topic == null) {
                if (other.topic != null) {
                    return false;
                }
            } else if (!topic.equals(other.topic)) {
                return false;
            }
            return true;
        }
    }

    private static enum Type {
        QUEUE, TOPIC;
    }

    /** The singleton instance */
    private static NotificationManagerJob instance;

    /** The observer map of topic to listeners */
    protected Map<ListenerKey, NotificationListener> listeners;

    private Connection connection;

    private boolean connected = false;

    /**
     * Get the active subscription manager job. If one does not exist, start an
     * instance as a system job.
     * 
     * @return the subscription manager
     */
    protected static synchronized NotificationManagerJob getInstance() {
        if (instance == null) {
            instance = new NotificationManagerJob();
        }
        return instance;
    }

    protected static synchronized void setCustomInstance(
            NotificationManagerJob notificationManagerJob) {
        // TODO what to do if there already is an instance.
        instance = notificationManagerJob;
    }

    /**
     * @param name
     */
    protected NotificationManagerJob() {
        this.listeners = new HashMap<ListenerKey, NotificationListener>();
        Activator.getDefault().registerDisposable(this);
    }

    protected void connect(boolean notifyError) {
        boolean successful = true;
        try {
            ConnectionFactory connectionFactory = JMSConnection.getInstance()
                    .getFactory();
            disconnect(notifyError);

            // Create a Connection
            connection = connectionFactory.createConnection();
            connection.setExceptionListener(this);
            // connection.setClientID(VizApp.getWsId().toString());

            connection.start();
            connected = true;
            for (NotificationListener listener : listeners.values()) {
                try {
                    listener.setupConnection(this);
                } catch (JMSException e) {
                    successful = false;
                    if (notifyError) {
                        statusHandler
                                .handle(Priority.SIGNIFICANT,
                                        "NotificationManager failed to setup message listener.",
                                        e);
                    }
                }
            }
        } catch (JMSException e) {
            if (notifyError) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "NotificationManager failed to connect.", e);
            }
            successful = false;
        }

        if (!successful) {
            onException(null);
        }
    }

    protected void disconnect(boolean notifyError) {
        if (connection != null) {
            try {
                connection.stop();
            } catch (Exception e) {
                if (notifyError) {
                    statusHandler.handle(Priority.SIGNIFICANT,
                            "NotificationManager failed to stop a connection.",
                            e);
                }
            }
            try {
                connection.close();
            } catch (Exception e) {
                if (notifyError) {
                    statusHandler
                            .handle(Priority.SIGNIFICANT,
                                    "NotificationManager failed to close a connection.",
                                    e);
                }
            }
            connection = null;
        }
    }

    private TimerTask task = null;

    @Override
    public void onException(JMSException e) {
        connected = false;
        if (task == null) {
            task = new TimerTask() {
                @Override
                public void run() {
                    NotificationManagerJob.this.task = null;
                    connect(false);
                }
            };
            // needs to be configurable, currently 5 second reconnect
            new Timer().schedule(task, 5 * 1000);
        }

        // Reset connected bool
        for (NotificationListener listener : listeners.values()) {
            listener.connected = false;
        }

        if (e != null) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error in JMS connectivity: " + e.getLocalizedMessage(), e);
        }
    }

    public static synchronized void addQueueObserver(String queue,
            INotificationObserver obs) {
        addQueueObserver(queue, obs, null);
    }

    public static synchronized void addQueueObserver(String queue,
            INotificationObserver obs, String queryString) {
        NotificationManagerJob notifMgr = getInstance();
        ListenerKey key = new ListenerKey(queue, queryString);
        NotificationListener listener = notifMgr.listeners.get(key);
        if (listener == null || listener.consumer == null) {
            try {
                listener = new NotificationListener(queue, queryString,
                        Type.QUEUE);
                notifMgr.listeners.put(key, listener);
                listener.addObserver(obs);
                if (notifMgr.connected) {
                    listener.setupConnection(notifMgr);
                }
            } catch (JMSException e) {
                Status s = new Status(IStatus.ERROR, Activator.PLUGIN_ID, 0,
                        "NotificationManager failed to create queue consumer.",
                        e);
                StatusManager.getManager().handle(s);
            }
        } else {
            listener.addObserver(obs);
        }
    }

    /**
     * Register for notification messages using a callback.
     * 
     * @param topic
     *            message topic
     * @param obs
     *            the alert observer callback
     */
    public static synchronized void addObserver(String topic,
            INotificationObserver obs) {
        addObserver(topic, obs, null);
    }

    public static synchronized void addObserver(String topic,
            INotificationObserver obs, String queryString) {
        NotificationManagerJob notifMgr = getInstance();
        ListenerKey key = new ListenerKey(topic, queryString);
        NotificationListener listener = notifMgr.listeners.get(key);
        if (listener == null) {
            try {
                listener = new NotificationListener(topic, queryString,
                        Type.TOPIC);
                notifMgr.listeners.put(key, listener);
                listener.addObserver(obs);
                if (notifMgr.connected) {
                    listener.setupConnection(notifMgr);
                }
            } catch (JMSException e) {
                Status s = new Status(IStatus.ERROR, Activator.PLUGIN_ID, 0,
                        "NotificationManager failed to create consumer.", e);
                StatusManager.getManager().handle(s);
            }
        } else {
            listener.addObserver(obs);
        }
    }

    /**
     * Removes an alert message observer that was registered using the
     * addObserver method. This must be called in exactly the same
     * 
     * @param topic
     *            message topic
     * @param obs
     *            the observer to remove
     */
    public static synchronized void removeObserver(String topic,
            INotificationObserver obs) {
        removeObserver(topic, obs, null);
    }

    /**
     * Removes an alert message observer that was registered using the
     * addObserver method. This must be called in exactly the same
     * 
     * @param topic
     *            message topic
     * @param obs
     *            the observer to remove
     * @param queryString
     */
    public static synchronized void removeObserver(String topic,
            INotificationObserver obs, String queryString) {
        NotificationManagerJob notifMgr = getInstance();
        ListenerKey key = new ListenerKey(topic, queryString);
        NotificationListener listener = notifMgr.listeners.get(key);
        if (listener == null) {
            return;
        }
        listener.removeObserver(obs);
        if (listener.size() <= 0) {
            listener.disconnect();
            notifMgr.listeners.remove(key);
        }
    }

    /**
     * Removes an alert message observer that was registered using the
     * addObserver method. This must be called in exactly the same
     * 
     * @param topic
     *            message topic
     * @param obs
     *            the observer to remove
     */
    public static synchronized void removeQueueObserver(String queue,
            String queryString, INotificationObserver obs) {
        NotificationManagerJob notifMgr = getInstance();
        ListenerKey key = new ListenerKey(queue, queryString);
        NotificationListener listener = notifMgr.listeners.get(key);
        if (listener == null) {
            return;
        }
        listener.removeObserver(obs);
        if (listener.size() <= 0) {
            listener.disconnect();
            notifMgr.listeners.remove(key);
        }
    }

    /**
     * Connect to JMS
     */
    public static void connect() {
        getInstance().connect(true);
    }

    /**
     * Disconnect from JMS
     */
    public static void disconnect() {
        getInstance().disconnect(true);
    }

    private static class NotificationListener implements MessageListener {

        private Type type;

        private String id;

        private String queryString;

        /** The list of interested parties */
        protected List<INotificationObserver> observers;

        /** The map of job threads from observers */
        protected Map<INotificationObserver, JobWrapper> jobWrappers;

        protected MessageConsumer consumer;

        protected Session session;

        protected boolean connected = false;

        public NotificationListener(String id, String queryString, Type type) {
            this.observers = new ArrayList<INotificationObserver>();
            this.jobWrappers = new HashMap<INotificationObserver, JobWrapper>();
            this.type = type;
            this.id = id;
            this.queryString = queryString;
        }

        private void setupConnection(NotificationManagerJob manager)
                throws JMSException {
            switch (type) {
            case QUEUE: {
                setupQueue(manager);
                break;
            }
            case TOPIC: {
                setupTopic(manager);
                break;
            }
            }
            // If we made it here we are good
            connected = true;
        }

        public void disconnect() {
            if (connected) {
                if (consumer != null) {
                    try {
                        consumer.close();
                    } catch (JMSException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error closing consumer connection", e);
                    }
                    consumer = null;
                }

                if (session != null) {
                    try {
                        session.close();
                    } catch (JMSException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error closing session", e);
                    }
                    session = null;
                }
                connected = false;
            }
        }

        private void setupQueue(NotificationManagerJob manager)
                throws JMSException {
            disconnect();
            session = manager.connection.createSession(false,
                    Session.AUTO_ACKNOWLEDGE);
            if (session != null) {
                String queueName = id;
                Queue t = session.createQueue(queueName);

                if (queryString != null) {
                    consumer = session.createConsumer(t, queryString);
                } else {
                    consumer = session.createConsumer(t);
                }

                setConsumer(consumer);
                consumer.setMessageListener(this);
            }
        }

        private void setupTopic(NotificationManagerJob manager)
                throws JMSException {
            disconnect();
            session = manager.connection.createSession(false,
                    Session.AUTO_ACKNOWLEDGE);
            if (session != null) {
                String topicName = id;
                Topic t = session.createTopic(topicName);

                if (queryString != null) {
                    consumer = session.createConsumer(t, queryString);
                } else {
                    consumer = session.createConsumer(t);
                }

                consumer.setMessageListener(this);
            }
        }

        public void setConsumer(MessageConsumer consumer) {
            this.consumer = consumer;
        }

        /*
         * (non-Javadoc)
         * 
         * @see javax.jms.MessageListener#onMessage(javax.jms.Message)
         */
        @Override
        public void onMessage(Message msg) {
            if (observers == null) {
                return;
            }

            for (INotificationObserver obs : observers) {
                sendToObserver(obs, msg);
            }

            // Iterator<JobWrapper> iterator = jobWrappers.values().iterator();
            // while (iterator.hasNext()) {
            // JobWrapper wrapper = iterator.next();
            // if (!wrapper.isEmpty() && wrapper.getState() != Job.RUNNING) {
            // wrapper.schedule();
            // }
            // }
        }

        public synchronized void addObserver(INotificationObserver obs) {
            observers.add(obs);
        }

        public synchronized void removeObserver(INotificationObserver obs) {
            observers.remove(obs);
        }

        protected void sendToObserver(INotificationObserver observer,
                Message msg) {
            // Get the corresponding job, creating the
            // wrapper if necessary
            JobWrapper wrapper = null;
            synchronized (this) {
                wrapper = jobWrappers.get(observer);
                if (wrapper == null) {
                    wrapper = new JobWrapper(observer);
                    jobWrappers.put(observer, wrapper);
                }
            }
            wrapper.put(msg);
        }

        public int size() {
            return observers.size();
        }
    }

    /**
     * Provides internal insulation from slow-running processes by forming a
     * queue
     * 
     */
    private static class JobWrapper extends Job {

        protected java.util.Queue<Message> messages;

        protected INotificationObserver observer;

        protected long lastErrorPrintTime = 0;

        protected AtomicInteger messageCount;

        public JobWrapper(INotificationObserver observer) {
            super("Alert thread for " + observer);
            this.setSystem(true);
            this.observer = observer;
            this.messages = new ConcurrentLinkedQueue<Message>();
            this.messageCount = new AtomicInteger(0);
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            List<NotificationMessage> messageList = new ArrayList<NotificationMessage>();
            // one error log messages per execution of job
            boolean errorLogged = false;
            while (!this.messages.isEmpty()) {

                while (!this.messages.isEmpty()) {
                    messageCount.decrementAndGet();
                    Message msg = messages.remove();

                    if (!(msg instanceof BytesMessage)) {
                        final Status s = new Status(Status.ERROR,
                                Activator.PLUGIN_ID,
                                "Incoming message was not a binary message as expected");
                        StatusManager.getManager().handle(s);
                    } else {
                        messageList.add(new NotificationMessage(msg));
                    }
                }

                try {
                    observer.notificationArrived(messageList
                            .toArray(new NotificationMessage[messageList.size()]));
                } catch (Throwable e) {
                    if (!errorLogged) {
                        final Status s = new Status(Status.ERROR,
                                Activator.PLUGIN_ID,
                                "Error occurred during alert processing", e);
                        StatusManager.getManager().handle(s);
                        errorLogged = true;
                    }
                }
                messageList.clear();
            }

            return Status.OK_STATUS;
        }

        /**
         * Add the message to the queue
         * 
         * @param msg
         *            the msg
         */
        public void put(Message msg) {
            messages.offer(msg);
            if (messageCount.incrementAndGet() > IN_MEM_MESSAGE_LIMIT) {
                messageCount.decrementAndGet();
                messages.remove();
                if (System.currentTimeMillis() - lastErrorPrintTime > 600000) {
                    final Status s = new Status(
                            Status.ERROR,
                            Activator.PLUGIN_ID,
                            "Message queue size exceeded for observer "
                                    + observer
                                    + ", old messages will be replaced by incoming messages.");
                    StatusManager.getManager().handle(s);
                    lastErrorPrintTime = System.currentTimeMillis();
                }
            }
            this.schedule();
        }

        /**
         * Checks if is empty.
         * 
         * @return true, if is empty
         */
        public boolean isEmpty() {
            return this.messages.isEmpty();
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.services.IDisposable#dispose()
     */
    @Override
    public void dispose() {
        for (NotificationListener listener : listeners.values()) {
            listener.disconnect();
        }
        listeners.clear();
    }
}
