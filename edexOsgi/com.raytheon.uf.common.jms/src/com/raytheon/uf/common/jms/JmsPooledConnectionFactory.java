package com.raytheon.uf.common.jms;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Session;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

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

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsPooledConnectionFactory implements ConnectionFactory {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(JmsPooledConnectionFactory.class);

    private final ConnectionFactory connFactory;

    private String provider = "QPID";

    // TODO Limit number of sessions per pool, enable lookup so that threads
    // that already have a connection/session will grab the same session
    private LinkedList<JmsPooledConnection> pooledConns = new LinkedList<JmsPooledConnection>();

    private int reconnectInterval = 30000;

    private int sessionsPerConnection = 1;

    private int sessionHoldTime = 120000;

    private int resourceRetention = 180000;

    private int maxAvailableSessions = 10;

    // TODO: Sessions need to be grouped by acknowledgement mode

    // session in use, key is "threadId-threadName"
    private HashMap<String, JmsPooledSession> inUseSessions = new HashMap<String, JmsPooledSession>(
            128);

    // sessions that were recently returned, key is "threadId-threadName"
    private HashMap<String, AvailableJmsPooledObject<JmsPooledSession>> pendingSessions = new HashMap<String, AvailableJmsPooledObject<JmsPooledSession>>();

    // sessions that have been released from pendingSessions and are awaiting
    // being closed.
    private LinkedList<AvailableJmsPooledObject<JmsPooledSession>> availableSessions = new LinkedList<AvailableJmsPooledObject<JmsPooledSession>>();

    private ConcurrentLinkedQueue<JmsPooledSession> deadSessions = new ConcurrentLinkedQueue<JmsPooledSession>();

    public JmsPooledConnectionFactory(ConnectionFactory factory) {
        this.connFactory = factory;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.ConnectionFactory#createConnection()
     */
    @Override
    public Connection createConnection() throws JMSException {
        return new JmsConnectionWrapper(this);
    }

    protected Connection createInternalConnection() throws JMSException {
        return connFactory.createConnection();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.ConnectionFactory#createConnection(java.lang.String,
     * java.lang.String)
     */
    @Override
    public Connection createConnection(String arg0, String arg1)
            throws JMSException {
        throw new IllegalArgumentException(
                "JmsPooledConnectionFactory not implemented for username/password connections");
    }

    public void returnSessionToPool(JmsPooledSession sess) {
        String threadKey = sess.getThreadKey();
        // should be able to remove from inUseSession directly, bullet proofing
        // to check for leaks in logic
        synchronized (inUseSessions) {
            JmsPooledSession tmpSess = inUseSessions.get(threadKey);
            if (tmpSess != null) {
                if (tmpSess.equals(sess)) {
                    inUseSessions.remove(threadKey);
                } else {
                    statusHandler
                            .warn("JmsPooledSession was returned to pool and another session is already in use by the same thread.  May be a leak in the pooling logic");
                }
            } else {
                statusHandler
                        .warn("JmsPooledSession was returned to pool when it was no longer an active session.  May be a leak in the pooling logic");
            }
        }

        // pending sessions shouldn't have an entry, bullet proofing to check
        // for leaks in logic
        AvailableJmsPooledObject<JmsPooledSession> prev = null;
        synchronized (pendingSessions) {
            prev = pendingSessions.put(sess.getThreadKey(),
                    new AvailableJmsPooledObject<JmsPooledSession>(sess));
        }

        if (prev != null) {
            statusHandler
                    .warn("JmsPooledSession was returned to pool and another session already existed in pending state for this thread.  May be a leak in the pooling logic");

            // shouldn't be possible unless multiple threads have the same name
            // or one thread opened multiple sessions
            prev.reset();
            prev.getPooledObject().setInUse(false);
            synchronized (availableSessions) {
                availableSessions.add(prev);
            }
        }
    }

    public boolean removeSessionFromPool(JmsPooledSession sess) {
        String threadKey = sess.getThreadKey();
        boolean found = false;

        synchronized (inUseSessions) {
            JmsPooledSession tmpSess = inUseSessions.get(threadKey);
            if (tmpSess != null) {
                if (tmpSess.equals(sess)) {
                    found = true;
                    inUseSessions.remove(threadKey);
                } else if (sess.isValid()) {
                    statusHandler
                            .info("JmsPooledSession is being removed from pool and another session already being used for this thread.  Increase pending time-out");
                }
            }
        }

        synchronized (pendingSessions) {
            // pending session may not be this session in cases where
            // multiple sessions where opened on a single thread
            AvailableJmsPooledObject<JmsPooledSession> tmpSess = pendingSessions
                    .get(threadKey);

            if (tmpSess != null) {
                JmsPooledSession pooledSess = tmpSess.getPooledObject();
                if (sess.equals(pooledSess)) {
                    found = true;
                    pendingSessions.remove(threadKey);
                } else if (sess.isValid()) {
                    statusHandler
                            .warn("JmsPooledSession is being removed from pool and another session ws already pending for this thread.  May be a leak in the pooling logic");
                }
            }
        }

        synchronized (availableSessions) {
            found = availableSessions.remove(sess);
        }

        return found;
    }

    public String getProvider() {
        return provider;
    }

    public void setProvider(String provider) {
        this.provider = provider;
    }

    public int getResourceRetention() {
        return resourceRetention;
    }

    public void setResourceRetention(int resourceRetention) {
        this.resourceRetention = resourceRetention;
    }

    public int getReconnectInterval() {
        return reconnectInterval;
    }

    public void setReconnectInterval(int reconnectInterval) {
        this.reconnectInterval = reconnectInterval;
    }

    public int getSessionsPerConnection() {
        return sessionsPerConnection;
    }

    public void setSessionsPerConnection(int sessionsPerConnection) {
        this.sessionsPerConnection = sessionsPerConnection;
    }

    public int getSessionHoldTime() {
        return sessionHoldTime;
    }

    public void setSessionHoldTime(int sessionHoldTime) {
        this.sessionHoldTime = sessionHoldTime;
    }

    private JmsPooledConnection getConnectionFromPool(String clientId) {
        JmsPooledConnection conn = null;

        if (clientId == null) {
            // grab next connection with less
            synchronized (pooledConns) {
                conn = pooledConns.peek();
            }

            if (conn == null) {
                conn = new JmsPooledConnection(this, clientId);
                synchronized (pooledConns) {
                    pooledConns.add(conn);
                }
            }
        } else {
            throw new IllegalArgumentException(
                    "Client Id not supported with pooled connections");
        }

        return conn;
    }

    public void removeConnectionFromPool(JmsPooledConnection conn) {
        synchronized (pooledConns) {
            pooledConns.remove(conn);
        }
    }

    public void returnConnectionToPool(JmsPooledConnection conn) {
        synchronized (pooledConns) {
            pooledConns.add(conn);
        }
    }

    public Session createSession(String clientId, boolean transacted,
            int acknowledgeMode) throws JMSException {
        String threadKey = "" + Thread.currentThread().getId() + "-"
                + Thread.currentThread().getName();
        JmsPooledSession session = null;

        synchronized (inUseSessions) {
            session = inUseSessions.get(threadKey);

            if (session != null) {
                session.incSessionRefs();
                statusHandler
                        .info(threadKey
                                + " already has session in use, returning previous session thread, references="
                                + session.getSessionRefs());
            }
        }

        if (session != null) {
            return new JmsSessionWrapper(session);
        }

        // check sessions by Thread
        synchronized (pendingSessions) {
            AvailableJmsPooledObject<JmsPooledSession> wrapper = pendingSessions
                    .remove(threadKey);

            // was retrieved session valid
            if (wrapper != null) {
                session = wrapper.getPooledObject();

                if (!session.isValid()) {
                    deadSessions.add(session);
                    session = null;
                }
            }
        }

        // check available sessions
        if (session == null) {
            synchronized (availableSessions) {
                AvailableJmsPooledObject<JmsPooledSession> wrapper = availableSessions
                        .poll();

                if (wrapper != null) {
                    session = wrapper.getPooledObject();
                }

                if (session != null) {
                    // was retrieved session valid
                    if (!session.isValid()) {
                        deadSessions.add(session);
                        session = null;
                    }
                }
            }

            // create new session
            while (session == null) {
                JmsPooledConnection conn = getConnectionFromPool(clientId);
                session = conn.createSession(transacted, acknowledgeMode);
            }

            session.setThreadKey(threadKey);
        }

        synchronized (inUseSessions) {
            session.incSessionRefs();
            inUseSessions.put(threadKey, session);
        }

        return new JmsSessionWrapper(session);
    }

    public void checkPooledResources() {
        long curTime = System.currentTimeMillis();
        List<AvailableJmsPooledObject<JmsPooledSession>> sessionsToProcess = new LinkedList<AvailableJmsPooledObject<JmsPooledSession>>();
        int sessionsClosed = 0;

        // grab sessions to move from pending to available
        synchronized (pendingSessions) {
            Iterator<AvailableJmsPooledObject<JmsPooledSession>> iter = pendingSessions
                    .values().iterator();
            while (iter.hasNext()) {
                AvailableJmsPooledObject<JmsPooledSession> wrapper = iter
                        .next();
                if (wrapper.expired(curTime, resourceRetention)) {
                    iter.remove();
                    sessionsToProcess.add(wrapper);
                }
            }
        }

        for (AvailableJmsPooledObject<JmsPooledSession> wrapper : sessionsToProcess) {
            wrapper.reset();
            // putting to available pool, close down consumer/producer resources
            JmsPooledSession sess = wrapper.getPooledObject();
            sess.closePooledConsumersProducers();
            sess.setInUse(false);
            sess.setThreadKey(null);

            synchronized (availableSessions) {
                availableSessions.add(wrapper);
            }
        }

        sessionsToProcess.clear();

        synchronized (availableSessions) {
            Iterator<AvailableJmsPooledObject<JmsPooledSession>> iter = availableSessions
                    .iterator();
            while (iter.hasNext()) {
                AvailableJmsPooledObject<JmsPooledSession> wrapper = iter
                        .next();
                // available sessions added based on time, so oldest is front of
                // queue
                if (wrapper.expired(curTime, sessionHoldTime)
                        || availableSessions.size() > maxAvailableSessions) {
                    // not immediately closing session so that we minimize time
                    // in sync block
                    deadSessions.add(wrapper.getPooledObject());
                    iter.remove();
                } else {
                    // sessions ordered in reverse order
                    break;
                }
            }

        }

        while (!deadSessions.isEmpty()) {
            JmsPooledSession session = deadSessions.poll();
            if (session != null) {
                session.getConnection().removeSession(session);
                sessionsClosed++;
            }
        }

        List<JmsPooledSession> sessionsToCheck = null;
        synchronized (inUseSessions) {
            sessionsToCheck = new ArrayList<JmsPooledSession>(
                    inUseSessions.values());
        }
        int consumersProducersClosed = 0;
        for (JmsPooledSession session : sessionsToCheck) {
            consumersProducersClosed += session
                    .closeOldPooledResources(resourceRetention);
        }

        // close pooled producers on pending sessions
        synchronized (pendingSessions) {
            sessionsToProcess = new ArrayList<AvailableJmsPooledObject<JmsPooledSession>>(
                    pendingSessions.values());
        }
        for (AvailableJmsPooledObject<JmsPooledSession> wrapper : sessionsToProcess) {
            consumersProducersClosed += wrapper.getPooledObject()
                    .closeOldPooledResources(resourceRetention);
        }

        if (sessionsClosed > 0 || consumersProducersClosed > 0) {
            statusHandler.handle(Priority.INFO,
                    "Closed unused jms pooled resources: sessions closed: "
                            + sessionsClosed + ", consumers/producers closed: "
                            + consumersProducersClosed + ", total time "
                            + (System.currentTimeMillis() - curTime));
        }
    }
}
