package com.raytheon.uf.common.jms;

import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;

import com.raytheon.uf.common.jms.wrapper.JmsConnectionWrapper;
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

    // connections in use, key is "threadId-threadName"
    private final Map<String, JmsPooledConnection> inUseConnections = new HashMap<String, JmsPooledConnection>();

    // connections that were recently returned, key is "threadId-threadName"
    private final Map<String, AvailableJmsPooledObject<JmsPooledConnection>> pendingConnections = new HashMap<String, AvailableJmsPooledObject<JmsPooledConnection>>();

    // connections that have been released from pendingConnections and are
    // awaiting being closed.
    private final Deque<AvailableJmsPooledObject<JmsPooledConnection>> availableConnections = new LinkedList<AvailableJmsPooledObject<JmsPooledConnection>>();

    private final ConcurrentLinkedQueue<JmsPooledConnection> deadConnections = new ConcurrentLinkedQueue<JmsPooledConnection>();

    private int reconnectInterval = 30000;

    private int connectionHoldTime = 120000;

    private int resourceRetention = 180000;

    private int maxSpareConnections = 10;

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
        String threadKey = "" + Thread.currentThread().getId() + "-"
                + Thread.currentThread().getName();
        JmsPooledConnection conn = null;

        synchronized (inUseConnections) {
            conn = inUseConnections.get(threadKey);

            if (conn != null) {
                JmsConnectionWrapper ref = conn.createReference();
                if (ref != null) {
                    statusHandler
                            .info(threadKey
                                    + " already has a connection in use, returning previous connection thread, references="
                                    + conn.getReferenceCount());
                    return ref;
                } else {
                    deadConnections.add(conn);
                    inUseConnections.remove(threadKey);
                    conn = null;
                }
            }
        }

        AvailableJmsPooledObject<JmsPooledConnection> wrapper = null;

        // check connections by Thread
        synchronized (pendingConnections) {
            wrapper = pendingConnections.remove(threadKey);
        }

        // was retrieved connection valid
        if (wrapper != null) {
            conn = wrapper.getPooledObject();
            JmsConnectionWrapper ref = getConnectionWrapper(threadKey, conn);

            if (ref != null) {
                return ref;
            } else {
                deadConnections.add(conn);
                conn = null;
            }
        }

        // check available connections
        boolean keepChecking = true;

        while (keepChecking) {
            synchronized (availableConnections) {
                wrapper = availableConnections.poll();
            }

            if (wrapper != null) {
                conn = wrapper.getPooledObject();
            } else {
                keepChecking = false;
            }

            if (conn != null) {
                // was retrieved connection valid
                JmsConnectionWrapper ref = getConnectionWrapper(threadKey, conn);

                if (ref != null) {
                    return ref;
                } else {
                    deadConnections.add(conn);
                    conn = null;
                }
            }
        }

        // create new connection?
        if (conn == null) {
            conn = new JmsPooledConnection(this);
        }

        return getConnectionWrapper(threadKey, conn);
    }

    private JmsConnectionWrapper getConnectionWrapper(String threadKey,
            JmsPooledConnection conn) {
        synchronized (conn.getStateLock()) {
            if (conn.isValid()) {
                conn.setState(State.InUse);
                JmsConnectionWrapper ref = conn.createReference();
                if (ref != null) {
                    conn.setKey(threadKey);
                    synchronized (inUseConnections) {
                        inUseConnections.put(threadKey, conn);
                    }
                    return ref;
                }
            }
        }

        return null;
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

    public void removeConnectionFromPool(JmsPooledConnection conn) {
        String threadKey = conn.getKey();
        boolean success = false;

        // remove it from inUseConnections if it was in use, theoretically could
        // go by connection state, but may miss something due to threading
        synchronized (inUseConnections) {
            JmsPooledConnection inUse = inUseConnections.remove(threadKey);

            // make sure the one we removed is indeed this connection, 99%
            // of time this is correct
            success = inUse == conn;

            if (success) {
                // found conn, done
                return;
            } else {
                // put the bad removal back in. Done this way instead of
                // get/remove as 99% of time remove is correct, this
                // really only here for bullet proofing code against bad
                // use of pool
                if (inUse != null) {
                    inUseConnections.put(threadKey, inUse);
                }
            }
        }

        // remove it from pendingConnections
        AvailableJmsPooledObject<JmsPooledConnection> pooledObj = null;
        synchronized (pendingConnections) {
            pooledObj = pendingConnections.remove(threadKey);
            if (pooledObj != null) {
                if (pooledObj.getPooledObject() == conn) {
                    // found conn, done
                    return;
                } else {
                    pendingConnections.put(threadKey, pooledObj);
                }
            }
        }

        // remove it from availableConnections
        synchronized (availableConnections) {
            availableConnections.remove(conn);
        }
    }

    public boolean returnConnectionToPool(JmsPooledConnection conn) {
        boolean success = false;
        String threadKey = conn.getKey();

        synchronized (inUseConnections) {
            JmsPooledConnection inUse = inUseConnections.remove(threadKey);

            // make sure the one we removed is indeed this connection, 99%
            // of time this is correct
            success = inUse == conn;

            if (!success) {
                // put the bad removal back in. Done this way instead of
                // get/remove as 99% of time remove is correct, this
                // really only here for bullet proofing code against bad
                // use of pool
                if (inUse != null) {
                    inUseConnections.put(threadKey, inUse);
                    statusHandler
                            .handle(Priority.INFO,
                                    "Another connection already in use for this thread, not returning this connection to pool");
                }
            }
        }

        if (success && conn.isValid()) {
            // only put it in the available pool if it was successfully
            // remove from the inUse pool
            AvailableJmsPooledObject<JmsPooledConnection> prev = null;
            synchronized (pendingConnections) {
                prev = pendingConnections
                        .put(threadKey,
                                new AvailableJmsPooledObject<JmsPooledConnection>(
                                        conn));
            }
            if (prev != null) {
                // there was a previous connection registered to
                // this thread, move it to available
                statusHandler
                        .handle(Priority.WARN,
                                "Another connection already pooled for this thread, moving previous connection to available");
                prev.reset();
                synchronized (availableConnections) {
                    availableConnections.add(prev);
                }
            }
        } else {
            success = false;
        }

        return success;
    }

    public void checkPooledResources() {
        long curTime = System.currentTimeMillis();
        List<AvailableJmsPooledObject<JmsPooledConnection>> connectionsToProcess = new LinkedList<AvailableJmsPooledObject<JmsPooledConnection>>();
        int connectionsClosed = 0;

        // grab connections to move from pending to available
        synchronized (pendingConnections) {
            Iterator<AvailableJmsPooledObject<JmsPooledConnection>> iter = pendingConnections
                    .values().iterator();
            while (iter.hasNext()) {
                AvailableJmsPooledObject<JmsPooledConnection> wrapper = iter
                        .next();
                if (wrapper.expired(curTime, resourceRetention)) {
                    iter.remove();
                    connectionsToProcess.add(wrapper);
                }
            }
        }

        synchronized (availableConnections) {
            for (AvailableJmsPooledObject<JmsPooledConnection> wrapper : connectionsToProcess) {
                wrapper.reset();

                // putting to available pool
                JmsPooledConnection conn = wrapper.getPooledObject();
                conn.setKey(null);
                availableConnections.add(wrapper);
            }
        }

        connectionsToProcess.clear();

        synchronized (availableConnections) {
            Iterator<AvailableJmsPooledObject<JmsPooledConnection>> iter = availableConnections
                    .iterator();
            while (iter.hasNext()) {
                AvailableJmsPooledObject<JmsPooledConnection> wrapper = iter
                        .next();
                // available sessions added based on time, so oldest is front of
                // queue
                if (wrapper.expired(curTime, connectionHoldTime)
                        || (availableConnections.size() > maxSpareConnections)) {
                    // not immediately closing connection so that we minimize
                    // time in sync block
                    deadConnections.add(wrapper.getPooledObject());
                    iter.remove();
                } else {
                    // connections ordered in reverse order
                    break;
                }
            }

        }

        while (!deadConnections.isEmpty()) {
            JmsPooledConnection conn = deadConnections.poll();
            if (conn != null) {
                conn.close();
                connectionsClosed++;
            }
        }

        List<JmsPooledConnection> connectionsToCheck = null;
        synchronized (inUseConnections) {
            connectionsToCheck = new ArrayList<JmsPooledConnection>(
                    inUseConnections.values());
        }
        int resourcesClosed = 0;
        for (JmsPooledConnection conn : connectionsToCheck) {
            resourcesClosed += conn.closeOldPooledResources(resourceRetention);
        }

        // close pooled resources on pending connections
        synchronized (pendingConnections) {
            connectionsToProcess = new ArrayList<AvailableJmsPooledObject<JmsPooledConnection>>(
                    pendingConnections.values());
        }

        synchronized (availableConnections) {
            connectionsToProcess.addAll(availableConnections);
        }

        for (AvailableJmsPooledObject<JmsPooledConnection> wrapper : connectionsToProcess) {
            resourcesClosed += wrapper.getPooledObject()
                    .closeOldPooledResources(resourceRetention);
        }

        if ((connectionsClosed > 0) || (resourcesClosed > 0)) {
            statusHandler.handle(
                    Priority.INFO,
                    "Closed unused jms pooled resources: connections closed: "
                            + connectionsClosed + ", other resources closed: "
                            + resourcesClosed + ", total time "
                            + (System.currentTimeMillis() - curTime));
        }
    }

    /**
     * @return the connectionHoldTime
     */
    public int getConnectionHoldTime() {
        return connectionHoldTime;
    }

    /**
     * @param connectionHoldTime
     *            the connectionHoldTime to set
     */
    public void setConnectionHoldTime(int connectionHoldTime) {
        this.connectionHoldTime = connectionHoldTime;
    }

    /**
     * @return the maxSpareConnections
     */
    public int getMaxSpareConnections() {
        return maxSpareConnections;
    }

    /**
     * @param maxSpareConnections
     *            the maxSpareConnections to set
     */
    public void setMaxSpareConnections(int maxSpareConnections) {
        this.maxSpareConnections = maxSpareConnections;
    }
}
