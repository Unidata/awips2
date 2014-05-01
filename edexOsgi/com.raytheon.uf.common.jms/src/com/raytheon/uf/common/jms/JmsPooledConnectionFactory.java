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
package com.raytheon.uf.common.jms;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
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
 * Connection Factory that keep underlying JMS resources used by a thread open
 * for re-use by that same thread. This is to get around Spring's opening and
 * closing the full jms stack for each message. We cannot use Spring caching
 * mechanism since it requires one connection object to be used for all jms
 * sessions. In that scenario one error causes every jms resource to disconnect
 * and has been known to dead lock in the qpid code.
 * 
 * The close action puts the resource into a pool for reuse. The jms resource
 * may only be reused by the same thread. This is in part since each thread
 * always connects to the same set of jms resources. Also on the QPID broker
 * transient data is only removed when the session itself is closed. So reusing
 * a resource on a different thread can cause transient topic resources with no
 * consumers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2011            rjpeter     Initial creation
 * Oct 04, 2013 2357       rjpeter     Removed pooling, keeps resources open for the
 *                                     thread that created them for a configured amount of time.
 * Feb 07, 2014 2357       rjpeter     Track by Thread object, periodly check that tracked Threads are still alive.
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

    // connections in use
    private final Map<Thread, JmsPooledConnection> inUseConnections = new HashMap<Thread, JmsPooledConnection>();

    // connections that were recently returned
    private final Map<Thread, AvailableJmsPooledObject<JmsPooledConnection>> pendingConnections = new HashMap<Thread, AvailableJmsPooledObject<JmsPooledConnection>>();

    private final ConcurrentLinkedQueue<JmsPooledConnection> deadConnections = new ConcurrentLinkedQueue<JmsPooledConnection>();

    private int reconnectInterval = 30000;

    private int resourceRetention = 180000;

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
        Thread thread = Thread.currentThread();
        JmsPooledConnection conn = null;

        synchronized (inUseConnections) {
            conn = inUseConnections.get(thread);

            if (conn != null) {
                JmsConnectionWrapper ref = conn.createReference();
                if (ref != null) {
                    statusHandler
                            .info(thread.getName()
                                    + " already has a connection in use, returning previous connection thread, references="
                                    + conn.getReferenceCount());
                    return ref;
                } else {
                    deadConnections.add(conn);
                    inUseConnections.remove(thread);
                    conn = null;
                }
            }
        }

        AvailableJmsPooledObject<JmsPooledConnection> wrapper = null;

        // check connections by Thread
        synchronized (pendingConnections) {
            wrapper = pendingConnections.remove(thread);
        }

        // was retrieved connection valid
        if (wrapper != null) {
            conn = wrapper.getPooledObject();
            JmsConnectionWrapper ref = getConnectionWrapper(conn);

            if (ref != null) {
                return ref;
            } else {
                deadConnections.add(conn);
                conn = null;
            }
        }

        // create new connection?
        if (conn == null) {
            conn = new JmsPooledConnection(this, thread);
        }

        return getConnectionWrapper(conn);
    }

    private JmsConnectionWrapper getConnectionWrapper(JmsPooledConnection conn) {
        synchronized (conn.getStateLock()) {
            if (conn.isValid()) {
                conn.setState(State.InUse);
                JmsConnectionWrapper ref = conn.createReference();
                if (ref != null) {
                    synchronized (inUseConnections) {
                        inUseConnections.put(conn.getThread(), conn);
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
        Thread thread = conn.getThread();
        boolean success = false;

        // remove it from inUseConnections if it was in use, theoretically could
        // go by connection state, but may miss something due to threading
        synchronized (inUseConnections) {
            JmsPooledConnection inUse = inUseConnections.remove(thread);

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
                    inUseConnections.put(thread, inUse);
                }
            }
        }

        // remove it from pendingConnections
        AvailableJmsPooledObject<JmsPooledConnection> pooledObj = null;
        synchronized (pendingConnections) {
            pooledObj = pendingConnections.remove(thread);
            if (pooledObj != null) {
                if (pooledObj.getPooledObject() == conn) {
                    // found conn, done
                    return;
                } else {
                    pendingConnections.put(thread, pooledObj);
                }
            }
        }
    }

    public boolean returnConnectionToPool(JmsPooledConnection conn) {
        boolean success = false;
        Thread thread = conn.getThread();

        synchronized (inUseConnections) {
            JmsPooledConnection inUse = inUseConnections.remove(thread);

            // make sure the one we removed is indeed this connection, 99%
            // of time this is correct
            success = inUse == conn;

            if (!success) {
                // put the bad removal back in. Done this way instead of
                // get/remove as 99% of time remove is correct, this
                // really only here for bullet proofing code against bad
                // use of pool
                if (inUse != null) {
                    inUseConnections.put(thread, inUse);
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
                        .put(thread,
                                new AvailableJmsPooledObject<JmsPooledConnection>(
                                        conn));
            }
            if ((prev != null) && (prev.getPooledObject() != conn)) {
                // there was a previous connection registered to
                // this thread, close it
                statusHandler
                        .handle(Priority.WARN,
                                "Another connection already pooled for this thread, closing previous connection");
                deadConnections.add(prev.getPooledObject());
            }
        } else {
            success = false;
        }

        return success;
    }

    public void checkPooledResources() {
        int connectionsClosed = 0;

        long curTime = System.currentTimeMillis();

        // check for connections to close
        synchronized (pendingConnections) {
            Iterator<AvailableJmsPooledObject<JmsPooledConnection>> iter = pendingConnections
                    .values().iterator();
            while (iter.hasNext()) {
                AvailableJmsPooledObject<JmsPooledConnection> wrapper = iter
                        .next();
                if (wrapper.expired(curTime, resourceRetention)) {
                    iter.remove();
                    deadConnections.add(wrapper.getPooledObject());
                }
            }
        }

        // check for dead threads
        synchronized (inUseConnections) {
            Iterator<Map.Entry<Thread, JmsPooledConnection>> iter = inUseConnections
                    .entrySet().iterator();
            while (iter.hasNext()) {
                Map.Entry<Thread, JmsPooledConnection> entry = iter.next();
                if (!entry.getKey().isAlive()) {
                    iter.remove();
                    deadConnections.add(entry.getValue());
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

        ArrayList<JmsPooledConnection> connectionsToCheck = null;
        synchronized (inUseConnections) {
            connectionsToCheck = new ArrayList<JmsPooledConnection>(
                    inUseConnections.values());
        }

        int resourcesClosed = 0;
        for (JmsPooledConnection conn : connectionsToCheck) {
            resourcesClosed += conn.closeOldPooledResources(resourceRetention);
        }
        connectionsToCheck.clear();

        // close pooled resources on pending connections
        synchronized (pendingConnections) {
            connectionsToCheck.ensureCapacity(pendingConnections.size());
            for (AvailableJmsPooledObject<JmsPooledConnection> wrapper : pendingConnections
                    .values()) {
                connectionsToCheck.add(wrapper.getPooledObject());
            }
        }

        for (JmsPooledConnection conn : connectionsToCheck) {
            resourcesClosed += conn.closeOldPooledResources(resourceRetention);
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

}
