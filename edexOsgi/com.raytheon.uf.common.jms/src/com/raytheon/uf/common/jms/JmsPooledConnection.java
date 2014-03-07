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
import java.util.List;

import javax.jms.Connection;
import javax.jms.ConnectionMetaData;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Session;

import com.raytheon.uf.common.jms.wrapper.JmsConnectionWrapper;
import com.raytheon.uf.common.jms.wrapper.JmsSessionWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Jms Pooled Connection. Tracks references to the connection to know when the
 * connection can be released to the pool. Any exception will close pooled
 * session instead of returning to the pool. The sessions are tracked in both
 * active and available states. An available session can be reused by the next
 * client. The connection is pinned to Thread that creates the connection and
 * cannot be used/reused by any other thread.
 * 
 * Synchronization Principle To prevent deadlocks: Chained sync blocks can only
 * happen in a downward direction. A manager has a synchronized lock can make a
 * call down to a wrapper, but not nice versa. Also a session inside a sync
 * block can make a call down to a producer but not vice versa.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2011            rjpeter     Initial creation.
 * Mar 08, 2012 194        njensen     Improved safety of close().
 * Feb 21, 2013 1642       rjpeter     Fix deadlock scenario.
 * Feb 07, 2014 2357       rjpeter     Track by Thread, close session is it has no
 *                                     producers/consumers.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class JmsPooledConnection implements ExceptionListener {
    private static final long ERROR_BROADCAST_INTERVAL = 30000;

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(JmsPooledConnection.class);

    private final JmsPooledConnectionFactory connFactory;

    private volatile Connection conn = null;

    // keeps track of number of creates vs. closes to know when it can be
    // returned to the pool
    private final List<JmsConnectionWrapper> references = new ArrayList<JmsConnectionWrapper>(
            1);

    private final Object stateLock = new Object();

    private volatile State state = State.InUse;

    // technically can have multiple sessions to one connection and sessions can
    // differ by transaction mode and acknowledgement mode, currently not
    // supported
    private volatile JmsPooledSession session = null;

    private volatile AvailableJmsPooledObject<JmsPooledSession> availableSession = null;

    private final Thread thread;

    private final String clientId;

    private volatile long connectionStartTime = 0;

    private volatile boolean exceptionOccurred = false;

    public JmsPooledConnection(JmsPooledConnectionFactory connFactory,
            Thread thread) {
        this.connFactory = connFactory;
        this.thread = thread;
        this.clientId = null;
        getConnection();
    }

    public JmsSessionWrapper getSession(boolean transacted, int acknowledgeMode)
            throws JMSException {
        // pooled objects are always valid, the underlying object may not be
        if (!isValid()) {
            // throw exception
            throw new IllegalStateException("Connection is closed");
        }

        synchronized (stateLock) {
            // TODO: Add multiple session support
            if (session != null) {
                JmsSessionWrapper ref = session.createReference();
                if (ref != null) {
                    return ref;
                } else {
                    this.session.close();
                    this.session = null;
                }
            }

            if (availableSession != null) {
                JmsPooledSession availSess = availableSession.getPooledObject();
                synchronized (availSess.getStateLock()) {
                    if (availSess.isValid()
                            && availSess.hasProducersOrConsumers()) {
                        availSess.setState(State.InUse);
                        session = availSess;
                    } else {
                        availSess.close();
                    }
                    availableSession = null;
                }
            }

            if (session == null) {
                session = new JmsPooledSession(this, conn.createSession(
                        transacted, acknowledgeMode));
            }
        }

        return session.createReference();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#getClientID()
     */
    public String getClientID() throws JMSException {
        return clientId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#getMetaData()
     */
    public ConnectionMetaData getMetaData() throws JMSException {
        return getConnection().getMetaData();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.ExceptionListener#onException(javax.jms.JMSException)
     */
    @Override
    public void onException(JMSException jmsExc) {
        // need to worry about multiple exceptions causing repeated
        // disconnect/reconnect
        statusHandler.handle(Priority.WARN, "Caught Exception on "
                + connFactory.getProvider()
                + " connection.  Closing connection", jmsExc);
        close();
    }

    public void close() {
        boolean canClose = false;
        synchronized (stateLock) {
            if (!State.Closed.equals(state)) {
                canClose = true;
                state = State.Closed;
            }
        }

        if (canClose) {
            statusHandler.info("Closing connection: " + this.toString());
            // njensen: I moved removing the connection from the pool to be
            // the first thing in this block instead of last thing so
            // there's no chance it could be closed and then retrieved from
            // the pool by something else
            connFactory.removeConnectionFromPool(this);
            if (conn != null) {
                try {
                    conn.stop();
                } catch (Exception e) {
                    statusHandler.handle(Priority.WARN,
                            "Failed to stop connection", e);
                }
            }

            synchronized (references) {
                for (JmsConnectionWrapper wrapper : references) {
                    wrapper.closeWrapper();
                }
                references.clear();
            }

            if (session != null) {
                session.close();
                session = null;
            }

            if (availableSession != null) {
                availableSession.getPooledObject().close();
                availableSession = null;
            }

            try {
                conn.close();
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN,
                        "Failed to close connection " + conn, e);
            }
        }
        conn = null;
    }

    /**
     * Closing all resources that have been idle in the pool for more than
     * resourceRetention millis.
     * 
     * @param resourceRetention
     * @return
     */
    public int closeOldPooledResources(int resourceRetention) {
        if (!isValid()) {
            return 0;
        }

        int count = 0;
        JmsPooledSession sessionToCheck = null;

        synchronized (stateLock) {
            if (session != null) {
                sessionToCheck = session;
            }
        }

        if (sessionToCheck != null) {
            count += sessionToCheck.closeOldPooledResources(resourceRetention);
        }

        synchronized (stateLock) {
            if (availableSession != null) {
                // njensen: I added the synchronized line below so we're
                // synchronized on availableSession.stateLock
                synchronized (availableSession.getPooledObject().getStateLock()) {
                    if (availableSession.expired(System.currentTimeMillis(),
                            resourceRetention)) {
                        availableSession.getPooledObject().close();
                        count++;
                        availableSession = null;
                    } else {
                        sessionToCheck = availableSession.getPooledObject();
                    }
                }
            }
        }

        if (sessionToCheck != null) {
            count += sessionToCheck.closeOldPooledResources(resourceRetention);
        }

        return count;
    }

    public Connection getConnection() {
        // lazy initialized so that we can handle reconnect and logging since
        // spring reconnect doesn't give any status information and appears to
        // the user that the process is hung
        if (conn == null) {
            // safe since conn is volatile
            synchronized (stateLock) {
                if (conn == null) {
                    statusHandler.info("Creating connection: "
                            + this.toString());
                    long exceptionLastHandled = 0;
                    boolean connected = false;
                    while (!connected) {
                        Connection tmp = null;
                        try {
                            tmp = connFactory.createInternalConnection();
                            if (clientId != null) {
                                tmp.setClientID(clientId);
                            }
                            tmp.setExceptionListener(this);
                            tmp.start();
                            conn = tmp;
                            connectionStartTime = System.currentTimeMillis();
                            connected = true;
                        } catch (Exception e) {
                            if ((exceptionLastHandled + ERROR_BROADCAST_INTERVAL) < System
                                    .currentTimeMillis()) {
                                exceptionLastHandled = System
                                        .currentTimeMillis();
                                statusHandler.handle(
                                        Priority.ERROR,
                                        "Unable to connect to "
                                                + connFactory.getProvider(), e);
                            }

                            if (tmp != null) {
                                try {
                                    tmp.close();
                                } catch (Exception e2) {
                                    statusHandler
                                            .handle(Priority.INFO,
                                                    "Failed to close failed connection to "
                                                            + connFactory
                                                                    .getProvider(),
                                                    e2);
                                }
                            }
                            if (connFactory.getReconnectInterval() > 0) {
                                try {
                                    Thread.sleep(connFactory
                                            .getReconnectInterval());
                                } catch (InterruptedException e2) {
                                    // ignore
                                }
                            }
                        }
                    }
                }
            }
        }

        return conn;
    }

    public long getConnectionStartTime() {
        return connectionStartTime;
    }

    public boolean returnSessionToPool(JmsPooledSession sess) {
        boolean valid = false;
        if (sess.isValid()) {
            try {
                // ensure transaction is complete
                Session underlyingSession = sess.getSession();
                if (underlyingSession.getTransacted()) {
                    underlyingSession.commit();
                }
                valid = true;
            } catch (JMSException e) {
                statusHandler.handle(Priority.INFO,
                        "Failed to commit session on returing it to pool", e);
            }
        }

        if (!valid) {
            removeSession(sess);
        } else {
            synchronized (stateLock) {
                // should only be able to have 1 session
                if (availableSession != null) {
                    statusHandler
                            .warn("Pooled session already existed for this connection, closing previous session");
                    availableSession.getPooledObject().close();
                }
                availableSession = new AvailableJmsPooledObject<JmsPooledSession>(
                        session);
                session = null;
            }
        }

        return valid;
    }

    /**
     * Removes this pooled session from the pooled connection. Does NOT close
     * the session, this should be handled independently.
     * 
     * @param sess
     */
    public void removeSession(JmsPooledSession sess) {
        synchronized (stateLock) {
            if (sess != null) {
                if (this.session == sess) {
                    this.session = null;
                } else if ((availableSession != null)
                        && (availableSession.getPooledObject() == sess)) {
                    this.availableSession = null;
                }
            }
        }
    }

    public JmsConnectionWrapper createReference() {
        synchronized (stateLock) {
            if (isValid(State.InUse, true)) {
                JmsConnectionWrapper wrapper = new JmsConnectionWrapper(this);

                synchronized (references) {
                    references.add(wrapper);
                }

                return wrapper;
            }
        }

        return null;
    }

    public boolean isValid() {
        return isValid(State.Closed, false);
    }

    /**
     * Verifies if an exception has occurred, the state is the desired state,
     * and the underlying resource is still valid.
     * 
     * @param requiredState
     * @param mustBeRequiredState
     *            If true, current state must match requiredState for isValid to
     *            be true. If false, current state must not be the
     *            requiredState.
     * @return
     */
    public boolean isValid(State requiredState, boolean mustBeRequiredState) {
        boolean valid = false;
        if (!exceptionOccurred) {
            valid = state.equals(requiredState);
            if (!mustBeRequiredState) {
                valid = !valid;
            }

            if (valid) {
                // check underlying resource
                try {
                    if (conn != null) {
                        conn.getClientID();
                    }
                } catch (JMSException e) {
                    // underlying connection has been closed
                    valid = false;
                }
            }
        }
        return valid;
    }

    public int getReferenceCount() {
        synchronized (references) {
            return references.size();
        }
    }

    public void removeReference(JmsConnectionWrapper wrapper) {
        boolean returnToPool = false;
        synchronized (stateLock) {
            synchronized (references) {
                if (references.remove(wrapper) && references.isEmpty()
                        && State.InUse.equals(state)) {
                    state = State.Available;
                    returnToPool = true;

                    // double check state of session, should be available
                    if (session != null) {
                        statusHandler
                                .warn("Connection marked available, but Session not Available.  Sessions state is: "
                                        + session.getState());
                        session.close();
                        session = null;
                    }

                    if (availableSession != null) {
                        JmsPooledSession availSess = availableSession
                                .getPooledObject();
                        synchronized (availSess.getStateLock()) {
                            if ((availSess != null)
                                    && !State.Available.equals(availSess
                                            .getState())) {
                                statusHandler
                                        .warn("Connection marked available, but Session not Available.  Sessions state is: "
                                                + availSess.getState());
                                availSess.close();
                                availableSession = null;
                            }
                        }
                    }
                }
            }
        }

        boolean valid = isValid();
        if (valid && returnToPool) {
            valid = connFactory.returnConnectionToPool(this);
        }

        if (!valid) {
            close();
        }
    }

    public Thread getThread() {
        return thread;
    }

    /**
     * @return the exceptionOccurred
     */
    public boolean isExceptionOccurred() {
        return exceptionOccurred;
    }

    /**
     * @param exceptionOccurred
     *            the exceptionOccurred to set
     */
    public void setExceptionOccurred(boolean exceptionOccurred) {
        this.exceptionOccurred = exceptionOccurred;
    }

    /**
     * @return the state
     */
    public State getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     */
    public void setState(State state) {
        this.state = state;
    }

    /**
     * @return the stateLock
     */
    public Object getStateLock() {
        return stateLock;
    }
}
