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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2011            rjpeter     Initial creation
 * Mar 08, 2012   194   njensen   Improved safety of close()
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class JmsPooledConnection implements ExceptionListener {
    private static final long ERROR_BROADCAST_INTERVAL = 30000;

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(JmsPooledConnection.class);

    private JmsPooledConnectionFactory connFactory = null;

    private Connection conn = null;

    // keeps track of number of creates vs. closes to know when it can be
    // returned to the pool
    List<JmsConnectionWrapper> references = new ArrayList<JmsConnectionWrapper>(
            1);

    private Object stateLock = new Object();

    private State state = State.InUse;

    // technically can have multiple sessions to one connection and sessions can
    // differ by transaction mode and acknowledgement mode, currently not
    // supported
    private JmsPooledSession session = null;

    private AvailableJmsPooledObject<JmsPooledSession> availableSession = null;

    private String key = null;

    private String clientId = null;

    private long connectionStartTime = 0;

    private boolean exceptionOccurred = false;

    private Throwable trappedExc;

    public JmsPooledConnection(JmsPooledConnectionFactory connFactory) {
        this.connFactory = connFactory;
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

        synchronized (this) {
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
                    if (availSess.isValid()) {
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
        statusHandler.handle(Priority.INFO, "Caught Exception on "
                + connFactory.getProvider()
                + " connection.  Closing connection", jmsExc);
        close();
    }

    public void close() {
        // synchronize on the connection to avoid deadlock conditions in qpid
        boolean canClose = false;
        synchronized (stateLock) {
            if (!State.Closed.equals(state)) {
                canClose = true;
                state = State.Closed;

                if (trappedExc != null) {
                    statusHandler.handle(Priority.INFO,
                            "Trapped internal exception", trappedExc);
                }
            }
        }

        if (canClose) {
            synchronized (this) {
                // njensen: I moved removing the connection from the pool to be
                // the first thing in this block instead of last thing so
                // there's no chance it could be closed and then retrieved from
                // the pool by something else
                connFactory.removeConnectionFromPool(this);
                if (conn != null) {
                    try {
                        conn.stop();
                    } catch (Exception e) {
                        statusHandler.handle(Priority.INFO,
                                "Failed to stop connection", e);
                    }
                }

                synchronized (references) {
                    for (JmsConnectionWrapper wrapper : references) {
                        wrapper.closeInternal();
                    }
                    references.clear();
                }

                if (session != null) {
                    session.close();
                    session = null;
                }

                try {
                    conn.close();
                } catch (Exception e) {
                    statusHandler.handle(Priority.INFO,
                            "Failed to close connection " + conn, e);
                }
            }
            conn = null;
        }
    }

    /**
     * Closing all resources that have been idle in the pool for more than
     * resourceRetention millis.
     * 
     * @param resourceRetention
     * @return
     */
    public int closeOldPooledResources(int resourceRetention) {
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
        if (conn == null) {
            synchronized (this) {
                if (conn == null) {
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
                            if (exceptionLastHandled + ERROR_BROADCAST_INTERVAL < System
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
            synchronized (this) {
                // should only be able to have 1 session
                if (availableSession != null) {
                    availableSession.getPooledObject().close();
                    statusHandler
                            .warn("Pooled session already existed for this connection, closing previous session");
                }
                availableSession = new AvailableJmsPooledObject<JmsPooledSession>(
                        session);
                session = null;
            }
        }

        return valid;
    }

    public void removeSession(JmsPooledSession sess) {
        synchronized (this) {
            if (sess != null) {
                if (this.session == sess) {
                    this.session = null;
                } else if (availableSession != null
                        && availableSession.getPooledObject() == sess) {
                    this.availableSession = null;
                }
            }
        }
    }

    public JmsConnectionWrapper createReference() {
        synchronized (stateLock) {
            if (isValid(State.InUse, true)) {
                JmsConnectionWrapper wrapper = new JmsConnectionWrapper(this);
                references.add(wrapper);
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
                    if (availSess != null
                            && !State.Available.equals(availSess.getState())) {
                        statusHandler
                                .warn("Connection marked available, but Session not Available.  Sessions state is: "
                                        + availSess.getState());
                        availSess.close();
                        availableSession = null;
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

    public void setKey(String key) {
        this.key = key;
    }

    public String getKey() {
        return key;
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
