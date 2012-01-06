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

import java.util.LinkedList;

import javax.jms.Connection;
import javax.jms.ConnectionMetaData;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;

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

    // Limit the number of sessions per connection?
    private JmsPooledConnectionFactory connFactory = null;

    private Connection conn = null;

    private LinkedList<JmsPooledSession> sessions = new LinkedList<JmsPooledSession>();

    private long connectionStartTime;

    private boolean connected = false;

    private String clientId = null;

    private int sessionCount = 0;

    public JmsPooledConnection(JmsPooledConnectionFactory connFactory,
            String clientId) {
        this.connFactory = connFactory;
        this.clientId = clientId;
    }

    /**
     * Returns null if the connection has reached its session limit.
     * 
     * @see javax.jms.Connection#createSession(boolean, int)
     */
    public synchronized JmsPooledSession createSession(boolean transacted,
            int acknowledgeMode) throws JMSException {
        JmsPooledSession session = null;

        if (sessionCount < connFactory.getSessionsPerConnection()) {
            Connection conn = getConnection();

            synchronized (conn) {
                session = new JmsPooledSession(this, conn.createSession(
                        transacted, acknowledgeMode));
            }

            sessions.add(session);
            sessionCount++;

            if (sessionCount >= connFactory.getSessionsPerConnection()) {
                connFactory.removeConnectionFromPool(this);
            }
        }

        return session;
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
        disconnect();
    }

    private synchronized void disconnect() {
        // synchronize on the connection to avoid deadlock conditions in qpid
        connected = false;
        if (conn != null) {
            try {
                conn.stop();
            } catch (Exception e) {
                statusHandler.handle(Priority.INFO,
                        "Failed to stop connection", e);
            }

            try {
                conn.close();
            } catch (Exception e) {
                statusHandler.handle(Priority.INFO,
                        "Failed to close connection", e);
            }
        }
        conn = null;

        while (!sessions.isEmpty()) {
            sessions.remove().closeInternal();
        }

        if (sessionCount >= connFactory.getSessionsPerConnection()) {
            connFactory.returnConnectionToPool(this);
        }

        sessionCount = 0;
    }

    private Connection getConnection() {
        if (conn == null) {
            synchronized (this) {
                if (conn == null) {
                    long exceptionLastHandled = 0;
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

    public boolean isConnected() {
        return connected;
    }

    public long getConnectionStartTime() {
        return connectionStartTime;
    }

    public void returnSessionToPool(JmsPooledSession sess) {
        boolean valid = false;
        if (sess.isValid()) {
            try {
                // ensure transaction is complete
                if (sess.getTransacted()) {
                    sess.commit();
                }
                valid = true;
            } catch (JMSException e) {
                statusHandler.handle(Priority.INFO,
                        "Failed to commit session on returing it to pool", e);
            }
        }

        if (valid) {
            connFactory.returnSessionToPool(sess);
        } else {
            removeSession(sess);
        }
    }

    public void removeSession(JmsPooledSession sess) {
        connFactory.removeSessionFromPool(sess);

        synchronized (this) {
            if (sessions.remove(sess)) {
                sessionCount--;

                if (sessionCount == 0) {
                    // all sessions have been closed, shut down connection
                    connFactory.removeConnectionFromPool(this);
                    sess.closeInternal();
                    disconnect();
                } else if (sessionCount == connFactory
                        .getSessionsPerConnection() - 1) {
                    connFactory.returnConnectionToPool(this);
                    sess.closeInternal();
                }
            }
        }
    }
}
