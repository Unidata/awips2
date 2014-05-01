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
package com.raytheon.uf.common.jms.wrapper;

import java.util.ArrayList;
import java.util.List;

import javax.jms.Connection;
import javax.jms.ConnectionConsumer;
import javax.jms.ConnectionMetaData;
import javax.jms.Destination;
import javax.jms.ExceptionListener;
import javax.jms.IllegalStateException;
import javax.jms.JMSException;
import javax.jms.ServerSessionPool;
import javax.jms.Session;
import javax.jms.Topic;

import com.raytheon.uf.common.jms.JmsPooledConnection;

/**
 * Wrapper class for jms connection pooling. Tracks wrapped sessions created
 * from this wrapped connection to know when the connection can be returned to
 * the pool.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2011            rjpeter     Initial creation
 * Feb 21, 2013 1642       rjpeter     Added volatile references for better concurrency handling.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsConnectionWrapper implements Connection {
    private final JmsPooledConnection mgr;

    private volatile boolean closed = false;

    private volatile boolean exceptionOccurred = false;

    private final List<JmsSessionWrapper> sessions = new ArrayList<JmsSessionWrapper>(
            1);

    private final String clientId = null;

    public JmsConnectionWrapper(JmsPooledConnection mgr) {
        this.mgr = mgr;
    }

    /**
     * Closes down this wrapper. Doesn't interact back with manager. For manager
     * interaction use close().
     * 
     * @return True if this wrapper hasn't been closed before, false otherwise.
     */
    public boolean closeWrapper() {
        synchronized (this) {
            if (!closed) {
                closed = true;
                for (JmsSessionWrapper session : sessions) {
                    try {
                        session.close();
                    } catch (JMSException e) {
                        // closing of wrapper doesn't throw an exception
                    }
                }

                if (exceptionOccurred) {
                    mgr.setExceptionOccurred(true);
                }

                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#close()
     */
    @Override
    public void close() throws JMSException {
        if (closeWrapper()) {
            // remove this wrapper from the manager
            mgr.removeReference(this);

            if (exceptionOccurred) {
                mgr.close();
            }
        }
    }

    public Connection getConnection() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Connection closed");
        }

        return mgr.getConnection();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#createConnectionConsumer(javax.jms.Destination,
     * java.lang.String, javax.jms.ServerSessionPool, int)
     */
    @Override
    public ConnectionConsumer createConnectionConsumer(Destination arg0,
            String arg1, ServerSessionPool arg2, int arg3) throws JMSException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.jms.Connection#createDurableConnectionConsumer(javax.jms.Topic,
     * java.lang.String, java.lang.String, javax.jms.ServerSessionPool, int)
     */
    @Override
    public ConnectionConsumer createDurableConnectionConsumer(Topic arg0,
            String arg1, String arg2, ServerSessionPool arg3, int arg4)
            throws JMSException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#createSession(boolean, int)
     */
    @Override
    public Session createSession(boolean transacted, int acknowledgeMode)
            throws JMSException {
        if (closed) {
            throw new IllegalStateException("Connection closed");
        }

        try {
            JmsSessionWrapper session = mgr.getSession(transacted,
                    acknowledgeMode);
            if (session != null) {
                sessions.add(session);
            } else {
                throw new IllegalStateException("Underlying session is closed");
            }
            return session;
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled connection");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#getClientID()
     */
    @Override
    public String getClientID() throws JMSException {
        return clientId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#getExceptionListener()
     */
    @Override
    public ExceptionListener getExceptionListener() throws JMSException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#getMetaData()
     */
    @Override
    public ConnectionMetaData getMetaData() throws JMSException {
        Connection conn = getConnection();

        try {
            return conn.getMetaData();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled connection");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#setClientID(java.lang.String)
     */
    @Override
    public void setClientID(String clientId) throws JMSException {
        throw new IllegalArgumentException(
                "ClientId not implemented on session pool");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.jms.Connection#setExceptionListener(javax.jms.ExceptionListener)
     */
    @Override
    public void setExceptionListener(ExceptionListener arg0)
            throws JMSException {
        // don't allow others to listen
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#start()
     */
    @Override
    public void start() throws JMSException {
        // ignore
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Connection#stop()
     */
    @Override
    public void stop() throws JMSException {
        // ignore
    }
}
