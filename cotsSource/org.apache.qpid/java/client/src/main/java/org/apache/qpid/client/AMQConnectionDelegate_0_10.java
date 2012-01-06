package org.apache.qpid.client;
/*
 * 
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * 
 */


import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.XASession;

import org.apache.qpid.AMQException;
import org.apache.qpid.client.configuration.ClientProperties;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.client.failover.FailoverProtectedOperation;
import org.apache.qpid.framing.ProtocolVersion;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.jms.Session;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.transport.Connection;
import org.apache.qpid.transport.ConnectionClose;
import org.apache.qpid.transport.ConnectionException;
import org.apache.qpid.transport.ConnectionListener;
import org.apache.qpid.transport.ProtocolVersionException;
import org.apache.qpid.transport.TransportException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AMQConnectionDelegate_0_10 implements AMQConnectionDelegate, ConnectionListener
{
    /**
     * This class logger.
     */
    private static final Logger _logger = LoggerFactory.getLogger(AMQConnectionDelegate_0_10.class);

    /**
     * The AMQ Connection.
     */
    private AMQConnection _conn;

    /**
     * The QpidConeection instance that is mapped with thie JMS connection.
     */
    org.apache.qpid.transport.Connection _qpidConnection;
    private ConnectionException exception = null;

    //--- constructor
    public AMQConnectionDelegate_0_10(AMQConnection conn)
    {
        _conn = conn;
        _qpidConnection = new Connection();
        _qpidConnection.setConnectionListener(this);
    }

    /**
     * create a Session and start it if required.
     */
    public Session createSession(boolean transacted, int acknowledgeMode, int prefetchHigh, int prefetchLow)
            throws JMSException
    {
        _conn.checkNotClosed();
        int channelId = _conn.getNextChannelID();
        AMQSession session;
        try
        {
            session = new AMQSession_0_10(_qpidConnection, _conn, channelId, transacted, acknowledgeMode, prefetchHigh,
                                          prefetchLow);
            _conn.registerSession(channelId, session);
            if (_conn._started)
            {
                session.start();
            }
        }
        catch (Exception e)
        {
            _logger.error("exception creating session:", e);
            throw new JMSAMQException("cannot create session", e);
        }
        return session;
    }

    /**
     * Create an XASession with default prefetch values of:
     * High = MaxPrefetch
     * Low  = MaxPrefetch / 2
     * @return XASession
     * @throws JMSException
     */
    public XASession createXASession() throws JMSException
    {
        return createXASession((int) _conn.getMaxPrefetch(), (int) _conn.getMaxPrefetch() / 2);
    }

    /**
     * create an XA Session and start it if required.
     */
    public XASession createXASession(int prefetchHigh, int prefetchLow) throws JMSException
    {
        _conn.checkNotClosed();
        int channelId = _conn.getNextChannelID();
        XASessionImpl session;
        try
        {
            session = new XASessionImpl(_qpidConnection, _conn, channelId, prefetchHigh, prefetchLow);
            _conn.registerSession(channelId, session);
            if (_conn._started)
            {
                session.start();
            }
        }
        catch (Exception e)
        {
            throw new JMSAMQException("cannot create session", e);
        }
        return session;
    }


    /**
     * Make a connection with the broker
     *
     * @param brokerDetail The detail of the broker to connect to.
     * @throws IOException
     * @throws AMQException
     */
    public ProtocolVersion makeBrokerConnection(BrokerDetails brokerDetail) throws IOException, AMQException
    {
        try
        {
            if (_logger.isDebugEnabled())
            {
                _logger.debug("connecting to host: " + brokerDetail.getHost() +
                              " port: " + brokerDetail.getPort() +
                              " vhost: " + _conn.getVirtualHost() +
                              " username: " + _conn.getUsername() +
                              " password: " + _conn.getPassword());
            }
            
            if (brokerDetail.getProperty(BrokerDetails.OPTIONS_IDLE_TIMEOUT) != null)
            {
                this.setIdleTimeout(Long.parseLong(brokerDetail.getProperty(BrokerDetails.OPTIONS_IDLE_TIMEOUT)));
            }
            else
            {
                // use the default value set for all connections
                this.setIdleTimeout(Long.getLong(ClientProperties.IDLE_TIMEOUT_PROP_NAME,0));
            }
            
            String saslMechs = brokerDetail.getProperty("sasl_mechs")!= null?
                               brokerDetail.getProperty("sasl_mechs"):
                               System.getProperty("qpid.sasl_mechs","PLAIN");
            
            _qpidConnection.connect(brokerDetail.getHost(), brokerDetail.getPort(), _conn.getVirtualHost(),
                                    _conn.getUsername(), _conn.getPassword(), brokerDetail.useSSL(),saslMechs);
            _conn._connected = true;
            _conn._failoverPolicy.attainedConnection();
        }
        catch(ProtocolVersionException pe)
        {
            return new ProtocolVersion(pe.getMajor(), pe.getMinor());
        }
        catch (ConnectionException e)
        {            
            throw new AMQException(AMQConstant.CHANNEL_ERROR, "cannot connect to broker", e);
        }

        return null;
    }

    public void failoverPrep()
    {
        List<AMQSession> sessions = new ArrayList<AMQSession>(_conn.getSessions().values());
        for (AMQSession s : sessions)
        {
            s.failoverPrep();
        }
    }

    public void resubscribeSessions() throws JMSException, AMQException, FailoverException
    {
        List<AMQSession> sessions = new ArrayList<AMQSession>(_conn.getSessions().values());
        _logger.info(String.format("Resubscribing sessions = %s sessions.size=%s", sessions, sessions.size()));
        for (AMQSession s : sessions)
        {
            ((AMQSession_0_10) s)._qpidConnection = _qpidConnection;
            s.resubscribe();
        }
    }


    public void closeConnection(long timeout) throws JMSException, AMQException
    {
        try
        {
            _qpidConnection.close();
        }
        catch (TransportException e)
        {
            throw new AMQException(e.getMessage(), e);
        }
    }

    public void opened(Connection conn) {}

    public void exception(Connection conn, ConnectionException exc)
    {
        if (exception != null)
        {
            _logger.error("previous exception", exception);
        }

        exception = exc;
    }

    public void closed(Connection conn)
    {
        ConnectionException exc = exception;
        exception = null;

        if (exc == null)
        {
            return;
        }

        ConnectionClose close = exc.getClose();
        if (close == null)
        {
            try
            {
                if (_conn.firePreFailover(false) && _conn.attemptReconnection())
                {
                    _conn.failoverPrep();
                    _qpidConnection.resume();
                    _conn.fireFailoverComplete();
                    return;
                }
            }
            catch (Exception e)
            {
                _logger.error("error during failover", e);
            }
        }

        ExceptionListener listener = _conn._exceptionListener;
        if (listener == null)
        {
            _logger.error("connection exception: " + conn, exc);
        }
        else
        {
            String code = null;
            if (close != null)
            {
                code = close.getReplyCode().toString();
            }

            JMSException ex = new JMSException(exc.getMessage(), code);
            ex.initCause(exc);
            listener.onException(ex);
        }
    }

    public <T, E extends Exception> T executeRetrySupport(FailoverProtectedOperation<T,E> operation) throws E
    {
        try
        {
            return operation.execute();
        }
        catch (FailoverException e)
        {
            throw new RuntimeException(e);
        }
    }

    public void setIdleTimeout(long l)
    {
        _qpidConnection.setIdleTimeout(l);
    }

    public int getMaxChannelID()
    {
       return Integer.MAX_VALUE;
    }

    public ProtocolVersion getProtocolVersion()
    {
        return ProtocolVersion.v0_10;
    }
}
