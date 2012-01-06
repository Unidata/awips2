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
package org.apache.qpid.client;

import java.io.IOException;
import java.net.ConnectException;
import java.nio.channels.UnresolvedAddressException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.Set;

import javax.jms.JMSException;
import javax.jms.XASession;

import org.apache.qpid.AMQException;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.client.failover.FailoverProtectedOperation;
import org.apache.qpid.client.failover.FailoverRetrySupport;
import org.apache.qpid.client.state.AMQState;
import org.apache.qpid.client.state.StateWaiter;
import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.framing.BasicQosBody;
import org.apache.qpid.framing.BasicQosOkBody;
import org.apache.qpid.framing.ChannelOpenBody;
import org.apache.qpid.framing.ChannelOpenOkBody;
import org.apache.qpid.framing.ProtocolVersion;
import org.apache.qpid.framing.TxSelectBody;
import org.apache.qpid.framing.TxSelectOkBody;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.jms.ChannelLimitReachedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AMQConnectionDelegate_8_0 implements AMQConnectionDelegate
{
    private static final Logger _logger = LoggerFactory.getLogger(AMQConnectionDelegate_8_0.class);
    private AMQConnection _conn;


    public void closeConnection(long timeout) throws JMSException, AMQException
    {
        _conn.getProtocolHandler().closeConnection(timeout);

    }

    public AMQConnectionDelegate_8_0(AMQConnection conn)
    {
        _conn = conn;
    }

    protected boolean checkException(Throwable thrown)
    {
        Throwable cause = thrown.getCause();

        if (cause == null)
        {
            cause = thrown;
        }

        return ((cause instanceof ConnectException) || (cause instanceof UnresolvedAddressException));
    }

    public ProtocolVersion makeBrokerConnection(BrokerDetails brokerDetail) throws AMQException, IOException
    {
        final Set<AMQState> openOrClosedStates =
                EnumSet.of(AMQState.CONNECTION_OPEN, AMQState.CONNECTION_CLOSED);


        StateWaiter waiter = _conn._protocolHandler.createWaiter(openOrClosedStates);

        // TODO: use system property thingy for this
        if (System.getProperty("UseTransportIo", "false").equals("false"))
        {
            TransportConnection.getInstance(brokerDetail).connect(_conn._protocolHandler, brokerDetail);
        }
        else
        {
            _conn.getProtocolHandler().createIoTransportSession(brokerDetail);
        }
        _conn._protocolHandler.getProtocolSession().init();
        // this blocks until the connection has been set up or when an error
        // has prevented the connection being set up

        AMQState state = waiter.await();

        if(state == AMQState.CONNECTION_OPEN)
        {
            _conn._failoverPolicy.attainedConnection();
            _conn._connected = true;
            return null;
        } 
        else 
        {
            return _conn._protocolHandler.getSuggestedProtocolVersion();
        }

    }

    public org.apache.qpid.jms.Session createSession(final boolean transacted, final int acknowledgeMode, final int prefetch)
            throws JMSException
    {
        return createSession(transacted, acknowledgeMode, prefetch, prefetch);
    }

    public XASession createXASession(int prefetchHigh, int prefetchLow) throws JMSException
    {
        throw new UnsupportedOperationException("0_8 version does not provide XA support");
    }

    public org.apache.qpid.jms.Session createSession(final boolean transacted, final int acknowledgeMode,
                                                     final int prefetchHigh, final int prefetchLow) throws JMSException
    {
        _conn.checkNotClosed();

        if (_conn.channelLimitReached())
        {
            throw new ChannelLimitReachedException(_conn._maximumChannelCount);
        }

        return new FailoverRetrySupport<org.apache.qpid.jms.Session, JMSException>(
                new FailoverProtectedOperation<org.apache.qpid.jms.Session, JMSException>()
                {
                    public org.apache.qpid.jms.Session execute() throws JMSException, FailoverException
                    {
                        int channelId = _conn.getNextChannelID();

                        if (_logger.isDebugEnabled())
                        {
                            _logger.debug("Write channel open frame for channel id " + channelId);
                        }

                        // We must create the session and register it before actually sending the frame to the server to
                        // open it, so that there is no window where we could receive data on the channel and not be set
                        // up to handle it appropriately.
                        AMQSession session =
                                new AMQSession_0_8(_conn, channelId, transacted, acknowledgeMode, prefetchHigh,
                                               prefetchLow);
                        // _protocolHandler.addSessionByChannel(channelId, session);
                        _conn.registerSession(channelId, session);

                        boolean success = false;
                        try
                        {
                            createChannelOverWire(channelId, prefetchHigh, prefetchLow, transacted);
                            success = true;
                        }
                        catch (AMQException e)
                        {
                            JMSException jmse = new JMSException("Error creating session: " + e);
                            jmse.setLinkedException(e);
                            throw jmse;
                        }
                        finally
                        {
                            if (!success)
                            {
                                _conn.deregisterSession(channelId);
                            }
                        }

                        if (_conn._started)
                        {
                            try
                            {
                                session.start();
                            }
                            catch (AMQException e)
                            {
                                throw new JMSAMQException(e);
                            }
                        }

                        return session;
                    }
                }, _conn).execute();
    }

    /**
     * Create an XASession with default prefetch values of:
     * High = MaxPrefetch
     * Low  = MaxPrefetch / 2
     * @return XASession
     * @throws JMSException thrown if there is a problem creating the session.
     */
    public XASession createXASession() throws JMSException
    {
        return createXASession((int) _conn.getMaxPrefetch(), (int) _conn.getMaxPrefetch() / 2);
    }

    private void createChannelOverWire(int channelId, int prefetchHigh, int prefetchLow, boolean transacted)
            throws AMQException, FailoverException
    {
        ChannelOpenBody channelOpenBody = _conn.getProtocolHandler().getMethodRegistry().createChannelOpenBody(null);
        // TODO: Be aware of possible changes to parameter order as versions change.
        _conn._protocolHandler.syncWrite(channelOpenBody.generateFrame(channelId),  ChannelOpenOkBody.class);

        // todo send low water mark when protocol allows.
        // todo Be aware of possible changes to parameter order as versions change.
        BasicQosBody basicQosBody = _conn.getProtocolHandler().getMethodRegistry().createBasicQosBody(0,prefetchHigh,false);
        _conn._protocolHandler.syncWrite(basicQosBody.generateFrame(channelId),BasicQosOkBody.class);

        if (transacted)
        {
            if (_logger.isDebugEnabled())
            {
                _logger.debug("Issuing TxSelect for " + channelId);
            }
            TxSelectBody body = _conn.getProtocolHandler().getMethodRegistry().createTxSelectBody();

            // TODO: Be aware of possible changes to parameter order as versions change.
            _conn._protocolHandler.syncWrite(body.generateFrame(channelId), TxSelectOkBody.class);
        }
    }

    public void failoverPrep()
    {
        // do nothing
    }

    /**
     * For all sessions, and for all consumers in those sessions, resubscribe. This is called during failover handling.
     * The caller must hold the failover mutex before calling this method.
     */
    public void resubscribeSessions() throws JMSException, AMQException, FailoverException
    {
        ArrayList sessions = new ArrayList(_conn.getSessions().values());
        _logger.info(MessageFormat.format("Resubscribing sessions = {0} sessions.size={1}", sessions, sessions.size())); // FIXME: removeKey?
        for (Iterator it = sessions.iterator(); it.hasNext();)
        {
            AMQSession s = (AMQSession) it.next();
            // _protocolHandler.addSessionByChannel(s.getChannelId(), s);
            reopenChannel(s.getChannelId(), s.getDefaultPrefetchHigh(), s.getDefaultPrefetchLow(), s.getTransacted());
            s.resubscribe();
        }
    }

    private void reopenChannel(int channelId, int prefetchHigh, int prefetchLow, boolean transacted)
    throws AMQException, FailoverException
    {
        try
        {
            createChannelOverWire(channelId, prefetchHigh, prefetchLow, transacted);
        }
        catch (AMQException e)
        {
            _conn.deregisterSession(channelId);
            throw new AMQException(null, "Error reopening channel " + channelId + " after failover: " + e, e);
        }
    }

    public <T, E extends Exception> T executeRetrySupport(FailoverProtectedOperation<T,E> operation) throws E
    {
        while (true)
        {
            try
            {
                _conn.blockUntilNotFailingOver();
            }
            catch (InterruptedException e)
            {
                _logger.debug("Interrupted: " + e, e);

                return null;
            }

            synchronized (_conn.getFailoverMutex())
            {
                try
                {
                    return operation.execute();
                }
                catch (FailoverException e)
                {
                    _logger.debug("Failover exception caught during operation: " + e, e);
                }
                catch (IllegalStateException e)
                {
                    if (!(e.getMessage().startsWith("Fail-over interupted no-op failover support")))
                    {
                        throw e;
                    }
                }
            }
        }
    }

    public void setIdleTimeout(long l){}

    public int getMaxChannelID()
    {
        return (int) (Math.pow(2, 16)-1);
    }

    public ProtocolVersion getProtocolVersion()
    {
        return ProtocolVersion.v8_0;
    }
}
