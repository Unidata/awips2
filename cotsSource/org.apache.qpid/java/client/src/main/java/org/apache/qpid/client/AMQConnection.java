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
import java.lang.reflect.InvocationTargetException;
import java.net.ConnectException;
import java.net.UnknownHostException;
import java.nio.channels.UnresolvedAddressException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import javax.jms.ConnectionConsumer;
import javax.jms.ConnectionMetaData;
import javax.jms.Destination;
import javax.jms.ExceptionListener;
import javax.jms.IllegalStateException;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueSession;
import javax.jms.ServerSessionPool;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicSession;
import javax.naming.NamingException;
import javax.naming.Reference;
import javax.naming.Referenceable;
import javax.naming.StringRefAddr;

import org.apache.qpid.AMQConnectionFailureException;
import org.apache.qpid.AMQException;
import org.apache.qpid.AMQProtocolException;
import org.apache.qpid.AMQUnresolvedAddressException;
import org.apache.qpid.AMQDisconnectedException;
import org.apache.qpid.client.configuration.ClientProperties;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.client.failover.FailoverProtectedOperation;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicQosBody;
import org.apache.qpid.framing.BasicQosOkBody;
import org.apache.qpid.framing.ChannelOpenBody;
import org.apache.qpid.framing.ChannelOpenOkBody;
import org.apache.qpid.framing.ProtocolVersion;
import org.apache.qpid.framing.TxSelectBody;
import org.apache.qpid.framing.TxSelectOkBody;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.jms.Connection;
import org.apache.qpid.jms.ConnectionListener;
import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.jms.FailoverPolicy;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.url.URLSyntaxException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AMQConnection extends Closeable implements Connection, QueueConnection, TopicConnection, Referenceable
{
    public static final class ChannelToSessionMap
    {
        private final AMQSession[] _fastAccessSessions = new AMQSession[16];
        private final LinkedHashMap<Integer, AMQSession> _slowAccessSessions = new LinkedHashMap<Integer, AMQSession>();
        private int _size = 0;
        private static final int FAST_CHANNEL_ACCESS_MASK = 0xFFFFFFF0;
        private AtomicInteger _idFactory = new AtomicInteger(0);
        private int _maxChannelID;
        private boolean _cycledIds;

        public AMQSession get(int channelId)
        {
            if ((channelId & FAST_CHANNEL_ACCESS_MASK) == 0)
            {
                return _fastAccessSessions[channelId];
            }
            else
            {
                return _slowAccessSessions.get(channelId);
            }
        }

        public AMQSession put(int channelId, AMQSession session)
        {
            AMQSession oldVal;
            if ((channelId & FAST_CHANNEL_ACCESS_MASK) == 0)
            {
                oldVal = _fastAccessSessions[channelId];
                _fastAccessSessions[channelId] = session;
            }
            else
            {
                oldVal = _slowAccessSessions.put(channelId, session);
            }
            if ((oldVal != null) && (session == null))
            {
                _size--;
            }
            else if ((oldVal == null) && (session != null))
            {
                _size++;
            }

            return session;

        }

        public AMQSession remove(int channelId)
        {
            AMQSession session;
            if ((channelId & FAST_CHANNEL_ACCESS_MASK) == 0)
            {
                session = _fastAccessSessions[channelId];
                _fastAccessSessions[channelId] = null;
            }
            else
            {
                session = _slowAccessSessions.remove(channelId);
            }

            if (session != null)
            {
                _size--;
            }
            return session;

        }

        public Collection<AMQSession> values()
        {
            ArrayList<AMQSession> values = new ArrayList<AMQSession>(size());

            for (int i = 0; i < 16; i++)
            {
                if (_fastAccessSessions[i] != null)
                {
                    values.add(_fastAccessSessions[i]);
                }
            }
            values.addAll(_slowAccessSessions.values());

            return values;
        }

        public int size()
        {
            return _size;
        }

        public void clear()
        {
            _size = 0;
            _slowAccessSessions.clear();
            for (int i = 0; i < 16; i++)
            {
                _fastAccessSessions[i] = null;
            }
        }

        /*
         * Synchronized on whole method so that we don't need to consider the
         * increment-then-reset path in too much detail
         */
        public synchronized int getNextChannelId()
        {
            int id = 0;
            if (!_cycledIds)
            {
                id = _idFactory.incrementAndGet();
                if (id == _maxChannelID)
                {
                    _cycledIds = true;
                    _idFactory.set(0); // Go back to the start
                }
            }
            else
            {
                boolean done = false;
                while (!done)
                {
                    // Needs to work second time through
                    id = _idFactory.incrementAndGet();
                    if (id > _maxChannelID)
                    {
                        _idFactory.set(0);
                        id = _idFactory.incrementAndGet();
                    }
                    if ((id & FAST_CHANNEL_ACCESS_MASK) == 0)
                    {
                        done = (_fastAccessSessions[id] == null);
                    }
                    else
                    {
                        done = (!_slowAccessSessions.keySet().contains(id));
                    }
                }
            }

            return id;
        }

        public void setMaxChannelID(int maxChannelID)
        {
            _maxChannelID = maxChannelID;
        }
    }

    private static final Logger _logger = LoggerFactory.getLogger(AMQConnection.class);


    /**
     * This is the "root" mutex that must be held when doing anything that could be impacted by failover. This must be
     * held by any child objects of this connection such as the session, producers and consumers.
     */
    private final Object _failoverMutex = new Object();

    private final Object _sessionCreationLock = new Object();

    /**
     * A channel is roughly analogous to a session. The server can negotiate the maximum number of channels per session
     * and we must prevent the client from opening too many. Zero means unlimited.
     */
    protected long _maximumChannelCount;

    /** The maximum size of frame supported by the server */
    private long _maximumFrameSize;

    /**
     * The protocol handler dispatches protocol events for this connection. For example, when the connection is dropped
     * the handler deals with this. It also deals with the initial dispatch of any protocol frames to their appropriate
     * handler.
     */
    protected AMQProtocolHandler _protocolHandler;

    /** Maps from session id (Integer) to AMQSession instance */
    private final ChannelToSessionMap _sessions = new ChannelToSessionMap();

    private String _clientName;

    /** The user name to use for authentication */
    private String _username;

    /** The password to use for authentication */
    private String _password;

    /** The virtual path to connect to on the AMQ server */
    private String _virtualHost;

    protected ExceptionListener _exceptionListener;

    private ConnectionListener _connectionListener;

    private ConnectionURL _connectionURL;

    /**
     * Whether this connection is started, i.e. whether messages are flowing to consumers. It has no meaning for message
     * publication.
     */
    protected volatile boolean _started;

    /** Policy dictating how to failover */
    protected FailoverPolicy _failoverPolicy;

    /*
     * _Connected should be refactored with a suitable wait object.
     */
    protected boolean _connected;

    /*
     * The connection meta data
     */
    private QpidConnectionMetaData _connectionMetaData;

    /** Configuration info for SSL */
    private SSLConfiguration _sslConfiguration;

    private AMQShortString _defaultTopicExchangeName = ExchangeDefaults.TOPIC_EXCHANGE_NAME;
    private AMQShortString _defaultQueueExchangeName = ExchangeDefaults.DIRECT_EXCHANGE_NAME;
    private AMQShortString _temporaryTopicExchangeName = ExchangeDefaults.TOPIC_EXCHANGE_NAME;
    private AMQShortString _temporaryQueueExchangeName = ExchangeDefaults.DIRECT_EXCHANGE_NAME;

    /** Thread Pool for executing connection level processes. Such as returning bounced messages. */
    private final ExecutorService _taskPool = Executors.newCachedThreadPool();
    private static final long DEFAULT_TIMEOUT = 1000 * 30;

    protected AMQConnectionDelegate _delegate;

    // this connection maximum number of prefetched messages
    private int _maxPrefetch;

    //Indicates whether persistent messages are synchronized
    private boolean _syncPersistence;

    //Indicates whether we need to sync on every message ack
    private boolean _syncAck;

    //Indicates the sync publish options (persistent|all)
    //By default it's async publish
    private String _syncPublish = "";

    /**
     * @param broker      brokerdetails
     * @param username    username
     * @param password    password
     * @param clientName  clientid
     * @param virtualHost virtualhost
     *
     * @throws AMQException
     * @throws URLSyntaxException
     */
    public AMQConnection(String broker, String username, String password, String clientName, String virtualHost)
            throws AMQException, URLSyntaxException
    {
        this(new AMQConnectionURL(
                ConnectionURL.AMQ_PROTOCOL + "://" + username + ":" + password + "@"
                + ((clientName == null) ? "" : clientName) + "/" + virtualHost + "?brokerlist='"
                + AMQBrokerDetails.checkTransport(broker) + "'"), null);
    }

    /**
     * @param broker      brokerdetails
     * @param username    username
     * @param password    password
     * @param clientName  clientid
     * @param virtualHost virtualhost
     *
     * @throws AMQException
     * @throws URLSyntaxException
     */
    public AMQConnection(String broker, String username, String password, String clientName, String virtualHost,
                         SSLConfiguration sslConfig) throws AMQException, URLSyntaxException
    {
        this(new AMQConnectionURL(
                ConnectionURL.AMQ_PROTOCOL + "://" + username + ":" + password + "@"
                + ((clientName == null) ? "" : clientName) + "/" + virtualHost + "?brokerlist='"
                + AMQBrokerDetails.checkTransport(broker) + "'"), sslConfig);
    }

    public AMQConnection(String host, int port, String username, String password, String clientName, String virtualHost)
            throws AMQException, URLSyntaxException
    {
        this(host, port, false, username, password, clientName, virtualHost, null);
    }

    public AMQConnection(String host, int port, String username, String password, String clientName, String virtualHost,
                         SSLConfiguration sslConfig) throws AMQException, URLSyntaxException
    {
        this(host, port, false, username, password, clientName, virtualHost, sslConfig);
    }

    public AMQConnection(String host, int port, boolean useSSL, String username, String password, String clientName,
                         String virtualHost, SSLConfiguration sslConfig) throws AMQException, URLSyntaxException
    {
        this(new AMQConnectionURL(
                useSSL
                ? (ConnectionURL.AMQ_PROTOCOL + "://" + username + ":" + password + "@"
                   + ((clientName == null) ? "" : clientName) + virtualHost + "?brokerlist='tcp://" + host + ":" + port
                   + "'" + "," + ConnectionURL.OPTIONS_SSL + "='true'")
                : (ConnectionURL.AMQ_PROTOCOL + "://" + username + ":" + password + "@"
                   + ((clientName == null) ? "" : clientName) + virtualHost + "?brokerlist='tcp://" + host + ":" + port
                   + "'" + "," + ConnectionURL.OPTIONS_SSL + "='false'")), sslConfig);
    }

    public AMQConnection(String connection) throws AMQException, URLSyntaxException
    {
        this(new AMQConnectionURL(connection), null);
    }

    public AMQConnection(String connection, SSLConfiguration sslConfig) throws AMQException, URLSyntaxException
    {
        this(new AMQConnectionURL(connection), sslConfig);
    }

    /**
     * @todo Some horrible stuff going on here with setting exceptions to be non-null to detect if an exception
     * was thrown during the connection! Intention not clear. Use a flag anyway, not exceptions... Will fix soon.
     */
    public AMQConnection(ConnectionURL connectionURL, SSLConfiguration sslConfig) throws AMQException
    {
        // set this connection maxPrefetch
        if (connectionURL.getOption(ConnectionURL.OPTIONS_MAXPREFETCH) != null)
        {
            _maxPrefetch = Integer.parseInt(connectionURL.getOption(ConnectionURL.OPTIONS_MAXPREFETCH));
        }
        else
        {
            // use the defaul value set for all connections
            _maxPrefetch = Integer.parseInt(System.getProperties().getProperty(ClientProperties.MAX_PREFETCH_PROP_NAME,
                    ClientProperties.MAX_PREFETCH_DEFAULT));
        }

        if (connectionURL.getOption(ConnectionURL.OPTIONS_SYNC_PERSISTENCE) != null)
        {
            _syncPersistence =
                Boolean.parseBoolean(connectionURL.getOption(ConnectionURL.OPTIONS_SYNC_PERSISTENCE));
            _logger.warn("sync_persistence is a deprecated property, " +
            		"please use sync_publish={persistent|all} instead");
        }
        else
        {
            // use the defaul value set for all connections
            _syncPersistence = Boolean.getBoolean(ClientProperties.SYNC_PERSISTENT_PROP_NAME);
            if (_syncPersistence)
            {
                _logger.warn("sync_persistence is a deprecated property, " +
                        "please use sync_publish={persistent|all} instead");
            }
        }

        if (connectionURL.getOption(ConnectionURL.OPTIONS_SYNC_ACK) != null)
        {
            _syncAck = Boolean.parseBoolean(connectionURL.getOption(ConnectionURL.OPTIONS_SYNC_ACK));
        }
        else
        {
            // use the defaul value set for all connections
            _syncAck = Boolean.getBoolean(ClientProperties.SYNC_ACK_PROP_NAME);
        }

        if (connectionURL.getOption(ConnectionURL.OPTIONS_SYNC_PUBLISH) != null)
        {
            _syncPublish = connectionURL.getOption(ConnectionURL.OPTIONS_SYNC_PUBLISH);
        }
        else
        {
            // use the default value set for all connections
            _syncPublish = System.getProperty((ClientProperties.SYNC_ACK_PROP_NAME),_syncPublish);
        }

        String amqpVersion = System.getProperty((ClientProperties.AMQP_VERSION), "0-10");

        _failoverPolicy = new FailoverPolicy(connectionURL, this);
        BrokerDetails brokerDetails = _failoverPolicy.getCurrentBrokerDetails();
        if (brokerDetails.getTransport().equals(BrokerDetails.VM) || "0-8".equals(amqpVersion)) 
        {
            _delegate = new AMQConnectionDelegate_8_0(this);
        } 
        else if ("0-9".equals(amqpVersion))
        {
            _delegate = new AMQConnectionDelegate_0_9(this);
        }
        else if ("0-91".equals(amqpVersion) || "0-9-1".equals(amqpVersion))
        {
            _delegate = new AMQConnectionDelegate_9_1(this);
        }
        else
        {
            _delegate = new AMQConnectionDelegate_0_10(this);
        }
        _sessions.setMaxChannelID(_delegate.getMaxChannelID());

        if (_logger.isInfoEnabled())
        {
            _logger.info("Connection:" + connectionURL);
        }

        _sslConfiguration = sslConfig;
        if (connectionURL == null)
        {
            throw new IllegalArgumentException("Connection must be specified");
        }

        _connectionURL = connectionURL;

        _clientName = connectionURL.getClientName();
        _username = connectionURL.getUsername();
        _password = connectionURL.getPassword();

        setVirtualHost(connectionURL.getVirtualHost());

        if (connectionURL.getDefaultQueueExchangeName() != null)
        {
            _defaultQueueExchangeName = connectionURL.getDefaultQueueExchangeName();
        }

        if (connectionURL.getDefaultTopicExchangeName() != null)
        {
            _defaultTopicExchangeName = connectionURL.getDefaultTopicExchangeName();
        }

        if (connectionURL.getTemporaryQueueExchangeName() != null)
        {
            _temporaryQueueExchangeName = connectionURL.getTemporaryQueueExchangeName();
        }

        if (connectionURL.getTemporaryTopicExchangeName() != null)
        {
            _temporaryTopicExchangeName = connectionURL.getTemporaryTopicExchangeName();
        }

        _protocolHandler = new AMQProtocolHandler(this);

        // We are not currently connected
        _connected = false;

        boolean retryAllowed = true;
        Exception connectionException = null;
        while (!_connected && retryAllowed && brokerDetails != null)
        {
            ProtocolVersion pe = null;
            try
            {
                pe = makeBrokerConnection(brokerDetails);
            }
            catch (Exception e)
            {
                if (_logger.isInfoEnabled())
                {
                    _logger.info("Unable to connect to broker at " +
                                 _failoverPolicy.getCurrentBrokerDetails(),
                                 e);
                }
                connectionException = e;
            }

            if (pe != null)
            {
                // reset the delegate to the version returned by the
                // broker
                initDelegate(pe);
            }
            else if (!_connected)
            {
                retryAllowed = _failoverPolicy.failoverAllowed();
                brokerDetails = _failoverPolicy.getNextBrokerDetails();
            }
        }

        if (_logger.isDebugEnabled())
        {
            _logger.debug("Are we connected:" + _connected);
        }

        if (!_connected)
        {
            String message = null;

            if (connectionException != null)
            {
                if (connectionException.getCause() != null)
                {
                    message = connectionException.getCause().getMessage();
                }
                else
                {
                    message = connectionException.getMessage();
                }
            }

            if ((message == null) || message.equals(""))
            {
                if (message == null)
                {
                    message = "Unable to Connect";
                }
                else // can only be "" if getMessage() returned it therfore lastException != null
                {
                    message = "Unable to Connect:" + connectionException.getClass();
                }
            }

            for (Throwable th = connectionException; th != null; th = th.getCause())
            {
                if (th instanceof UnresolvedAddressException ||
                    th instanceof UnknownHostException)
                {
                    throw new AMQUnresolvedAddressException
                        (message,
                         _failoverPolicy.getCurrentBrokerDetails().toString(),
                         connectionException);
                }
            }

            throw new AMQConnectionFailureException(message, connectionException);
        }

        _connectionMetaData = new QpidConnectionMetaData(this);
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

    private void initDelegate(ProtocolVersion pe) throws AMQProtocolException
    {
        try
        {
            Class c = Class.forName(String.format
                                    ("org.apache.qpid.client.AMQConnectionDelegate_%s_%s",
                                     pe.getMajorVersion(), pe.getMinorVersion()));
            Class partypes[] = new Class[1];
            partypes[0] = AMQConnection.class;
            _delegate = (AMQConnectionDelegate) c.getConstructor(partypes).newInstance(this);
            _sessions.setMaxChannelID(_delegate.getMaxChannelID());
        }
        catch (ClassNotFoundException e)
        {
            throw new AMQProtocolException
                (AMQConstant.UNSUPPORTED_CLIENT_PROTOCOL_ERROR,
                 String.format("Protocol: %s.%s is rquired by the broker but is not " +
                               "currently supported by this client library implementation",
                               pe.getMajorVersion(), pe.getMinorVersion()),
                 e);
        }
        catch (NoSuchMethodException e)
        {
            throw new RuntimeException("unable to locate constructor for delegate", e);
        }
        catch (InstantiationException e)
        {
            throw new RuntimeException("error instantiating delegate", e);
        }
        catch (IllegalAccessException e)
        {
            throw new RuntimeException("error accessing delegate", e);
        }
        catch (InvocationTargetException e)
        {
            throw new RuntimeException("error invoking delegate", e);
        }
    }

    protected AMQConnection(String username, String password, String clientName, String virtualHost)
    {
        _clientName = clientName;
        _username = username;
        _password = password;
        setVirtualHost(virtualHost);
    }

    private void setVirtualHost(String virtualHost)
    {
        if (virtualHost != null && virtualHost.startsWith("/"))
        {
            virtualHost = virtualHost.substring(1);
        }

        _virtualHost = virtualHost;
    }

    public boolean attemptReconnection(String host, int port)
    {
        BrokerDetails bd = new AMQBrokerDetails(host, port, _sslConfiguration);

        _failoverPolicy.setBroker(bd);

        try
        {
            makeBrokerConnection(bd);

            return true;
        }
        catch (Exception e)
        {
            if (_logger.isInfoEnabled())
            {
                _logger.info("Unable to connect to broker at " + bd);
            }

            attemptReconnection();
        }

        return false;
    }

    public boolean attemptReconnection()
    {
        BrokerDetails broker = null;
        while (_failoverPolicy.failoverAllowed() && (broker = _failoverPolicy.getNextBrokerDetails()) != null)
        {
            try
            {
                makeBrokerConnection(broker);
                return true;
            }
            catch (Exception e)
            {
                if (!(e instanceof AMQException))
                {
                    if (_logger.isInfoEnabled())
                    {
                        _logger.info("Unable to connect to broker at " + _failoverPolicy.getCurrentBrokerDetails(), e);
                    }
                }
                else
                {
                    if (_logger.isInfoEnabled())
                    {
                        _logger.info(e.getMessage() + ":Unable to connect to broker at "
                                     + _failoverPolicy.getCurrentBrokerDetails());
                    }
                }
            }
        }

        // connection unsuccessful
        return false;
    }

    public ProtocolVersion makeBrokerConnection(BrokerDetails brokerDetail) throws IOException, AMQException
    {
        return _delegate.makeBrokerConnection(brokerDetail);
    }

    public <T, E extends Exception> T executeRetrySupport(FailoverProtectedOperation<T,E> operation) throws E
    {
        return _delegate.executeRetrySupport(operation);
    }

    /**
     * Get the details of the currently active broker
     *
     * @return null if no broker is active (i.e. no successful connection has been made, or the BrokerDetail instance
     *         otherwise
     */
    public BrokerDetails getActiveBrokerDetails()
    {
        return _failoverPolicy.getCurrentBrokerDetails();
    }

    public boolean failoverAllowed()
    {
        if (!_connected)
        {
            return false;
        }
        else
        {
            return _failoverPolicy.failoverAllowed();
        }
    }

    public org.apache.qpid.jms.Session createSession(final boolean transacted, final int acknowledgeMode) throws JMSException
    {
        return createSession(transacted, acknowledgeMode, _maxPrefetch);
    }

    public org.apache.qpid.jms.Session createSession(final boolean transacted, final int acknowledgeMode, final int prefetch)
            throws JMSException
    {
        return createSession(transacted, acknowledgeMode, prefetch, prefetch);
    }

    public org.apache.qpid.jms.Session createSession(final boolean transacted, final int acknowledgeMode,
                                                     final int prefetchHigh, final int prefetchLow) throws JMSException
    {
        synchronized (_sessionCreationLock)
        {
            checkNotClosed();
            return _delegate.createSession(transacted, acknowledgeMode, prefetchHigh, prefetchLow);
        }
    }

    private void createChannelOverWire(int channelId, int prefetchHigh, int prefetchLow, boolean transacted)
            throws AMQException, FailoverException
    {

        ChannelOpenBody channelOpenBody = getProtocolHandler().getMethodRegistry().createChannelOpenBody(null);

        // TODO: Be aware of possible changes to parameter order as versions change.

        _protocolHandler.syncWrite(channelOpenBody.generateFrame(channelId), ChannelOpenOkBody.class);

        BasicQosBody basicQosBody = getProtocolHandler().getMethodRegistry().createBasicQosBody(0, prefetchHigh, false);

        // todo send low water mark when protocol allows.
        // todo Be aware of possible changes to parameter order as versions change.
        _protocolHandler.syncWrite(basicQosBody.generateFrame(channelId), BasicQosOkBody.class);

        if (transacted)
        {
            if (_logger.isDebugEnabled())
            {
                _logger.debug("Issuing TxSelect for " + channelId);
            }

            TxSelectBody body = getProtocolHandler().getMethodRegistry().createTxSelectBody();

            // TODO: Be aware of possible changes to parameter order as versions change.
            _protocolHandler.syncWrite(body.generateFrame(channelId), TxSelectOkBody.class);
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
            deregisterSession(channelId);
            throw new AMQException(null, "Error reopening channel " + channelId + " after failover: " + e, e);
        }
    }

    public void setFailoverPolicy(FailoverPolicy policy)
    {
        _failoverPolicy = policy;
    }

    public FailoverPolicy getFailoverPolicy()
    {
        return _failoverPolicy;
    }

    /**
     * Returns an AMQQueueSessionAdaptor which wraps an AMQSession and throws IllegalStateExceptions where specified in
     * the JMS spec
     *
     * @param transacted
     * @param acknowledgeMode
     *
     * @return QueueSession
     *
     * @throws JMSException
     */
    public QueueSession createQueueSession(boolean transacted, int acknowledgeMode) throws JMSException
    {
        return new AMQQueueSessionAdaptor(createSession(transacted, acknowledgeMode));
    }

    /**
     * Returns an AMQTopicSessionAdapter which wraps an AMQSession and throws IllegalStateExceptions where specified in
     * the JMS spec
     *
     * @param transacted
     * @param acknowledgeMode
     *
     * @return TopicSession
     *
     * @throws JMSException
     */
    public TopicSession createTopicSession(boolean transacted, int acknowledgeMode) throws JMSException
    {
        return new AMQTopicSessionAdaptor(createSession(transacted, acknowledgeMode));
    }

    public boolean channelLimitReached()
    {
        return (_maximumChannelCount != 0) && (_sessions.size() == _maximumChannelCount);
    }

    public String getClientID() throws JMSException
    {
        checkNotClosed();

        return _clientName;
    }

    public void setClientID(String clientID) throws JMSException
    {
        checkNotClosed();
        // in AMQP it is not possible to change the client ID. If one is not specified
        // upon connection construction, an id is generated automatically. Therefore
        // we can always throw an exception.
        if (!Boolean.getBoolean(ClientProperties.IGNORE_SET_CLIENTID_PROP_NAME))
        {
            throw new IllegalStateException("Client name cannot be changed after being set");
        }
        else
        {
            _logger.info("Operation setClientID is ignored using ID: " + getClientID());
        }
    }

    public ConnectionMetaData getMetaData() throws JMSException
    {
        checkNotClosed();

        return _connectionMetaData;

    }

    public ExceptionListener getExceptionListener() throws JMSException
    {
        checkNotClosed();

        return _exceptionListener;
    }

    public void setExceptionListener(ExceptionListener listener) throws JMSException
    {
        checkNotClosed();
        _exceptionListener = listener;
    }

    /**
     * Start the connection, i.e. start flowing messages. Note that this method must be called only from a single thread
     * and is not thread safe (which is legal according to the JMS specification).
     *
     * @throws JMSException
     */
    public void start() throws JMSException
    {
        checkNotClosed();
        if (!_started)
        {
            _started = true;
            final Iterator it = _sessions.values().iterator();
            while (it.hasNext())
            {
                final AMQSession s = (AMQSession) (it.next());
                try
                {
                    s.start();
                }
                catch (AMQException e)
                {
                    throw new JMSAMQException(e);
                }
            }

        }
    }

    public void stop() throws JMSException
    {
        checkNotClosed();
        if (_started)
        {
            for (Iterator i = _sessions.values().iterator(); i.hasNext();)
            {
                try
                {
                    ((AMQSession) i.next()).stop();
                }
                catch (AMQException e)
                {
                    throw new JMSAMQException(e);
                }
            }

            _started = false;
        }
    }

    public void     close() throws JMSException
    {
        close(DEFAULT_TIMEOUT);
    }

    public void close(long timeout) throws JMSException
    {
        close(new ArrayList<AMQSession>(_sessions.values()), timeout);
    }

    public void close(List<AMQSession> sessions, long timeout) throws JMSException
    {
        if (!_closed.getAndSet(true))
        {
            _closing.set(true);
            try{
                doClose(sessions, timeout);
            }finally{
                _closing.set(false);
            }
        }
    }

    private void doClose(List<AMQSession> sessions, long timeout) throws JMSException
    {
        synchronized (_sessionCreationLock)
        {
            if (!sessions.isEmpty())
            {
                AMQSession session = sessions.remove(0);
                synchronized (session.getMessageDeliveryLock())
                {
                    doClose(sessions, timeout);
                }
            }
            else
            {
                synchronized (getFailoverMutex())
                {
                    try
                    {
                        long startCloseTime = System.currentTimeMillis();

                        closeAllSessions(null, timeout, startCloseTime);

                        //This MUST occur after we have successfully closed all Channels/Sessions
                        _taskPool.shutdown();

                        if (!_taskPool.isTerminated())
                        {
                            try
                            {
                                // adjust timeout
                                long taskPoolTimeout = adjustTimeout(timeout, startCloseTime);

                                _taskPool.awaitTermination(taskPoolTimeout, TimeUnit.MILLISECONDS);
                            }
                            catch (InterruptedException e)
                            {
                                _logger.info("Interrupted while shutting down connection thread pool.");
                            }
                        }

                        // adjust timeout
                        timeout = adjustTimeout(timeout, startCloseTime);
                        _delegate.closeConnection(timeout);

                        //If the taskpool hasn't shutdown by now then give it shutdownNow.
                        // This will interupt any running tasks.
                        if (!_taskPool.isTerminated())
                        {
                            List<Runnable> tasks = _taskPool.shutdownNow();
                            for (Runnable r : tasks)
                            {
                                _logger.warn("Connection close forced taskpool to prevent execution:" + r);
                            }
                        }
                    }
                    catch (AMQException e)
                    {
                        _logger.error("error:", e);
                        JMSException jmse = new JMSException("Error closing connection: " + e);
                        jmse.setLinkedException(e);
                        throw jmse;
                    }
                }
            }
        }
    }

    private long adjustTimeout(long timeout, long startTime)
    {
        long now = System.currentTimeMillis();
        timeout -= now - startTime;
        if (timeout < 0)
        {
            timeout = 0;
        }

        return timeout;
    }

    /**
     * Marks all sessions and their children as closed without sending any protocol messages. Useful when you need to
     * mark objects "visible" in userland as closed after failover or other significant event that impacts the
     * connection. <p/> The caller must hold the failover mutex before calling this method.
     */
    private void markAllSessionsClosed()
    {
        final LinkedList sessionCopy = new LinkedList(_sessions.values());
        final Iterator it = sessionCopy.iterator();
        while (it.hasNext())
        {
            final AMQSession session = (AMQSession) it.next();

            session.markClosed();
        }

        _sessions.clear();
    }

    /**
     * Close all the sessions, either due to normal connection closure or due to an error occurring.
     *
     * @param cause if not null, the error that is causing this shutdown <p/> The caller must hold the failover mutex
     *              before calling this method.
     */
    private void closeAllSessions(Throwable cause, long timeout, long starttime) throws JMSException
    {
        final LinkedList sessionCopy = new LinkedList(_sessions.values());
        final Iterator it = sessionCopy.iterator();
        JMSException sessionException = null;
        while (it.hasNext())
        {
            final AMQSession session = (AMQSession) it.next();
            if (cause != null)
            {
                session.closed(cause);
            }
            else
            {
                try
                {
                    if (starttime != -1)
                    {
                        timeout = adjustTimeout(timeout, starttime);
                    }

                    session.close(timeout);
                }
                catch (JMSException e)
                {
                    _logger.error("Error closing session: " + e);
                    sessionException = e;
                }
            }
        }

        _sessions.clear();
        if (sessionException != null)
        {
            throw sessionException;
        }
    }

    public ConnectionConsumer createConnectionConsumer(Destination destination, String messageSelector,
                                                       ServerSessionPool sessionPool, int maxMessages) throws JMSException
    {
        checkNotClosed();

        return null;
    }

    public ConnectionConsumer createConnectionConsumer(Queue queue, String messageSelector, ServerSessionPool sessionPool,
                                                       int maxMessages) throws JMSException
    {
        checkNotClosed();

        return null;
    }

    public ConnectionConsumer createConnectionConsumer(Topic topic, String messageSelector, ServerSessionPool sessionPool,
                                                       int maxMessages) throws JMSException
    {
        checkNotClosed();

        return null;
    }

    public ConnectionConsumer createDurableConnectionConsumer(Topic topic, String subscriptionName, String messageSelector,
                                                              ServerSessionPool sessionPool, int maxMessages) throws JMSException
    {
        // TODO Auto-generated method stub
        checkNotClosed();

        return null;
    }

    public long getMaximumChannelCount() throws JMSException
    {
        checkNotClosed();

        return _maximumChannelCount;
    }

    public void setConnectionListener(ConnectionListener listener)
    {
        _connectionListener = listener;
    }

    public ConnectionListener getConnectionListener()
    {
        return _connectionListener;
    }

    public void setMaximumChannelCount(long maximumChannelCount)
    {
        _maximumChannelCount = maximumChannelCount;
    }

    public void setMaximumFrameSize(long frameMax)
    {
        _maximumFrameSize = frameMax;
    }

    public long getMaximumFrameSize()
    {
        return _maximumFrameSize;
    }

    public ChannelToSessionMap getSessions()
    {
        return _sessions;
    }

    public String getUsername()
    {
        return _username;
    }

    public String getPassword()
    {
        return _password;
    }

    public String getVirtualHost()
    {
        return _virtualHost;
    }

    public AMQProtocolHandler getProtocolHandler()
    {
        return _protocolHandler;
    }

    public boolean started()
    {
        return _started;
    }

    public void bytesSent(long writtenBytes)
    {
        if (_connectionListener != null)
        {
            _connectionListener.bytesSent(writtenBytes);
        }
    }

    public void bytesReceived(long receivedBytes)
    {
        if (_connectionListener != null)
        {
            _connectionListener.bytesReceived(receivedBytes);
        }
    }

    /**
     * Fire the preFailover event to the registered connection listener (if any)
     *
     * @param redirect true if this is the result of a redirect request rather than a connection error
     *
     * @return true if no listener or listener does not veto change
     */
    public boolean firePreFailover(boolean redirect)
    {
        boolean proceed = true;
        if (_connectionListener != null)
        {
            proceed = _connectionListener.preFailover(redirect);
        }

        return proceed;
    }

    /**
     * Fire the preResubscribe event to the registered connection listener (if any). If the listener vetoes
     * resubscription then all the sessions are closed.
     *
     * @return true if no listener or listener does not veto resubscription.
     *
     * @throws JMSException
     */
    public boolean firePreResubscribe() throws JMSException
    {
        if (_connectionListener != null)
        {
            boolean resubscribe = _connectionListener.preResubscribe();
            if (!resubscribe)
            {
                markAllSessionsClosed();
            }

            return resubscribe;
        }
        else
        {
            return true;
        }
    }

    /** Fires a failover complete event to the registered connection listener (if any). */
    public void fireFailoverComplete()
    {
        if (_connectionListener != null)
        {
            _connectionListener.failoverComplete();
        }
    }

    /**
     * In order to protect the consistency of the connection and its child sessions, consumers and producers, the
     * "failover mutex" must be held when doing any operations that could be corrupted during failover.
     *
     * @return a mutex. Guaranteed never to change for the lifetime of this connection even if failover occurs.
     */
    public final Object getFailoverMutex()
    {
        return _failoverMutex;
    }

    public void failoverPrep()
    {
        _delegate.failoverPrep();
    }

    public void resubscribeSessions() throws JMSException, AMQException, FailoverException
    {
        _delegate.resubscribeSessions();
    }

    /**
     * If failover is taking place this will block until it has completed. If failover is not taking place it will
     * return immediately.
     *
     * @throws InterruptedException
     */
    public void blockUntilNotFailingOver() throws InterruptedException
    {
        _protocolHandler.blockUntilNotFailingOver();
    }

    /**
     * Invoked by the AMQProtocolSession when a protocol session exception has occurred. This method sends the exception
     * to a JMS exception listener, if configured, and propagates the exception to sessions, which in turn will
     * propagate to consumers. This allows synchronous consumers to have exceptions thrown to them.
     *
     * @param cause the exception
     */
    public void exceptionReceived(Throwable cause)
    {

        if (_logger.isDebugEnabled())
        {
            _logger.debug("exceptionReceived done by:" + Thread.currentThread().getName(), cause);
        }

        final JMSException je;
        if (cause instanceof JMSException)
        {
            je = (JMSException) cause;
        }
        else
        {
            AMQConstant code = null;

            if (cause instanceof AMQException)
            {
                code = ((AMQException) cause).getErrorCode();
            }

            if (code != null)
            {
                je =
                        new JMSException(Integer.toString(code.getCode()), "Exception thrown against " + toString() + ": "
                                                                           + cause);
            }
            else
            {
                //Should never get here as all AMQEs are required to have an ErrorCode!
                // Other than AMQDisconnectedEx!

                if (cause instanceof AMQDisconnectedException)
                {
                    Exception last = _protocolHandler.getStateManager().getLastException();
                    if (last != null)
                    {
                        _logger.info("StateManager had an exception for us to use a cause of our Disconnected Exception");
                        cause = last;
                    }
                }
                je = new JMSException("Exception thrown against " + toString() + ": " + cause);
            }

            if (cause instanceof Exception)
            {
                je.setLinkedException((Exception) cause);
            }
        }

        boolean closer = false;

        // in the case of an IOException, MINA has closed the protocol session so we set _closed to true
        // so that any generic client code that tries to close the connection will not mess up this error
        // handling sequence
        if (cause instanceof IOException || cause instanceof AMQDisconnectedException)
        {
            // If we have an IOE/AMQDisconnect there is no connection to close on.
            _closing.set(false);
            closer = !_closed.getAndSet(true);

            _protocolHandler.getProtocolSession().notifyError(je);
        }

        if (_exceptionListener != null)
        {
            _exceptionListener.onException(je);
        }
        else
        {
            _logger.error("Throwable Received but no listener set: " + cause.getMessage());
        }

        if (hardError(cause))
        {
            try
            {
                if (_logger.isInfoEnabled())
                {
                    _logger.info("Closing AMQConnection due to :" + cause.getMessage());
                }

                closer = (!_closed.getAndSet(true)) || closer;
                if (closer)
                {
                    closeAllSessions(cause, -1, -1); // FIXME: when doing this end up with RejectedExecutionException from executor.
                }
            }
            catch (JMSException e)
            {
                _logger.error("Error closing all sessions: " + e, e);
            }

        }
        else
        {
            _logger.info("Not a hard-error connection not closing: " + cause.getMessage());
        }
    }

    private boolean hardError(Throwable cause)
    {
        if (cause instanceof AMQException)
        {
            return ((AMQException) cause).isHardError();
        }

        return true;
    }

    void registerSession(int channelId, AMQSession session)
    {
        _sessions.put(channelId, session);
    }

    public void deregisterSession(int channelId)
    {
        _sessions.remove(channelId);
    }

    public String toString()
    {
        StringBuffer buf = new StringBuffer("AMQConnection:\n");
        if (_failoverPolicy.getCurrentBrokerDetails() == null)
        {
            buf.append("No active broker connection");
        }
        else
        {
            BrokerDetails bd = _failoverPolicy.getCurrentBrokerDetails();
            buf.append("Host: ").append(String.valueOf(bd.getHost()));
            buf.append("\nPort: ").append(String.valueOf(bd.getPort()));
        }

        buf.append("\nVirtual Host: ").append(String.valueOf(_virtualHost));
        buf.append("\nClient ID: ").append(String.valueOf(_clientName));
        buf.append("\nActive session count: ").append((_sessions == null) ? 0 : _sessions.size());

        return buf.toString();
    }

    public String toURL()
    {
        return _connectionURL.toString();
    }

    public Reference getReference() throws NamingException
    {
        return new Reference(AMQConnection.class.getName(), new StringRefAddr(AMQConnection.class.getName(), toURL()),
                             AMQConnectionFactory.class.getName(), null); // factory location
    }

    public SSLConfiguration getSSLConfiguration()
    {
        return _sslConfiguration;
    }

    public AMQShortString getDefaultTopicExchangeName()
    {
        return _defaultTopicExchangeName;
    }

    public void setDefaultTopicExchangeName(AMQShortString defaultTopicExchangeName)
    {
        _defaultTopicExchangeName = defaultTopicExchangeName;
    }

    public AMQShortString getDefaultQueueExchangeName()
    {
        return _defaultQueueExchangeName;
    }

    public void setDefaultQueueExchangeName(AMQShortString defaultQueueExchangeName)
    {
        _defaultQueueExchangeName = defaultQueueExchangeName;
    }

    public AMQShortString getTemporaryTopicExchangeName()
    {
        return _temporaryTopicExchangeName;
    }

    public AMQShortString getTemporaryQueueExchangeName()
    {
        return _temporaryQueueExchangeName; // To change body of created methods use File | Settings | File Templates.
    }

    public void setTemporaryTopicExchangeName(AMQShortString temporaryTopicExchangeName)
    {
        _temporaryTopicExchangeName = temporaryTopicExchangeName;
    }

    public void setTemporaryQueueExchangeName(AMQShortString temporaryQueueExchangeName)
    {
        _temporaryQueueExchangeName = temporaryQueueExchangeName;
    }

    public void performConnectionTask(Runnable task)
    {
        _taskPool.execute(task);
    }

    public AMQSession getSession(int channelId)
    {
        return _sessions.get(channelId);
    }

    public ProtocolVersion getProtocolVersion()
    {
        return _delegate.getProtocolVersion();
    }

    public boolean isFailingOver()
    {
        return (_protocolHandler.getFailoverLatch() != null);
    }

    /**
     * Get the maximum number of messages that this connection can pre-fetch.
     *
     * @return The maximum number of messages that this connection can pre-fetch.
     */
    public long getMaxPrefetch()
    {
        return _maxPrefetch;
    }

    /**
     * Indicates whether persistent messages are synchronized
     *
     * @return true if persistent messages are synchronized false otherwise
     */
    public boolean getSyncPersistence()
    {
        return _syncPersistence;
    }

    /**
     * Indicates whether we need to sync on every message ack
     */
    public boolean getSyncAck()
    {
        return _syncAck;
    }

    public String getSyncPublish()
    {
        return _syncPublish;
    }

    public void setIdleTimeout(long l)
    {
        _delegate.setIdleTimeout(l);
    }

    public int getNextChannelID()
    {
        return _sessions.getNextChannelId();
    }
}
