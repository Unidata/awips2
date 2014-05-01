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
using System;
using System.Collections;
using System.IO;
using System.Reflection;
using System.Threading;
using log4net;
using Apache.Qpid.Client.Failover;
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Client.Qms;
using Apache.Qpid.Client.State;
using Apache.Qpid.Client.Transport;
using Apache.Qpid.Client.Transport.Socket.Blocking;
using Apache.Qpid.Collections;
using Apache.Qpid.Framing;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Client
{
    public class AMQConnection : Closeable, IConnection
    {
        private static readonly ILog _log = LogManager.GetLogger(typeof(AMQConnection));
        
        IConnectionInfo _connectionInfo;
        private int _nextChannelId = 0;

        // _Connected should be refactored with a suitable wait object.
        private bool _connected;

        Thread _heartBeatThread;
        HeartBeatThread _heartBeatRunner;

        // The last error code that occured on the connection. Used to return the correct exception to the client
        private AMQException _lastAMQException = null;

        /**
         * This is the "root" mutex that must be held when doing anything that could be impacted by failover.
         * This must be held by any child objects of this connection such as the session, producers and consumers.
         */
        private readonly Object _failoverMutex = new Object();
        public object FailoverMutex
        {
            get { return _failoverMutex; }
        }

        /**
         * Policy dictating how to failover
         */
        private FailoverPolicy _failoverPolicy;

        internal bool IsFailoverAllowed
        {
            get { if(!_connected) return false; else return _failoverPolicy.FailoverAllowed(); }
        }

        /// <summary>
        /// A channel is roughly analogous to a session. The server can negotiate the maximum number of channels
        /// per session and we must prevent the client from opening too many. Zero means unlimited.
        /// </summary>
        private ushort _maximumChannelCount;

        /// <summary>
        /// The maximum size of frame supported by the server
        /// </summary>
        private uint _maximumFrameSize;

        private AMQStateManager _stateManager;

        private AMQProtocolSession _protocolSession;
        public AMQProtocolSession ProtocolSession { get { return _protocolSession;  } }

        /// <summary>
        /// Maps from session id (Integer) to AmqChannel instance
        /// </summary>
        private readonly IDictionary _sessions = new LinkedHashtable();

        private ExceptionListenerDelegate _exceptionListener;

        private IConnectionListener _connectionListener;

        private ITransport _transport;
        public ITransport Transport { get { return _transport; } }
        
        /// <summary>
        /// Whether this connection is started, i.e. whether messages are flowing to consumers. It has no meaning for
        /// message publication.
        /// </summary>
        private bool _started;

        private AMQProtocolListener _protocolListener;
        public AMQProtocolListener ProtocolListener { get { return _protocolListener;  } }

        public IProtocolWriter ProtocolWriter
        {
            get { return _transport.ProtocolWriter; }
        }

        ProtocolWriter _protocolWriter;

        public ProtocolWriter ConvenientProtocolWriter
        {
            get { return _protocolWriter; }
        }

        public AMQConnection(IConnectionInfo connectionInfo)
        {
            if (connectionInfo == null)
            {
                throw new ArgumentException("ConnectionInfo must be specified");
            }
            _log.Debug("ConnectionInfo: " + connectionInfo);
            _connectionInfo = connectionInfo;
            _log.Debug("password = " + _connectionInfo.Password);
            _failoverPolicy = new FailoverPolicy(connectionInfo);

            // We are not currently connected.
            _connected = false;

            Exception lastException = null;
            do
            {
                try
                {
                    IBrokerInfo brokerInfo = _failoverPolicy.GetNextBrokerInfo();
                    _log.Debug("Connecting to " + brokerInfo);
                    MakeBrokerConnection(brokerInfo);
                    break;
                }
                catch (Exception e)
                {
                    lastException = e;
                    _log.Error("Unable to connect to broker " + _failoverPolicy.GetCurrentBrokerInfo(), e);
                    // XXX: Should perhaps break out of the do/while here if not a SocketException...
                }
            } while (!_connected && _failoverPolicy.FailoverAllowed());

            _log.Debug("Are we connected:" + _connected);

            if (!_connected)
            {
            	if ( lastException is AMQException )
            	{
            		throw lastException;
            	}
            	else
            	{
            		throw new AMQConnectionException("Unable to connect", lastException);
            	}
            }
            
        }

        /*private ITransport LoadTransportFromAssembly(string host, int port, String assemblyName, String transportType)
        {
            //Assembly assembly = Assembly.LoadFrom(assemblyName);
            Assembly assembly = Assembly.Load(assemblyName);

            foreach (Type type in assembly.GetTypes())
            {
                _log.Debug(String.Format("type = {0}", type));
            }

            Type transport = assembly.GetType(transportType);

            if (transport == null)
            {
                throw new ArgumentException(
                    String.Format("Type is not found in assembly. Type={0} Assembly={1}", transportType, assemblyName));
                
            }

            _log.Debug("transport = " + transport);
            _log.Debug("ctors = " + transport.GetConstructors());

            ConstructorInfo info = transport.GetConstructors()[0];
            ITransport result = (ITransport)info.Invoke(new object[] { host, port, this });

            _log.Debug("transport = " + result);

            return result;
        }*/

        public void Disconnect()
        {
            _transport.Close();
        }

        #region IConnection Members

        public string ClientID
        {
            get
            {
                CheckNotClosed();
                return _connectionInfo.ClientName;
            }
            set
            {
                CheckNotClosed();
                _connectionInfo.ClientName = value;
            }
        }

        public override void Close()
        {
            lock (FailoverMutex)
            {
                // atomically set to closed and check the _previous value was NOT CLOSED
                if (Interlocked.Exchange(ref _closed, CLOSED) == NOT_CLOSED)
                {
                    try
                    {
                        CloseAllSessions(null);
                        CloseConnection();
                    }
                    catch (AMQException e)
                    {
                        throw new QpidException("Error closing connection: " + e);
                    }
                }
            }
        }

        private void CloseConnection()
        {
            _stateManager.ChangeState(AMQState.CONNECTION_CLOSING);

            AMQFrame frame = ConnectionCloseBody.CreateAMQFrame(
                0, 200, "Qpid.NET client is closing the connection.", 0, 0);

            ProtocolWriter.Write(frame);

            _log.Debug("Blocking for connection close ok frame");

            Disconnect();
        }

        class CreateChannelFailoverSupport : FailoverSupport
        {
            private static readonly ILog _log = LogManager.GetLogger(typeof(CreateChannelFailoverSupport));

            private bool _transacted;
            private AcknowledgeMode _acknowledgeMode;
            int _prefetchHigh;
            int _prefetchLow;
            AMQConnection _connection;
            
            public CreateChannelFailoverSupport(AMQConnection connection, bool transacted, AcknowledgeMode acknowledgeMode, int prefetchHigh, int prefetchLow)
            {
                _connection = connection;
                _transacted = transacted;
                _acknowledgeMode = acknowledgeMode;
                _prefetchHigh = prefetchHigh;
                _prefetchLow = prefetchLow;
            }

            protected override object operation()
            {
                ushort channelId = _connection.NextChannelId();

                if (_log.IsDebugEnabled)
                {
                    _log.Debug("Write channel open frame for channel id " + channelId);
                }

                // We must create the channel and register it before actually sending the frame to the server to
                // open it, so that there is no window where we could receive data on the channel and not be set
                // up to handle it appropriately.
                AmqChannel channel = new AmqChannel(_connection, 
                        channelId, _transacted, _acknowledgeMode, _prefetchHigh, _prefetchLow);
                _connection.ProtocolSession.AddSessionByChannel(channelId, channel);
                _connection.RegisterSession(channelId, channel);

                bool success = false;
                try
                {
                    _connection.CreateChannelOverWire(channelId, _prefetchHigh, _prefetchLow, _transacted);
                    success = true;
                }
                catch (AMQException e)
                {
                    throw new QpidException("Error creating channel: " + e, e);
                }
                finally
                {
                    if (!success) {
                        _connection.ProtocolSession.RemoveSessionByChannel(channelId);
                        _connection.DeregisterSession(channelId);
                    }
                }

                if (_connection._started)
                {
                    channel.Start();
                }
                return channel;
            }
        }
        
        internal ushort NextChannelId()
        {
            return (ushort) Interlocked.Increment(ref _nextChannelId);
        }
        
        public IChannel CreateChannel(bool transacted, AcknowledgeMode acknowledgeMode)
        {
            return CreateChannel(transacted, acknowledgeMode, AmqChannel.DEFAULT_PREFETCH_HIGH_MARK);
        }

        public IChannel CreateChannel(bool transacted, AcknowledgeMode acknowledgeMode, int prefetch)
        {
           return CreateChannel(transacted, acknowledgeMode, prefetch, prefetch);
        }
   
        public IChannel CreateChannel(bool transacted, AcknowledgeMode acknowledgeMode, int prefetchHigh, int prefetchLow)
        {
            CheckNotClosed();
            if (ChannelLimitReached())
            {
                throw new ChannelLimitReachedException(_maximumChannelCount);
            }
            else
            {
                CreateChannelFailoverSupport operation =
                    new CreateChannelFailoverSupport(this, transacted, acknowledgeMode, prefetchHigh, prefetchLow);
                return (IChannel)operation.execute(this);
            }
        }

        public void CloseSession(AmqChannel channel)
        {
            // FIXME: Don't we need FailoverSupport here (as we have SyncWrite).
            _protocolSession.CloseSession(channel);
            
            AMQFrame frame = ChannelCloseBody.CreateAMQFrame(
                channel.ChannelId, 200, "JMS client closing channel", 0, 0);
            
            _log.Debug("Blocking for channel close frame for channel " + channel.ChannelId);
            _protocolWriter.SyncWrite(frame, typeof(ChannelCloseOkBody));
            _log.Debug("Received channel close frame");
            // When control resumes at this point, a reply will have been received that
            // indicates the broker has closed the channel successfully
        }

        public ExceptionListenerDelegate ExceptionListener
        {
            get
            {
                CheckNotClosed();
                return _exceptionListener;
            }
            set
            {
                CheckNotClosed();
                _exceptionListener = value;
            }
        }

        /// <summary>
        /// Start the connection, i.e. start flowing messages. Note that this method must be called only from a single thread
        /// and is not thread safe (which is legal according to the JMS specification).
        /// @throws JMSException
        /// </summary>
        public void Start()
        {
            CheckNotClosed();

            if (!_started)
            {
                foreach (DictionaryEntry lde in _sessions)
                {                   
                    AmqChannel s = (AmqChannel)lde.Value;
                    s.Start();
                }
                _started = true;
            }
        }

        public void Stop()
        {
            CheckNotClosed();

            if (_started)
            {
                foreach (DictionaryEntry lde in _sessions)
                {
                    AmqChannel s = (AmqChannel) lde.Value;
                    s.Stop();
                }
                _started = false;
            }
        }

        public IConnectionListener ConnectionListener
        {
            get { return _connectionListener; }
            set { _connectionListener = value; }
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Close();
        }

        #endregion

        private bool ChannelLimitReached()
        {
            return _maximumChannelCount != 0 && _sessions.Count == _maximumChannelCount;
        }

        /// <summary>
        /// Close all the sessions, either due to normal connection closure or due to an error occurring.
        /// @param cause if not null, the error that is causing this shutdown
        /// </summary>
        private void CloseAllSessions(Exception cause)
        {
            _log.Debug("Closing all session in connection " + this);
            ICollection sessions = new ArrayList(_sessions.Values);
            foreach (AmqChannel channel in sessions)
            {
                _log.Debug("Closing channel " + channel);
                if (cause != null)
                {
                    channel.ClosedWithException(cause);
                }
                else
                {
                    try
                    {
                        channel.Close();
                    }
                    catch (QpidException e)
                    {
                        _log.Error("Error closing channel: " + e);
                    }
                }
            }
            _log.Debug("Done closing all sessions in connection " + this);
        }

        public int MaximumChannelCount
        {
            get
            {
                CheckNotClosed();
                return _maximumChannelCount;
            }
        }
        
        internal void SetMaximumChannelCount(ushort maximumChannelCount)
        {
            CheckNotClosed();
            _maximumChannelCount = maximumChannelCount;
        }

        public uint MaximumFrameSize
        {
            get
            {
                return _maximumFrameSize;
            }

            set
            {
                _maximumFrameSize = value;
            }
        }

        public IDictionary Sessions
        {
            get
            {
                return _sessions;
            }
        }

        public string Host
        {
            get
            {
                return _failoverPolicy.GetCurrentBrokerInfo().Host;
            }
        }

        public int Port
        {
            get
            {
                return _failoverPolicy.GetCurrentBrokerInfo().Port;
            }
        }

        public string Username
        {
            get
            {
                return _connectionInfo.Username;
            }
        }

        public string Password
        {
            get
            {
                return _connectionInfo.Password;
            }
        }

        public string VirtualHost
        {
            get
            {
                return _connectionInfo.VirtualHost;
            }
        }

        /// <summary>
        /// Invoked by the AMQProtocolSession when a protocol session exception has occurred.
        /// This method sends the exception to a JMS exception listener, if configured, and
        /// propagates the exception to sessions, which in turn will propagate to consumers.
        /// This allows synchronous consumers to have exceptions thrown to them.
        /// </summary>
        /// <param name="cause">the exception</param>        
        public void ExceptionReceived(Exception cause)
        {
            if (_exceptionListener != null)
            {
                // Listener expects one of these...
                QpidException xe;

                if (cause is QpidException)
                {
                    xe = (QpidException) cause;
                }
                else
                {
                    xe = new QpidException("Exception thrown against " + ToString() + ": " + cause, cause);                    
                }
                // in the case of an IOException, MINA has closed the protocol session so we set _closed to true
                // so that any generic client code that tries to close the connection will not mess up this error
                // handling sequence
                if (cause is IOException)
                {
                    Interlocked.Exchange(ref _closed, CLOSED);
                }
#if __MonoCS__
                _exceptionListener(xe);
#else
                _exceptionListener.Invoke(xe);
#endif
            }
            else
            {
                _log.Error("Connection exception: " + cause);
            }

            // An undelivered is not fatal to the connections usability.
            if (!(cause is AMQUndeliveredException))
            {
                Interlocked.Exchange(ref _closed, CLOSED);
                CloseAllSessions(cause);
            }
            else
            {
                ;
            }
        }

        internal void RegisterSession(int channelId, AmqChannel channel)
        {
            _sessions[channelId] = channel;
        }

        internal void DeregisterSession(int channelId)
        {
            _sessions.Remove(channelId);
        }

        /**
         * Fire the preFailover event to the registered connection listener (if any)
         *
         * @param redirect true if this is the result of a redirect request rather than a connection error
         * @return true if no listener or listener does not veto change
         */
        public bool FirePreFailover(bool redirect)
        {
            bool proceed = true;
            if (_connectionListener != null)
            {
                proceed = _connectionListener.PreFailover(redirect);
            }
            return proceed;
        }

        /**
         * Fire the preResubscribe event to the registered connection listener (if any). If the listener
         * vetoes resubscription then all the sessions are closed.
         *
         * @return true if no listener or listener does not veto resubscription.
         * @throws JMSException
         */
        public bool FirePreResubscribe()
        {
            if (_connectionListener != null)
            {
                bool resubscribe = _connectionListener.PreResubscribe();
                if (!resubscribe)
                {
                    MarkAllSessionsClosed();
                }
                return resubscribe;
            }
            else
            {
                return true;
            }
        }

        /**
         * Marks all sessions and their children as closed without sending any protocol messages. Useful when
         * you need to mark objects "visible" in userland as closed after failover or other significant event that
         * impacts the connection.
         * <p/>
         * The caller must hold the failover mutex before calling this method.
         */
        private void MarkAllSessionsClosed()
        {
            //LinkedList sessionCopy = new LinkedList(_sessions.values());
            ArrayList sessionCopy = new ArrayList(_sessions.Values);
            foreach (AmqChannel session in sessionCopy)
            {
                session.MarkClosed();
            }
            _sessions.Clear();
        }

        /**
         * Fires a failover complete event to the registered connection listener (if any).
         */
        public void FireFailoverComplete()
        {
            if (_connectionListener != null)
            {
                _connectionListener.FailoverComplete();
            }
        }

        public bool AttemptReconnection(String host, int port, SslOptions sslConfig)
        {
            IBrokerInfo bd = new AmqBrokerInfo("amqp", host, port, sslConfig);

            _failoverPolicy.setBroker(bd);

            try
            {
                MakeBrokerConnection(bd);
                return true;
            }
            catch (Exception e)
            {
                _log.Debug("Unable to connect to broker at " + bd, e);
                AttemptReconnection();
            }
            return false;
        }

        private void MakeBrokerConnection(IBrokerInfo brokerDetail)
        {
            try
            {
                _stateManager = new AMQStateManager();
                _protocolListener = new AMQProtocolListener(this, _stateManager);
                _protocolListener.AddFrameListener(_stateManager);

                /*
                // Currently there is only one transport option - BlockingSocket.
                String assemblyName = "Apache.Qpid.Client.Transport.Socket.Blocking.dll";
                String transportType = "Apache.Qpid.Client.Transport.Socket.Blocking.BlockingSocketTransport";

                // Load the transport assembly dynamically.
                _transport = LoadTransportFromAssembly(brokerDetail.getHost(), brokerDetail.getPort(), assemblyName, transportType);
                */

                _transport = new BlockingSocketTransport();
                
                // Connect.
                _transport.Connect(brokerDetail, this);                
                _protocolWriter = new ProtocolWriter(_transport.ProtocolWriter, _protocolListener);
                _protocolSession = new AMQProtocolSession(_transport.ProtocolWriter, _transport, this);
                _protocolListener.ProtocolSession = _protocolSession;

                // Now start the connection "handshake".
                _transport.ProtocolWriter.Write(new ProtocolInitiation());

                // Blocks until the connection has been opened.
                _stateManager.AttainState(AMQState.CONNECTION_OPEN);

                _failoverPolicy.attainedConnection();

                // XXX: Again this should be changed to a suitable notify.
                _connected = true;
            }
            catch (AMQException e)
            {
                _lastAMQException = e;
                throw; // rethrow
            }
        }

        public bool AttemptReconnection()
        {
            while (_failoverPolicy.FailoverAllowed())
            {
                try
                {
                    MakeBrokerConnection(_failoverPolicy.GetNextBrokerInfo());
                    return true;
                }
                catch (Exception e)
                {
                    if (!(e is AMQException))
                    {
                        _log.Debug("Unable to connect to broker at " + _failoverPolicy.GetCurrentBrokerInfo(), e);
                    }
                    else
                    {
                        _log.Debug(e.Message + ":Unable to connect to broker at " + _failoverPolicy.GetCurrentBrokerInfo());
                    }
                }
            }

            // Connection unsuccessful.
            return false;
        }

        /**
         * For all channels, and for all consumers in those sessions, resubscribe. This is called during failover handling.
         * The caller must hold the failover mutex before calling this method.
         */
        public void ResubscribeChannels()
        {
            ArrayList channels = new ArrayList(_sessions.Values);
            _log.Debug(String.Format("Resubscribing sessions = {0} sessions.size={1}", channels, channels.Count));
            foreach (AmqChannel channel in channels)
            {
                _protocolSession.AddSessionByChannel(channel.ChannelId, channel);
                ReopenChannel(
                   channel.ChannelId, 
                   channel.DefaultPrefetchHigh, 
                   channel.DefaultPrefetchLow, 
                   channel.Transacted
                   );
                channel.ReplayOnFailOver();
            }
        }

        private void ReopenChannel(ushort channelId, int prefetchHigh, int prefetchLow, bool transacted)
        {
            _log.Debug(string.Format("Reopening channel id={0} prefetchHigh={1} prefetchLow={2} transacted={3}",
                channelId, prefetchHigh, prefetchLow, transacted));
            try
            {
                CreateChannelOverWire(channelId, prefetchHigh, prefetchLow, transacted);
            }
            catch (AMQException e)
            {
                _protocolSession.RemoveSessionByChannel(channelId);
                DeregisterSession(channelId);
                throw new AMQException("Error reopening channel " + channelId + " after failover: " + e);
            }
        }

        void CreateChannelOverWire(ushort channelId, int prefetchHigh, int prefetchLow, bool transacted)
        {
            _protocolWriter.SyncWrite(ChannelOpenBody.CreateAMQFrame(channelId, null), typeof (ChannelOpenOkBody));
            
            // Don't use the BasicQos frame if connecting to OpenAMQ (at it is not support). We 
            // know this when we connection using AMQP 0.7
            if (ProtocolInitiation.CURRENT_PROTOCOL_VERSION_MAJOR != 7)
            {
                // Basic.Qos frame appears to not be supported by OpenAMQ 1.0d.
                _protocolWriter.SyncWrite(BasicQosBody.CreateAMQFrame(channelId, 0, (ushort)prefetchHigh, false), typeof (BasicQosOkBody));
            }
            
            if (transacted)
            {
                if (_log.IsDebugEnabled)
                {
                    _log.Debug("Issuing TxSelect for " + channelId);
                }
                _protocolWriter.SyncWrite(TxSelectBody.CreateAMQFrame(channelId), typeof(TxSelectOkBody));
            }
        }

        public String toURL()
        {
            return _connectionInfo.AsUrl();
        }

        class HeartBeatThread
        {
            int _heartbeatMillis;
            IProtocolWriter _protocolWriter;
            bool _run = true;
            
            public HeartBeatThread(IProtocolWriter protocolWriter, int heartbeatMillis)
            {
                _protocolWriter = protocolWriter;
                _heartbeatMillis = heartbeatMillis;
            }
            
            public void Run()
            {
                while (_run)
                {
                    Thread.Sleep(_heartbeatMillis);
                    if (!_run) break;
                    _log.Debug("Sending heartbeat");
                    // TODO: Can we optimise this so that heartbeats are only written when we haven't sent anything recently to the broker?
                    _protocolWriter.Write(HeartbeatBody.FRAME);
                }
                _log.Debug("Heatbeat thread stopped");
            }
            
            public void Stop()
            {
                _run = false;
            }
        }
        
        public void StartHeartBeatThread(int heartbeatSeconds)
        {
            _log.Debug("Starting new heartbeat thread");
            _heartBeatRunner = new HeartBeatThread(ProtocolWriter, heartbeatSeconds * 1000);
            _heartBeatThread = new Thread(new ThreadStart(_heartBeatRunner.Run));
            _heartBeatThread.Name = "HeartBeat";
            _heartBeatThread.Start();
        }

        public void StopHeartBeatThread()
        {
            if (_heartBeatRunner != null)
            {
                _log.Debug("Stopping old heartbeat thread");
                _heartBeatRunner.Stop();
            }
        }
    }
}
