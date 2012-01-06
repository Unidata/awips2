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
using System.Threading;
using log4net;
using Apache.Qpid.Client.Failover;
using Apache.Qpid.Client.Protocol.Listener;
using Apache.Qpid.Client.State;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Protocol
{
    /// <summary>
    /// AMQProtocolListener 
    ///
    /// <p/>Fail-over state transition rules...
    ///
    /// <p/>The failover handler is created when the session is created since it needs a reference to the IoSession in order
    /// to be able to send errors during failover back to the client application. The session won't be available in the case
    /// when failing over due to a Connection.Redirect message from the broker.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Track fail over state of a connection.
    /// <tr><td> Manage method listeners. <td> IAMQMethodListener
    /// <tr><td> Receive notification of all IO errors on a connection. <td> IoHandler
    /// <tr><td> Inform method listeners of all method events on a connection. <td> IAMQMethodListener
    /// <tr><td> Inform method listeners of all error events on a connection. <td> IAMQMethodListener
    /// </table>
    ///
    /// <b>Todo:</b> The broker will close the connection with no warning if authentication fails. This may result in the fail-over process being
    /// triggered, when it should not be.
    ///
    /// </summary>
    public class AMQProtocolListener : IProtocolListener
    {
        /// <summary>Used for debugging.</summary>
        private static readonly ILog _log = LogManager.GetLogger(typeof(AMQProtocolListener));

        /// <summary>
        /// Holds the failover handler for the connection. When a failure is detected, and the current failover state allows it,
        /// the failover process is handed off to this handler.
        /// </summary>
        private FailoverHandler _failoverHandler;

        /// <summary>Tracks the current fail-over state.</summary>
        internal FailoverState _failoverState = FailoverState.NOT_STARTED;

        internal FailoverState FailoverState
        {
            get { return _failoverState; }
            set { _failoverState = value; }
        }

        internal ManualResetEvent FailoverLatch;

        AMQConnection _connection;
        AMQStateManager _stateManager;

        public AMQStateManager StateManager
        {
            get { return _stateManager; }
            set { _stateManager = value; }
        }

        private readonly ArrayList _frameListeners = ArrayList.Synchronized(new ArrayList());
        
        AMQProtocolSession _protocolSession = null;

        private readonly Object _lock = new Object();

        public AMQProtocolSession ProtocolSession { set { _protocolSession = value; } }
        
        public AMQProtocolListener(AMQConnection connection, AMQStateManager stateManager)
        {
            _connection = connection;
            _stateManager = stateManager;
            _failoverHandler = new FailoverHandler(connection);
        }

        public void OnMessage(IDataBlock message)
        {
            // Handle incorrect protocol version.
            if (message is ProtocolInitiation)
            {
                string error = String.Format("Protocol mismatch - {0}", message.ToString());
                AMQException e = new AMQProtocolHeaderException(error);
                _log.Error("Closing connection because of protocol mismatch", e);
                //_protocolSession.CloseProtocolSession();
                _stateManager.Error(e);
                return;
            }

            AMQFrame frame = (AMQFrame)message;

            if (frame.BodyFrame is AMQMethodBody)
            {
                if (_log.IsDebugEnabled)
                {
                    _log.Debug("Method frame received: " + frame);
                }
                AMQMethodEvent evt = new AMQMethodEvent(frame.Channel, (AMQMethodBody)frame.BodyFrame, _protocolSession);
                try
                {
                    bool wasAnyoneInterested = false;
                    lock (_frameListeners.SyncRoot)
                    {
                        foreach (IAMQMethodListener listener in _frameListeners)
                        {
                            wasAnyoneInterested = listener.MethodReceived(evt) || wasAnyoneInterested;
                        }
                    }
                    if (!wasAnyoneInterested)
                    {
                        throw new AMQException("AMQMethodEvent " + evt + " was not processed by any listener.");
                    }
                }
                catch (Exception e)
                {
                    foreach (IAMQMethodListener listener in _frameListeners)
                    {
                        listener.Error(e);
                    }
                }
            }
            else if (frame.BodyFrame is ContentHeaderBody)
            {
                _protocolSession.MessageContentHeaderReceived(frame.Channel,
                                                              (ContentHeaderBody)frame.BodyFrame);
            }
            else if (frame.BodyFrame is ContentBody)
            {
                _protocolSession.MessageContentBodyReceived(frame.Channel,
                                                            (ContentBody)frame.BodyFrame);
            }
            else if (frame.BodyFrame is HeartbeatBody)
            {
                _log.Debug("HeartBeat received");
            }
        }

        /// <summary>
        /// Receives notification of any IO exceptions on the connection.
        ///
        /// <p/>Upon receipt of a connection closed exception or any IOException, the fail-over process is attempted. If the fail-over fails, then
        /// all method listeners and the application connection object are notified of the connection failure exception.
        ///
        /// <p/>All other exception types are propagated to all method listeners.
        /// </summary>
        public void OnException(Exception cause)
        {
            _log.Warn("public void OnException(Exception cause = " + cause + "): called");

            // Ensure that the method listener set cannot be changed whilst this exception is propagated to all listeners. This also 
            // ensures that this exception is fully propagated to all listeners, before another one can be processed.
            lock (_lock)
            {
                if (cause is AMQConnectionClosedException || cause is System.IO.IOException)
                {
                    // Try a fail-over because the connection has failed.
                    FailoverState failoverState = AttemptFailover();

                    // Check if the fail-over has failed, in which case notify all method listeners of the exception.
                    // The application connection object is also notified of the failure of the connection with the exception.
                    if (failoverState == FailoverState.FAILED)
                    {
                        _log.Debug("Fail-over has failed. Notifying all method listeners of the exception.");

                        AMQException amqe = new AMQException("Protocol handler error: " + cause, cause);
                        PropagateExceptionToWaiters(amqe);
                        _connection.ExceptionReceived(cause);
                    }
                }
                // Notify all method listeners of the exception.
                else
                {
                    PropagateExceptionToWaiters(cause);
                    _connection.ExceptionReceived(cause);
                }
            }
        }

        /// <summary>
        /// Tries to fail-over the connection, if the connection policy will permit it, and the fail-over process has not yet been
        /// started. If the connection does not allow fail-over then an exception will be raised. If a fail-over is already in progress
        /// this method allows it to continue to run and will do nothing.
        ///
        /// <p/>This method should only be called when the connection has been remotely closed.
        /// </summary>
        ///
        /// <returns>The fail-over state at the end of this attempt.</returns>
        private FailoverState AttemptFailover()
        {
            _log.Debug("private void AttemptFailover(): called");
            _log.Debug("_failoverState = " + _failoverState);

            // Ensure that the connection stops sending heart beats, if it still is.
            _connection.StopHeartBeatThread();

            // Check that the connection policy allows fail-over to be attempted.
            if (!_connection.IsFailoverAllowed)
            {
                _log.Debug("Connection does not allowed to failover");
                _connection.ExceptionReceived(
                    new AMQDisconnectedException("Broker closed connection and reconnection is not permitted."));
            }

            // Check if connection was closed deliberately by the application, in which case no fail-over is attempted.
            if (_connection.Closed)
            {
                return _failoverState;
            }

            // If the connection allows fail-over and fail-over has not yet been started, then it is started and the fail-over state is 
            // advanced to 'in progress'
            if (_failoverState == FailoverState.NOT_STARTED && _connection.IsFailoverAllowed)
            {
                _log.Info("Starting the fail-over process.");

                _failoverState = FailoverState.IN_PROGRESS;
                StartFailoverThread();
            }

            return _failoverState;
        }

        /// <summary>
        /// There are two cases where we have other threads potentially blocking for events to be handled by this
        /// class. These are for the state manager (waiting for a state change) or a frame listener (waiting for a
        /// particular type of frame to arrive). When an error occurs we need to notify these waiters so that they can
        /// react appropriately.
        /// 
        /// <param name="e">the exception to propagate</param>
        /// </summary>
        public void PropagateExceptionToWaiters(Exception e)
        {
            // FIXME: not sure if required as StateManager is in _frameListeners. Probably something to do with fail-over.
            _stateManager.Error(e);
            lock ( _lock )
            {
               foreach ( IAMQMethodListener listener in _frameListeners )
               {
                  listener.Error(e);
               }
            }
        }

        public void AddFrameListener(IAMQMethodListener listener)
        {
           lock ( _lock )
           {
              _frameListeners.Add(listener);
           }
        }

        public void RemoveFrameListener(IAMQMethodListener listener)
        {
            if (_log.IsDebugEnabled)
            {
                _log.Debug("Removing frame listener: " + listener.ToString());
            }
            lock ( _lock )
            {
               _frameListeners.Remove(listener);
            }
        }

        public void BlockUntilNotFailingOver()
        {
            if (FailoverLatch != null)
            {
                FailoverLatch.WaitOne();
            }
        }

        /// <summary>
        ///  "Failover" for redirection.
        /// </summary>
        /// <param name="host"></param>
        /// <param name="port"></param>
        public void Failover(string host, int port)
        {
            _failoverHandler.setHost(host);
            _failoverHandler.setPort(port);
            // see javadoc for FailoverHandler to see rationale for separate thread
            StartFailoverThread();
        }

        private void StartFailoverThread()
        {
            Thread failoverThread = new Thread(new ThreadStart(_failoverHandler.Run));
            failoverThread.Name = "Failover";
            // Do not inherit daemon-ness from current thread as this can be a daemon
            // thread such as a AnonymousIoService thread.
            failoverThread.IsBackground = false;
            failoverThread.Start();
        }
    }
}
