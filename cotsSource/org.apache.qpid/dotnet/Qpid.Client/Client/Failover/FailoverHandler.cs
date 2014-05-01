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
using System.Threading;
using log4net;
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Client.State;

namespace Apache.Qpid.Client.Failover
{
    public class FailoverHandler
    {
        private static readonly ILog _log = LogManager.GetLogger(typeof(FailoverHandler));

        private AMQConnection _connection;

        /**
         * Used where forcing the failover host
         */
        private String _host;

        /**
         * Used where forcing the failover port
         */
        private int _port;

        public FailoverHandler(AMQConnection connection)
        {
            _connection = connection;
        }

        public void Run()
        {
            if (Thread.CurrentThread.IsBackground)
            {
                throw new InvalidOperationException("FailoverHandler must Run on a non-background thread.");
            }

            AMQProtocolListener pl = _connection.ProtocolListener;
            pl.FailoverLatch = new ManualResetEvent(false);

            // We wake up listeners. If they can handle failover, they will extend the
            // FailoverSupport class and will in turn block on the latch until failover
            // has completed before retrying the operation
            _connection.ProtocolListener.PropagateExceptionToWaiters(new FailoverException("Failing over about to start"));

            // Since failover impacts several structures we protect them all with a single mutex. These structures
            // are also in child objects of the connection. This allows us to manipulate them without affecting
            // client code which runs in a separate thread.
            lock (_connection.FailoverMutex)
            {
                _log.Info("Starting failover process");

                // We switch in a new state manager temporarily so that the interaction to get to the "connection open"
                // state works, without us having to terminate any existing "state waiters". We could theoretically
                // have a state waiter waiting until the connection is closed for some reason. Or in future we may have
                // a slightly more complex state model therefore I felt it was worthwhile doing this.
                AMQStateManager existingStateManager = _connection.ProtocolListener.StateManager;
                _connection.ProtocolListener.StateManager = new AMQStateManager();
                if (!_connection.FirePreFailover(_host != null))
                {
                    _connection.ProtocolListener.StateManager = existingStateManager;
                    if (_host != null)
                    {
                        _connection.ExceptionReceived(new AMQDisconnectedException("Redirect was vetoed by client"));
                    }
                    else
                    {
                        _connection.ExceptionReceived(new AMQDisconnectedException("Failover was vetoed by client"));
                    }
                    pl.FailoverLatch.Set();
                    pl.FailoverLatch = null;
                    return;
                }
                bool failoverSucceeded;
                // when host is non null we have a specified failover host otherwise we all the client to cycle through
                // all specified hosts

                // if _host has value then we are performing a redirect.
                if (_host != null)
                {
                   // todo: fix SSL support!
                    failoverSucceeded = _connection.AttemptReconnection(_host, _port, null);
                }
                else
                {
                    failoverSucceeded = _connection.AttemptReconnection();
                }

                // XXX: at this point it appears that we are going to set StateManager to existingStateManager in
                // XXX: both paths of control.
                if (!failoverSucceeded)
                {
                    _connection.ProtocolListener.StateManager = existingStateManager;
                    _connection.ExceptionReceived(
                        new AMQDisconnectedException("Server closed connection and no failover " +
                                                     "was successful"));
                }
                else
                {
                    _connection.ProtocolListener.StateManager = existingStateManager;
                    try
                    {
                        if (_connection.FirePreResubscribe())
                        {
                            _log.Info("Resubscribing on new connection");
                            _connection.ResubscribeChannels();
                        }
                        else
                        {
                            _log.Info("Client vetoed automatic resubscription");
                        }
                        _connection.FireFailoverComplete();
                        _connection.ProtocolListener.FailoverState = FailoverState.NOT_STARTED;
                        _log.Info("Connection failover completed successfully");
                    }
                    catch (Exception e)
                    {
                        _log.Info("Failover process failed - exception being propagated by protocol handler");
                        _connection.ProtocolListener.FailoverState = FailoverState.FAILED;
                        try
                        {
                            _connection.ProtocolListener.OnException(e);
                        }
                        catch (Exception ex)
                        {
                            _log.Error("Error notifying protocol session of error: " + ex, ex);
                        }
                    }
                }
            }
            pl.FailoverLatch.Set();
        }

        public String getHost()
        {
            return _host;
        }

        public void setHost(String host)
        {
            _host = host;
        }

        public int getPort()
        {
            return _port;
        }

        public void setPort(int port)
        {
            _port = port;
        }
    }
}


