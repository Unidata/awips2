/*
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
*/
using System;
using System.Text;
using System.Threading;
using org.apache.qpid.transport;
using org.apache.qpid.transport.network.io;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.client
{
    public class Client : IClient
    {
        private Connection _conn;
        private static readonly Logger _log = Logger.Get(typeof (Client));
        private const long timeout = 60000;
        private bool _isClosed;
        private readonly Object _closeOK;
        private IClosedListener _closedListner;

        public event EventHandler<ExceptionArgs> ExceptionRaised;
        public event EventHandler ConnectionLost;

        public bool IsClosed
        {
            get { return _isClosed; }
            set
            {
                _isClosed = value;
                if(_isClosed && ConnectionLost != null)
                    ConnectionLost(this, EventArgs.Empty);
            }
        }

        public Object CloseOk
        {
            get { return _closeOK; }
        }

        public Client()
        {
            _isClosed = false;
            _closeOK = new object();
        }

        #region Interface IClient
        
        /// <summary>
        /// Establishes a connection with a broker using the provided user auths 
        /// 
        /// </summary>
        /// <param name="host">Host name on which a broker is deployed</param>
        /// <param name="port">Broker port </param>
        /// <param name="virtualHost">virtual host name</param>
        /// <param name="username">User Name</param>
        /// <param name="password">Password</param>
        public void Connect(String host, int port, String virtualHost, String username, String password)
        {
            _log.Debug(String.Format("Client Connecting to host {0}; port {1}; virtualHost {2}; username {3}", host,
                                     port, virtualHost, username));
            ClientConnectionDelegate connectionDelegate = new ClientConnectionDelegate(this, username, password);
            ManualResetEvent negotiationComplete = new ManualResetEvent(false);
            connectionDelegate.SetCondition(negotiationComplete);
            connectionDelegate.VirtualHost = virtualHost;
            _conn = IoTransport.Connect(host, port, connectionDelegate);
            
            _conn.Send(new ProtocolHeader(1, 0, 10));
            negotiationComplete.WaitOne();

            if (connectionDelegate.Exception != null)
                throw connectionDelegate.Exception;

            connectionDelegate.SetCondition(null);

        }

        /// <summary>
        /// Establishes a connection with a broker using SSL
        /// 
        /// </summary>
        /// <param name="host">Host name on which a broker is deployed</param>
        /// <param name="port">Broker port </param>
        /// <param name="virtualHost">virtual host name</param>
        /// <param name="username">User Name</param>
        /// <param name="password">Password</param>
        /// <param name="serverName">Name of the SSL server</param>
        /// <param name="certPath">Path to the X509 certificate to be used for client authentication</param>
        /// <param name="rejectUntrusted">If true connection will not be established if the broker is not trusted</param>
        public void ConnectSSL(String host, int port, String virtualHost, String username, String password, string serverName, string certPath, bool rejectUntrusted)
        {
            _log.Debug(String.Format("Client Connecting to host {0}; port {1}; virtualHost {2}; username {3}", host,
                                     port, virtualHost, username));
            _log.Debug(String.Format("SSL paramters: serverName: {0}; certPath: {1}; rejectUntrusted: {2}", serverName, certPath, rejectUntrusted));
            ClientConnectionDelegate connectionDelegate = new ClientConnectionDelegate(this, username, password);
            ManualResetEvent negotiationComplete = new ManualResetEvent(false);
            connectionDelegate.SetCondition(negotiationComplete);
            connectionDelegate.VirtualHost = virtualHost;
            _conn = IoSSLTransport.Connect(host, port, serverName, certPath, rejectUntrusted, connectionDelegate);

            _conn.Send(new ProtocolHeader(1, 0, 10));
            negotiationComplete.WaitOne();

            if (connectionDelegate.Exception != null)
                throw connectionDelegate.Exception;

            connectionDelegate.SetCondition(null);
        }

        public void Close()
        {
            Channel ch = _conn.GetChannel(0);
            ch.ConnectionClose(ConnectionCloseCode.NORMAL, "client is closing");
            lock (CloseOk)
            {
                DateTime start = DateTime.Now;
                long elapsed = 0;
                while (!IsClosed && elapsed < timeout)
                {
                    Monitor.Wait(CloseOk, (int) (timeout - elapsed));
                    elapsed = DateTime.Now.Subtract(start).Milliseconds;
                }
                if (!IsClosed)
                {
                    throw new Exception("Timed out when closing connection");
                }
                _conn.Close();
            }
        }

        public IClientSession CreateSession(long expiryInSeconds)
        {
            Channel ch = _conn.GetChannel();
            ClientSession ssn = new ClientSession(Encoding.UTF8.GetBytes(UUID.RandomUuid().ToString()));
            ssn.Attach(ch);
            ssn.SessionAttach(ssn.GetName());
            ssn.SessionRequestTimeout(expiryInSeconds);
            return ssn;
        }

        public IClosedListener ClosedListener
        {
            set { _closedListner = value; }
            get { return _closedListner; }
        }       

        #endregion

        public void RaiseException(Exception exception)
        {
            if (ExceptionRaised != null)
                ExceptionRaised(this, new ExceptionArgs(exception));
        }
    }
}
