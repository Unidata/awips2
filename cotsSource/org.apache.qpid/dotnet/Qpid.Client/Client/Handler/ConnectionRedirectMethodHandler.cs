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
using log4net;
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Client.State;

namespace Apache.Qpid.Client.Handler
{
    public class ConnectionRedirectMethodHandler : IStateAwareMethodListener
    {
//        private static readonly ILog _logger = LogManager.GetLogger(typeof(ConnectionRedirectMethodHandler));

        private const int DEFAULT_REDIRECT_PORT = 5672;

        private static ConnectionRedirectMethodHandler _handler = new ConnectionRedirectMethodHandler();

        public static ConnectionRedirectMethodHandler GetInstance()
        {
            return _handler;
        }

        private ConnectionRedirectMethodHandler()
        {
        }

        public void MethodReceived(AMQStateManager stateManager, AMQMethodEvent evt)
        {
            /*_logger.Info("ConnectionRedirect frame received");
            ConnectionRedirectBody method = (ConnectionRedirectBody) evt.Method;

            // the host is in the form hostname:port with the port being optional
            int portIndex = method.Host.IndexOf(':');
            String host;
            int port;
            if (portIndex == -1)
            {
                host = method.Host;
                port = DEFAULT_REDIRECT_PORT;
            }
            else
            {
                host = method.Host.Substring(0, portIndex);
                port = Int32.Parse(method.Host.Substring(portIndex + 1));
            }
            evt.ProtocolSession.Failover(host, port);*/
        }
    }
}


