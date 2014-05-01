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
using Apache.Qpid.Framing;
using Apache.Qpid.Protocol;

namespace Apache.Qpid.Client.Handler
{
    public class ConnectionCloseMethodHandler : IStateAwareMethodListener
    {
        private static readonly ILog _logger = LogManager.GetLogger(typeof(ConnectionCloseMethodHandler));

        public void MethodReceived(AMQStateManager stateManager, AMQMethodEvent evt)
        {
            _logger.Debug("ConnectionClose frame received");
            ConnectionCloseBody method = (ConnectionCloseBody) evt.Method;

            int errorCode = method.ReplyCode;
            String reason = method.ReplyText;

            // send CloseOK
            evt.ProtocolSession.WriteFrame(ConnectionCloseOkBody.CreateAMQFrame(evt.ChannelId));
            
            if ( errorCode != AMQConstant.REPLY_SUCCESS.Code )
            {
               if ( errorCode == AMQConstant.NOT_ALLOWED.Code )
               {
                  _logger.Info("Authentication Error: " + Thread.CurrentThread.Name);
                  evt.ProtocolSession.CloseProtocolSession();

                  //todo this is a bit of a fudge (could be conssidered such as each new connection needs a new state manager or at least a fresh state.
                  stateManager.ChangeState(AMQState.CONNECTION_NOT_STARTED);

                  throw new AMQAuthenticationException(errorCode, reason);
               } else
               {
                  _logger.Info("Connection close received with error code " + errorCode);
                  throw new AMQConnectionClosedException(errorCode, "Error: " + reason);
               }
            }
            // this actually closes the connection in the case where it is not an error.
            evt.ProtocolSession.CloseProtocolSession();
            stateManager.ChangeState(AMQState.CONNECTION_CLOSED);
        }
    }
}
