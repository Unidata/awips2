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
using Apache.Qpid.Protocol;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Handler
{
    public class ChannelCloseMethodHandler : IStateAwareMethodListener
    {
        private static readonly ILog _logger = LogManager.GetLogger(typeof(ChannelCloseMethodHandler));

        public void MethodReceived(AMQStateManager stateManager, AMQMethodEvent evt)
        {
            _logger.Debug("ChannelClose method received");
            ChannelCloseBody method = (ChannelCloseBody) evt.Method;

            int errorCode = method.ReplyCode;
            string reason = method.ReplyText;
            if (_logger.IsDebugEnabled)
            {
                _logger.Debug("Channel close reply code: " + errorCode + ", reason: " + reason);
            }

            AMQFrame frame = ChannelCloseOkBody.CreateAMQFrame(evt.ChannelId);
            evt.ProtocolSession.WriteFrame(frame);

            if ( errorCode != AMQConstant.REPLY_SUCCESS.Code )
            {
               _logger.Debug("Channel close received with errorCode " + errorCode + ", throwing exception");
               if ( errorCode == AMQConstant.NO_CONSUMERS.Code )
                  throw new AMQNoConsumersException(reason);
               if ( errorCode == AMQConstant.NO_ROUTE.Code )
                  throw new AMQNoRouteException(reason);
               if ( errorCode == AMQConstant.INVALID_ARGUMENT.Code )
                  throw new AMQInvalidArgumentException(reason);
               if ( errorCode == AMQConstant.INVALID_ROUTING_KEY.Code )
                  throw new AMQInvalidRoutingKeyException(reason);
               // any other
               throw new AMQChannelClosedException(errorCode, "Error: " + reason);
            }
            evt.ProtocolSession.ChannelClosed(evt.ChannelId, errorCode, reason);
        }
    }
}



