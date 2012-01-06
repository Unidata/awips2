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
using log4net;
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Client.State;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Handler
{
    public class ConnectionTuneMethodHandler : IStateAwareMethodListener
    {
        private static readonly ILog _logger = LogManager.GetLogger(typeof(ConnectionTuneMethodHandler));

        public void MethodReceived(AMQStateManager stateManager, AMQMethodEvent evt)
        {
            _logger.Debug("ConnectionTune frame received");
            ConnectionTuneBody frame = (ConnectionTuneBody) evt.Method;
            AMQProtocolSession session = evt.ProtocolSession;

            ConnectionTuneParameters parameters = session.ConnectionTuneParameters;
            if (parameters == null)
            {
                parameters = new ConnectionTuneParameters();
            }

            _logger.Debug(String.Format("ConnectionTune.heartbeat = {0}.", frame.Heartbeat));

            parameters.FrameMax = frame.FrameMax;
            parameters.Heartbeat = frame.Heartbeat;
            session.ConnectionTuneParameters = parameters;

            stateManager.ChangeState(AMQState.CONNECTION_NOT_OPENED);
            session.WriteFrame(ConnectionTuneOkBody.CreateAMQFrame(
                                   evt.ChannelId, frame.ChannelMax, frame.FrameMax, frame.Heartbeat));
            session.WriteFrame(ConnectionOpenBody.CreateAMQFrame(
                                   evt.ChannelId, session.AMQConnection.VirtualHost, null, true));

            if (frame.Heartbeat > 0)
            {
                evt.ProtocolSession.AMQConnection.StartHeartBeatThread(frame.Heartbeat);
            }
        }
    }
}
