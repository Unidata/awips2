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
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Client.State;
using Apache.Qpid.Framing;
using Apache.Qpid.Sasl;

namespace Apache.Qpid.Client.Handler
{
    public class ConnectionSecureMethodHandler : IStateAwareMethodListener
    {
        public void MethodReceived(AMQStateManager stateManager, AMQMethodEvent evt)
        {
            ISaslClient saslClient = evt.ProtocolSession.SaslClient;
            if ( saslClient == null )
            {
               throw new AMQException("No SASL client set up - cannot proceed with authentication");
            }


            ConnectionSecureBody body = (ConnectionSecureBody)evt.Method;

            try
            {
                // Evaluate server challenge
                byte[] response = saslClient.EvaluateChallenge(body.Challenge);
                // AMQP version change: Hardwire the version to 0-8 (major=8, minor=0)
                // TODO: Connect this to the session version obtained from ProtocolInitiation for this session.
                // Be aware of possible changes to parameter order as versions change.
                AMQFrame responseFrame = ConnectionSecureOkBody.CreateAMQFrame(
                    evt.ChannelId, response);
                evt.ProtocolSession.WriteFrame(responseFrame);
            } catch ( SaslException e )
            {
                throw new AMQException("Error processing SASL challenge: " + e, e);
            }
        }
    }
}



