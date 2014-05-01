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
package org.apache.qpid.server.handler;

import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.BasicRecoverBody;
import org.apache.qpid.framing.BasicRecoverOkBody;
import org.apache.qpid.framing.ProtocolVersion;
import org.apache.qpid.framing.AMQMethodBody;
import org.apache.qpid.framing.amqp_8_0.MethodRegistry_8_0;
import org.apache.qpid.protocol.AMQMethodEvent;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.server.state.StateAwareMethodListener;

public class BasicRecoverMethodHandler implements StateAwareMethodListener<BasicRecoverBody>
{
    private static final Logger _logger = Logger.getLogger(BasicRecoverMethodHandler.class);

    private static final BasicRecoverMethodHandler _instance = new BasicRecoverMethodHandler();

    public static BasicRecoverMethodHandler getInstance()
    {
        return _instance;
    }

    public void methodReceived(AMQStateManager stateManager, BasicRecoverBody body, int channelId) throws AMQException
    {
        AMQProtocolSession session = stateManager.getProtocolSession();

        _logger.debug("Recover received on protocol session " + session + " and channel " + channelId);
        AMQChannel channel = session.getChannel(channelId);


        if (channel == null)
        {
            throw body.getChannelNotFoundException(channelId);
        }

        channel.resend(body.getRequeue());

        // Qpid 0-8 hacks a synchronous -ok onto recover.
        // In Qpid 0-9 we create a separate sync-recover, sync-recover-ok pair to be "more" compliant
        if(session.getProtocolVersion().equals(ProtocolVersion.v8_0))
        {
            MethodRegistry_8_0 methodRegistry = (MethodRegistry_8_0) session.getMethodRegistry();
            AMQMethodBody recoverOk = methodRegistry.createBasicRecoverOkBody();
            session.writeFrame(recoverOk.generateFrame(channelId));

        }

    }
}
