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
package org.apache.qpid.client.handler;

import org.apache.qpid.AMQException;
import org.apache.qpid.client.ConnectionTuneParameters;
import org.apache.qpid.client.protocol.AMQProtocolSession;
import org.apache.qpid.client.state.AMQState;
import org.apache.qpid.client.state.StateAwareMethodListener;
import org.apache.qpid.framing.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ConnectionTuneMethodHandler implements StateAwareMethodListener<ConnectionTuneBody>
{
    private static final Logger _logger = LoggerFactory.getLogger(ConnectionTuneMethodHandler.class);

    private static final ConnectionTuneMethodHandler _instance = new ConnectionTuneMethodHandler();

    public static ConnectionTuneMethodHandler getInstance()
    {
        return _instance;
    }

    protected ConnectionTuneMethodHandler()
    { }

    public void methodReceived(AMQProtocolSession session, ConnectionTuneBody frame, int channelId)
    {
        _logger.debug("ConnectionTune frame received");
        final MethodRegistry methodRegistry = session.getMethodRegistry();


        ConnectionTuneParameters params = session.getConnectionTuneParameters();
        if (params == null)
        {
            params = new ConnectionTuneParameters();
        }

        params.setFrameMax(frame.getFrameMax());
        params.setChannelMax(frame.getChannelMax());
        params.setHeartbeat(Integer.getInteger("amqj.heartbeat.delay", frame.getHeartbeat()));
        session.setConnectionTuneParameters(params);

        session.getStateManager().changeState(AMQState.CONNECTION_NOT_OPENED);

        ConnectionTuneOkBody tuneOkBody = methodRegistry.createConnectionTuneOkBody(params.getChannelMax(),
                                                                                    params.getFrameMax(),
                                                                                    params.getHeartbeat());
        // Be aware of possible changes to parameter order as versions change.
        session.writeFrame(tuneOkBody.generateFrame(channelId));

        String host = session.getAMQConnection().getVirtualHost();
        AMQShortString virtualHost = new AMQShortString("/" + host);

        ConnectionOpenBody openBody = methodRegistry.createConnectionOpenBody(virtualHost,null,true);

        // Be aware of possible changes to parameter order as versions change.
        session.writeFrame(openBody.generateFrame(channelId));
    }


}
