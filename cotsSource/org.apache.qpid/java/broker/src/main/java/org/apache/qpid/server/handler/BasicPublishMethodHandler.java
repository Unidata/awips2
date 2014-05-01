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
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.BasicPublishBody;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.security.access.Permission;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.server.state.StateAwareMethodListener;
import org.apache.qpid.server.virtualhost.VirtualHost;

public class BasicPublishMethodHandler implements StateAwareMethodListener<BasicPublishBody>
{
    private static final Logger _logger = Logger.getLogger(BasicPublishMethodHandler.class);

    private static final BasicPublishMethodHandler _instance = new BasicPublishMethodHandler();


    public static BasicPublishMethodHandler getInstance()
    {
        return _instance;
    }

    private BasicPublishMethodHandler()
    {
    }

    public void methodReceived(AMQStateManager stateManager, BasicPublishBody body, int channelId) throws AMQException
    {
        AMQProtocolSession session = stateManager.getProtocolSession();
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Publish received on channel " + channelId);
        }

        AMQShortString exchange = body.getExchange();
        // TODO: check the delivery tag field details - is it unique across the broker or per subscriber?
        if (exchange == null)
        {
            exchange = ExchangeDefaults.DEFAULT_EXCHANGE_NAME;

        }

        VirtualHost vHost = session.getVirtualHost();
        Exchange e = vHost.getExchangeRegistry().getExchange(exchange);
        // if the exchange does not exist we raise a channel exception
        if (e == null)
        {
            throw body.getChannelException(AMQConstant.NOT_FOUND, "Unknown exchange name");
        }
        else
        {
            // The partially populated BasicDeliver frame plus the received route body
            // is stored in the channel. Once the final body frame has been received
            // it is routed to the exchange.
            AMQChannel channel = session.getChannel(channelId);

            if (channel == null)
            {
                throw body.getChannelNotFoundException(channelId);
            }

            //Access Control
            if (!vHost.getAccessManager().authorisePublish(session, 
                    body.getImmediate(), body.getMandatory(), 
                    body.getRoutingKey(), e))
            {
                throw body.getConnectionException(AMQConstant.ACCESS_REFUSED, "Permission denied");
            }

            MessagePublishInfo info = session.getMethodRegistry().getProtocolVersionMethodConverter().convertToInfo(body);
            info.setExchange(exchange);
            channel.setPublishFrame(info, e);
        }
    }

}



