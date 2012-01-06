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
import org.apache.qpid.AMQInvalidRoutingKeyException;
import org.apache.qpid.framing.*;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.exchange.ExchangeRegistry;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.QueueRegistry;
import org.apache.qpid.server.security.access.Permission;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.server.state.StateAwareMethodListener;
import org.apache.qpid.server.virtualhost.VirtualHost;

public class QueueBindHandler implements StateAwareMethodListener<QueueBindBody>
{
    private static final Logger _log = Logger.getLogger(QueueBindHandler.class);

    private static final QueueBindHandler _instance = new QueueBindHandler();

    public static QueueBindHandler getInstance()
    {
        return _instance;
    }

    private QueueBindHandler()
    {
    }

    public void methodReceived(AMQStateManager stateManager, QueueBindBody body, int channelId) throws AMQException
    {
        AMQProtocolSession session = stateManager.getProtocolSession();
        VirtualHost virtualHost = session.getVirtualHost();
        ExchangeRegistry exchangeRegistry = virtualHost.getExchangeRegistry();
        QueueRegistry queueRegistry = virtualHost.getQueueRegistry();


        final AMQQueue queue;
        final AMQShortString routingKey;

        if (body.getQueue() == null)
        {
            AMQChannel channel = session.getChannel(channelId);

            if (channel == null)
            {
                throw body.getChannelNotFoundException(channelId);
            }

            queue = channel.getDefaultQueue();

            if (queue == null)
            {
                throw body.getChannelException(AMQConstant.NOT_FOUND, "No default queue defined on channel and queue was null");
            }

            if (body.getRoutingKey() == null)
            {
                routingKey = queue.getName();
            }
            else
            {
                routingKey = body.getRoutingKey().intern();
            }
        }
        else
        {
            queue = queueRegistry.getQueue(body.getQueue());
            routingKey = body.getRoutingKey() == null ? AMQShortString.EMPTY_STRING : body.getRoutingKey().intern();
        }

        if (queue == null)
        {
            throw body.getChannelException(AMQConstant.NOT_FOUND, "Queue " + body.getQueue() + " does not exist.");
        }
        final Exchange exch = exchangeRegistry.getExchange(body.getExchange());
        if (exch == null)
        {
            throw body.getChannelException(AMQConstant.NOT_FOUND, "Exchange " + body.getExchange() + " does not exist.");
        }


        try
        {

            //Perform ACLs
            if (!virtualHost.getAccessManager().authoriseBind(session, exch,
                    queue, routingKey))
            {
                throw body.getConnectionException(AMQConstant.ACCESS_REFUSED, "Permission denied");
            }
            else if (queue.isExclusive() && !queue.isDurable() && queue.getExclusiveOwner() != session)
            {
                throw body.getConnectionException(AMQConstant.NOT_ALLOWED,
                                                  "Queue " + queue.getName() + " is exclusive, but not created on this Connection.");
            }

            if (!exch.isBound(routingKey, body.getArguments(), queue))
            {
                queue.bind(exch, routingKey, body.getArguments());
            }
        }
        catch (AMQInvalidRoutingKeyException rke)
        {
            throw body.getChannelException(AMQConstant.INVALID_ROUTING_KEY, routingKey.toString());
        }
        catch (AMQException e)
        {
            throw body.getChannelException(AMQConstant.CHANNEL_ERROR, e.toString());
        }

        if (_log.isInfoEnabled())
        {
            _log.info("Binding queue " + queue + " to exchange " + exch + " with routing key " + routingKey);
        }
        if (!body.getNowait())
        {
            MethodRegistry methodRegistry = session.getMethodRegistry();
            AMQMethodBody responseBody = methodRegistry.createQueueBindOkBody();
            session.writeFrame(responseBody.generateFrame(channelId));

        }
    }
}
