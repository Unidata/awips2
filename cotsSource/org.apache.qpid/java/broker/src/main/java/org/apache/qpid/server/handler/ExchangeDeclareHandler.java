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
import org.apache.qpid.AMQConnectionException;
import org.apache.qpid.AMQException;
import org.apache.qpid.AMQUnknownExchangeType;
import org.apache.qpid.framing.*;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.exchange.ExchangeFactory;
import org.apache.qpid.server.exchange.ExchangeRegistry;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.security.access.Permission;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.server.state.StateAwareMethodListener;
import org.apache.qpid.server.virtualhost.VirtualHost;

public class ExchangeDeclareHandler implements StateAwareMethodListener<ExchangeDeclareBody>
{
    private static final Logger _logger = Logger.getLogger(ExchangeDeclareHandler.class);

    private static final ExchangeDeclareHandler _instance = new ExchangeDeclareHandler();

    public static ExchangeDeclareHandler getInstance()
    {
        return _instance;
    }



    private ExchangeDeclareHandler()
    {
    }

    public void methodReceived(AMQStateManager stateManager, ExchangeDeclareBody body, int channelId) throws AMQException
    {
        AMQProtocolSession session = stateManager.getProtocolSession();
        VirtualHost virtualHost = session.getVirtualHost();
        ExchangeRegistry exchangeRegistry = virtualHost.getExchangeRegistry();
        ExchangeFactory exchangeFactory = virtualHost.getExchangeFactory();

        if (!body.getPassive())
        {
            // Perform ACL if request is not passive
            if (!virtualHost.getAccessManager().authoriseCreateExchange(session, body.getAutoDelete(),
                    body.getDurable(), body.getExchange(), body.getInternal(), body.getNowait(), body.getPassive(),
                    body.getType()))
            {
                throw body.getConnectionException(AMQConstant.ACCESS_REFUSED, "Permission denied");
            }

        }

        if (_logger.isDebugEnabled())
        {
            _logger.debug("Request to declare exchange of type " + body.getType() + " with name " + body.getExchange());
        }
        synchronized(exchangeRegistry)
        {
            Exchange exchange = exchangeRegistry.getExchange(body.getExchange());



            if (exchange == null)
            {
                if(body.getPassive() && ((body.getType() == null) || body.getType().length() ==0))
                {
                    throw body.getChannelException(AMQConstant.NOT_FOUND, "Unknown exchange: " + body.getExchange());
                }
                else
                {
                    try
                    {

                        exchange = exchangeFactory.createExchange(body.getExchange() == null ? null : body.getExchange().intern(),
                                                                  body.getType() == null ? null : body.getType().intern(),
                                                                  body.getDurable(),
                                                                  body.getPassive(), body.getTicket());
                        exchangeRegistry.registerExchange(exchange);

                        if (exchange.isDurable())
                        {
                            virtualHost.getDurableConfigurationStore().createExchange(exchange);
                        }
                    }
                    catch(AMQUnknownExchangeType e)
                    {
                        throw body.getConnectionException(AMQConstant.COMMAND_INVALID, "Unknown exchange: " + body.getExchange(),e);
                    }
                }
            }
            else if (!exchange.getType().equals(body.getType()))
            {

                throw new AMQConnectionException(AMQConstant.NOT_ALLOWED, "Attempt to redeclare exchange: " + body.getExchange() + " of type " + exchange.getType() + " to " + body.getType() +".",body.getClazz(), body.getMethod(),body.getMajor(),body.getMinor(),null);
            }

        }
        if(!body.getNowait())
        {
            MethodRegistry methodRegistry = session.getMethodRegistry();
            AMQMethodBody responseBody = methodRegistry.createExchangeDeclareOkBody();
            session.writeFrame(responseBody.generateFrame(channelId));

        }
    }
}
