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

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.ExchangeDeleteBody;
import org.apache.qpid.framing.ExchangeDeleteOkBody;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.exchange.ExchangeInUseException;
import org.apache.qpid.server.exchange.ExchangeRegistry;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.security.access.Permission;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.server.state.StateAwareMethodListener;
import org.apache.qpid.server.virtualhost.VirtualHost;

public class ExchangeDeleteHandler implements StateAwareMethodListener<ExchangeDeleteBody>
{
    private static final ExchangeDeleteHandler _instance = new ExchangeDeleteHandler();

    public static ExchangeDeleteHandler getInstance()
    {
        return _instance;
    }

    private ExchangeDeleteHandler()
    {
    }

    public void methodReceived(AMQStateManager stateManager, ExchangeDeleteBody body, int channelId) throws AMQException
    {
        AMQProtocolSession session = stateManager.getProtocolSession();
        VirtualHost virtualHost = session.getVirtualHost();
        ExchangeRegistry exchangeRegistry = virtualHost.getExchangeRegistry();

        //Perform ACLs
        if (!virtualHost.getAccessManager().authoriseDelete(session,
                exchangeRegistry.getExchange(body.getExchange())))
        {
            throw body.getConnectionException(AMQConstant.ACCESS_REFUSED, "Permission denied");
        }

        try
        {
            if(exchangeRegistry.getExchange(body.getExchange()) == null)
            {
                throw body.getChannelException(AMQConstant.NOT_FOUND, "No such exchange: " + body.getExchange());
            }
            exchangeRegistry.unregisterExchange(body.getExchange(), body.getIfUnused());

            ExchangeDeleteOkBody responseBody = session.getMethodRegistry().createExchangeDeleteOkBody();
                        
            session.writeFrame(responseBody.generateFrame(channelId));
        }
        catch (ExchangeInUseException e)
        {
            throw body.getChannelException(AMQConstant.IN_USE, "Exchange in use");
            // TODO: sort out consistent channel close mechanism that does all clean up etc.
        }

    }
}
