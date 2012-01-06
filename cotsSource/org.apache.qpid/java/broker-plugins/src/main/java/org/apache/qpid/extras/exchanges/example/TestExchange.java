package org.apache.qpid.extras.exchanges.example;
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


import java.util.List;
import java.util.Map;
import java.util.ArrayList;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.exchange.ExchangeReferrer;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.message.InboundMessage;

public class TestExchange implements Exchange
{

    public void close() throws AMQException
    {
    }

    public void deregisterQueue(AMQShortString routingKey, AMQQueue queue, FieldTable args) throws AMQException
    {
    }

    public Map<AMQShortString, List<AMQQueue>> getBindings()
    {
        return null;
    }

    public AMQShortString getName()
    {
        return null;
    }

    public AMQShortString getType()
    {
        return null;
    }

    public boolean hasBindings()
    {
        return false;
    }

    public boolean isBound(String bindingKey, AMQQueue queue)
    {
        return false;
    }

    public boolean isBound(String bindingKey)
    {
        return false;
    }

    public Exchange getAlternateExchange()
    {
        return null;
    }

    public void setAlternateExchange(Exchange exchange)
    {

    }

    public void removeReference(ExchangeReferrer exchange)
    {

    }

    public void addReference(ExchangeReferrer exchange)
    {

    }

    public boolean hasReferrers()
    {
        return false;
    }

    public void initialise(VirtualHost host, AMQShortString name, boolean durable, boolean autoDelete)
            throws AMQException
    {
    }

    public boolean isAutoDelete()
    {
        return false;
    }

    public boolean isBound(AMQShortString routingKey, FieldTable arguments, AMQQueue queue)
    {
        return false;
    }

    public boolean isBound(AMQShortString routingKey, AMQQueue queue)
    {
        return false;
    }

    public boolean isBound(AMQShortString routingKey)
    {
        return false;
    }

    public boolean isBound(AMQQueue queue)
    {
        return false;
    }

    public boolean isDurable()
    {
        return false;
    }

    public void registerQueue(AMQShortString routingKey, AMQQueue queue, FieldTable args) throws AMQException
    {
    }

    public ArrayList<AMQQueue> route(InboundMessage message)
    {
        return new ArrayList<AMQQueue>();
    }

    public int getTicket()
    {
        return 0;
    }

    public void initialise(VirtualHost arg0, AMQShortString arg1, boolean arg2, int arg3, boolean arg4)
            throws AMQException
    {
    }
}
