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
package org.apache.qpid.framing.abstraction;

import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.framing.AMQShortString;

public class MessagePublishInfoImpl implements MessagePublishInfo
{
    private AMQShortString _exchange;
    private boolean _immediate;
    private boolean _mandatory;
    private AMQShortString _routingKey;

    public MessagePublishInfoImpl()
    {
    }

    public MessagePublishInfoImpl(AMQShortString exchange, boolean immediate, boolean mandatory,
                                  AMQShortString routingKey)
    {
        _exchange = exchange;
        _immediate = immediate;
        _mandatory = mandatory;
        _routingKey = routingKey;
    }

    public AMQShortString getExchange()
    {
        return _exchange;
    }

    public void setExchange(AMQShortString exchange)
    {
        _exchange = exchange;
    }

    public boolean isImmediate()
    {
        return _immediate;
    }

    public void setImmediate(boolean immedate)
    {
        _immediate = immedate;
    }

    public boolean isMandatory()
    {
        return _mandatory;
    }

    public void setMandatory(boolean mandatory)
    {
        _mandatory = mandatory;        
    }

    public AMQShortString getRoutingKey()
    {
        return _routingKey;
    }

    public void setRoutingKey(AMQShortString routingKey)
    {
        _routingKey = routingKey;
    }
}
