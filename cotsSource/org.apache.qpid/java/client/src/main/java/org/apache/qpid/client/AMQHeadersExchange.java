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
package org.apache.qpid.client;

import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.url.BindingURL;

/**
 * A destination backed by a headers exchange
 */
public class AMQHeadersExchange extends AMQDestination
{
    public AMQHeadersExchange(BindingURL binding)
    {
        this(binding.getExchangeName());
    }

    public AMQHeadersExchange(String name)
    {
        this(new AMQShortString(name));
    }

    public AMQHeadersExchange(AMQShortString queueName)
    {
        super(queueName, ExchangeDefaults.HEADERS_EXCHANGE_CLASS, queueName, true, true, null);
    }

    public boolean isNameRequired()
    {
        //Not sure what the best approach is here, probably to treat this like a topic
        //and allow server to generate names. As it is AMQ specific it doesn't need to
        //fit the JMS API expectations so this is not as yet critical.
        return getAMQQueueName() == null;
    }
}
