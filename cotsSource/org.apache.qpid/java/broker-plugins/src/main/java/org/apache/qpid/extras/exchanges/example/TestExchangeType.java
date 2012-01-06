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

package org.apache.qpid.extras.exchanges.example;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.exchange.ExchangeType;
import org.apache.qpid.server.virtualhost.VirtualHost;

public class TestExchangeType implements ExchangeType
{

    public Class getExchangeClass()
    {
        return TestExchange.class;
    }

    public AMQShortString getName()
    {
        return null;
    }

    public Exchange newInstance(VirtualHost host, AMQShortString name, boolean durable,
                                int token, boolean autoDelete)
            throws AMQException
    {
        TestExchange ex = new TestExchange();
        ex.initialise(host, name, durable, token, autoDelete);
        return ex;
    }

    public AMQShortString getDefaultExchangeName()
    {
        return new AMQShortString("test.exchange");
    }

}
