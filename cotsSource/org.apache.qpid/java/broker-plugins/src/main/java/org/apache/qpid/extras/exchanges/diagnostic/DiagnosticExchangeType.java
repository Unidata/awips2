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

package org.apache.qpid.extras.exchanges.diagnostic;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.exchange.ExchangeType;
import org.apache.qpid.server.virtualhost.VirtualHost;

/**
 * Exchange type class for getting hold of the exchange.
 */
public final class DiagnosticExchangeType implements ExchangeType<DiagnosticExchange>
{

    public AMQShortString getName()
    {
        return DiagnosticExchange.DIAGNOSTIC_EXCHANGE_CLASS;
    }

    public Class<DiagnosticExchange> getExchangeClass()
    {
        return DiagnosticExchange.class;
    }

    public DiagnosticExchange newInstance(VirtualHost host, AMQShortString name, boolean durable, int ticket, boolean autoDelete)
            throws AMQException
    {
        DiagnosticExchange exch = new DiagnosticExchange();
        exch.initialise(host, name, durable, ticket, autoDelete);
        return exch;
    }

    public AMQShortString getDefaultExchangeName()
    {
        return DiagnosticExchange.DIAGNOSTIC_EXCHANGE_NAME;
    }
}
