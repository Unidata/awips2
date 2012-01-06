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
package org.apache.qpid.server.exchange;

import org.apache.qpid.AMQException;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.store.DurableConfigurationStore;

public class ExchangeInitialiser
{
    public void initialise(ExchangeFactory factory, ExchangeRegistry registry, DurableConfigurationStore store) throws AMQException{
        for (ExchangeType<? extends Exchange> type : factory.getRegisteredTypes())
        {
            define (registry, factory, type.getDefaultExchangeName(), type.getName(), store);
        }

        define(registry, factory, ExchangeDefaults.DEFAULT_EXCHANGE_NAME, ExchangeDefaults.DIRECT_EXCHANGE_CLASS, store);
        registry.setDefaultExchange(registry.getExchange(ExchangeDefaults.DEFAULT_EXCHANGE_NAME));
    }

    private void define(ExchangeRegistry r, ExchangeFactory f,
                        AMQShortString name, AMQShortString type, DurableConfigurationStore store) throws AMQException
    {
        if(r.getExchange(name)== null)
        {
            Exchange exchange = f.createExchange(name, type, true, false, 0);
            r.registerExchange(exchange);

            if(exchange.isDurable())
            {
                store.createExchange(exchange);
            }
        }
    }
}
