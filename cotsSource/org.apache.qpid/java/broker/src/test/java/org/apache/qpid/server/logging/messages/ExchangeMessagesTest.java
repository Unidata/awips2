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
package org.apache.qpid.server.logging.messages;

import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.registry.ApplicationRegistry;

import java.util.List;

/**
 * Test EXH Log Messages
 */
public class ExchangeMessagesTest extends AbstractTestMessages
{
    public void testExchangeCreated_Transient()
    {
        // Get the Default Exchange on the Test Vhost for testing
        Exchange exchange = ApplicationRegistry.getInstance().
                getVirtualHostRegistry().getVirtualHost("test").
                getExchangeRegistry().getDefaultExchange();

        String type = exchange.getType().toString();
        String name = exchange.getName().toString();

        _logMessage = ExchangeMessages.EXH_CREATED(type, name, false);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Type:", type, "Name:", name};

        validateLogMessage(log, "EXH-1001", expected);
    }

    public void testExchangeCreated_Persistent()
    {
        // Get the Default Exchange on the Test Vhost for testing
        Exchange exchange = ApplicationRegistry.getInstance().
                getVirtualHostRegistry().getVirtualHost("test").
                getExchangeRegistry().getDefaultExchange();

        String type = exchange.getType().toString();
        String name = exchange.getName().toString();

        _logMessage = ExchangeMessages.EXH_CREATED(type, name, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Durable", "Type:", type, "Name:", name};

        validateLogMessage(log, "EXH-1001", expected);
    }


    public void testExchangeDeleted()
    {
        _logMessage = ExchangeMessages.EXH_DELETED();
        List<Object> log = performLog();

        String[] expected = {"Deleted"};

        validateLogMessage(log, "EXH-1002", expected);
    }

}
