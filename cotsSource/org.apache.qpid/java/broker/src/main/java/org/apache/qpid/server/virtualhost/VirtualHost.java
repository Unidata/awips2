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
package org.apache.qpid.server.virtualhost;

import org.apache.qpid.server.connection.IConnectionRegistry;
import org.apache.qpid.server.configuration.VirtualHostConfiguration;
import org.apache.qpid.server.queue.QueueRegistry;
import org.apache.qpid.server.exchange.ExchangeRegistry;
import org.apache.qpid.server.exchange.ExchangeFactory;
import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.server.store.TransactionLog;
import org.apache.qpid.server.store.DurableConfigurationStore;
import org.apache.qpid.server.security.auth.manager.AuthenticationManager;
import org.apache.qpid.server.security.access.ACLManager;
import org.apache.qpid.server.management.ManagedObject;

public interface VirtualHost
{
    IConnectionRegistry getConnectionRegistry();

    VirtualHostConfiguration getConfiguration();

    String getName();

    QueueRegistry getQueueRegistry();

    ExchangeRegistry getExchangeRegistry();

    ExchangeFactory getExchangeFactory();

    MessageStore getMessageStore();

    TransactionLog getTransactionLog();

    DurableConfigurationStore getDurableConfigurationStore();

    AuthenticationManager getAuthenticationManager();

    ACLManager getAccessManager();

    void close() throws Exception;

    ManagedObject getManagedObject();
}
