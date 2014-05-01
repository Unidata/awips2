/*
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
 */
package org.apache.qpid.server.plugins;

import java.util.HashMap;
import java.util.Map;

import org.apache.qpid.server.exchange.ExchangeType;
import org.apache.qpid.server.security.access.ACLPlugin;
import org.apache.qpid.server.security.access.ACLPluginFactory;
import org.apache.qpid.server.security.access.QueueDenier;

public class MockPluginManager extends PluginManager
{

    private Map<String, ACLPluginFactory> _securityPlugins = new HashMap<String, ACLPluginFactory>();

    public MockPluginManager(String plugindir) throws Exception
    {
        super(plugindir);
        _securityPlugins.put("org.apache.qpid.server.security.access.QueueDenier", QueueDenier.FACTORY);
    }

    @Override
    public Map<String, ExchangeType<?>> getExchanges()
    {
       return null;
    }

    @Override
    public Map<String, ACLPluginFactory> getSecurityPlugins()
    {
        return _securityPlugins;
    }
}
