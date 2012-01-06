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
package org.apache.qpid.server.util;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.configuration.VirtualHostConfiguration;
import org.apache.qpid.server.exchange.ExchangeFactory;
import org.apache.qpid.server.exchange.ExchangeRegistry;
import org.apache.qpid.server.management.NoopManagedObjectRegistry;
import org.apache.qpid.server.queue.QueueRegistry;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.security.access.ACLManager;
import org.apache.qpid.server.security.access.plugins.AllowAll;
import org.apache.qpid.server.security.auth.database.PropertiesPrincipalDatabaseManager;
import org.apache.qpid.server.security.auth.manager.PrincipalDatabaseAuthenticationManager;
import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.server.store.TestableMemoryMessageStore;
import org.apache.qpid.server.virtualhost.VirtualHostRegistry;
import org.apache.qpid.server.virtualhost.VirtualHostImpl;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.logging.RootMessageLoggerImpl;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.actors.TestLogActor;
import org.apache.qpid.server.logging.rawloggers.Log4jMessageLogger;

import java.util.Collection;
import java.util.Properties;
import java.util.Arrays;

public class TestApplicationRegistry extends ApplicationRegistry
{
    private QueueRegistry _queueRegistry;

    private ExchangeRegistry _exchangeRegistry;

    private ExchangeFactory _exchangeFactory;

    private MessageStore _messageStore;

    private VirtualHost _vHost;


    private ServerConfiguration _config;

    public TestApplicationRegistry() throws ConfigurationException
    {
    	super(new ServerConfiguration(new PropertiesConfiguration()));
    }

    public TestApplicationRegistry(ServerConfiguration config) throws ConfigurationException
    {
    	super(config);
    	_config = config;
    }

    public void initialise(int instanceID) throws Exception
    {
        _rootMessageLogger = new RootMessageLoggerImpl(_configuration,
                                                       new Log4jMessageLogger());

        //Add a Test Actor as a lot of our System Tests reach in to the broker
        // and manipulate it so the CurrentActor is not set.
        CurrentActor.set(new TestLogActor(_rootMessageLogger));

        Properties users = new Properties();

        users.put("guest", "guest");

        _databaseManager = new PropertiesPrincipalDatabaseManager("default", users);

        _accessManager = new ACLManager(_configuration.getSecurityConfiguration(), _pluginManager, AllowAll.FACTORY);

        _authenticationManager = new PrincipalDatabaseAuthenticationManager(null, null);

        _managedObjectRegistry = new NoopManagedObjectRegistry();

        _messageStore = new TestableMemoryMessageStore();

        _virtualHostRegistry = new VirtualHostRegistry(this);

        PropertiesConfiguration vhostProps = new PropertiesConfiguration();
        VirtualHostConfiguration hostConfig = new VirtualHostConfiguration("test", vhostProps);
        _vHost = new VirtualHostImpl(hostConfig, _messageStore);

        _virtualHostRegistry.registerVirtualHost(_vHost);

        _queueRegistry = _vHost.getQueueRegistry();
        _exchangeFactory = _vHost.getExchangeFactory();
        _exchangeRegistry = _vHost.getExchangeRegistry();

    }

    public QueueRegistry getQueueRegistry()
    {
        return _queueRegistry;
    }

    public ExchangeRegistry getExchangeRegistry()
    {
        return _exchangeRegistry;
    }

    public ExchangeFactory getExchangeFactory()
    {
        return _exchangeFactory;
    }

    public Collection<String> getVirtualHostNames()
    {
        String[] hosts = {"test"};
        return Arrays.asList(hosts);
    }

    public void setAccessManager(ACLManager newManager)
    {
        _accessManager = newManager;
    }

    public MessageStore getMessageStore()
    {
        return _messageStore;
    }

    @Override
    public void close() throws Exception
    {
        try
        {
            super.close();
        }
        finally
        {
            CurrentActor.remove();
        }
    }

}


