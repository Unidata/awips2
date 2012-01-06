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
package org.apache.qpid.server.registry;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.qpid.AMQException;
import org.apache.qpid.common.QpidProperties;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.logging.RootMessageLoggerImpl;
import org.apache.qpid.server.logging.messages.BrokerMessages;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.actors.BrokerActor;
import org.apache.qpid.server.logging.rawloggers.Log4jMessageLogger;
import org.apache.qpid.server.management.JMXManagedObjectRegistry;
import org.apache.qpid.server.management.NoopManagedObjectRegistry;
import org.apache.qpid.server.plugins.PluginManager;
import org.apache.qpid.server.security.access.ACLManager;
import org.apache.qpid.server.security.auth.database.ConfigurationFilePrincipalDatabaseManager;
import org.apache.qpid.server.security.auth.manager.PrincipalDatabaseAuthenticationManager;
import org.apache.qpid.server.virtualhost.VirtualHostRegistry;
import org.apache.qpid.server.virtualhost.VirtualHostImpl;

import java.io.File;

public class ConfigurationFileApplicationRegistry extends ApplicationRegistry
{
    private String _registryName;

    public ConfigurationFileApplicationRegistry(File configurationURL) throws ConfigurationException
    {
        super(new ServerConfiguration(configurationURL));
    }

    public void initialise(int instanceID) throws Exception
    {
        _rootMessageLogger = new RootMessageLoggerImpl(_configuration,
                                                       new Log4jMessageLogger());

        _registryName = String.valueOf(instanceID);

        // Set the Actor for current log messages
        CurrentActor.set(new BrokerActor(_registryName, _rootMessageLogger));

        CurrentActor.get().message(BrokerMessages.BRK_STARTUP(QpidProperties.getReleaseVersion(),QpidProperties.getBuildVersion()));

        initialiseManagedObjectRegistry();

        _virtualHostRegistry = new VirtualHostRegistry(this);

        _pluginManager = new PluginManager(_configuration.getPluginDirectory());

        _accessManager = new ACLManager(_configuration.getSecurityConfiguration(), _pluginManager);

        _databaseManager = new ConfigurationFilePrincipalDatabaseManager(_configuration);

        _authenticationManager = new PrincipalDatabaseAuthenticationManager(null, null);

        _databaseManager.initialiseManagement(_configuration);

        _managedObjectRegistry.start();

        initialiseVirtualHosts();

        // Startup complete pop the current actor
        CurrentActor.remove();
    }

    @Override
    public void close() throws Exception
    {
        //Set the Actor for Broker Shutdown
        CurrentActor.set(new BrokerActor(_registryName, _rootMessageLogger));
        try
        {
            super.close();
        }
        finally
        {
            CurrentActor.remove();
        }
    }

    private void initialiseVirtualHosts() throws Exception
    {
        for (String name : _configuration.getVirtualHosts())
        {
            _virtualHostRegistry.registerVirtualHost(new VirtualHostImpl(_configuration.getVirtualHostConfig(name)));
        }
        getVirtualHostRegistry().setDefaultVirtualHostName(_configuration.getDefaultVirtualHost());
    }

    private void initialiseManagedObjectRegistry() throws AMQException
    {
        if (_configuration.getManagementEnabled())
        {
            _managedObjectRegistry = new JMXManagedObjectRegistry();
        }
        else
        {
            _managedObjectRegistry = new NoopManagedObjectRegistry();
        }
    }
}
