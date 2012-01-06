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

import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.log4j.Logger;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.management.ManagedObjectRegistry;
import org.apache.qpid.server.plugins.PluginManager;
import org.apache.qpid.server.security.access.ACLManager;
import org.apache.qpid.server.security.auth.database.PrincipalDatabaseManager;
import org.apache.qpid.server.security.auth.manager.AuthenticationManager;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.virtualhost.VirtualHostRegistry;
import org.apache.qpid.server.logging.RootMessageLogger;
import org.apache.qpid.server.logging.messages.BrokerMessages;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.transport.QpidAcceptor;

/**
 * An abstract application registry that provides access to configuration information and handles the
 * construction and caching of configurable objects.
 * <p/>
 * Subclasses should handle the construction of the "registered objects" such as the exchange registry.
 */
public abstract class ApplicationRegistry implements IApplicationRegistry
{
    protected static final Logger _logger = Logger.getLogger(ApplicationRegistry.class);

    private static Map<Integer, IApplicationRegistry> _instanceMap = new HashMap<Integer, IApplicationRegistry>();

    private final Map<Class<?>, Object> _configuredObjects = new HashMap<Class<?>, Object>();

    protected final ServerConfiguration _configuration;

    public static final int DEFAULT_INSTANCE = 1;
    public static final String DEFAULT_APPLICATION_REGISTRY = "org.apache.qpid.server.util.NullApplicationRegistry";
    public static String _APPLICATION_REGISTRY = DEFAULT_APPLICATION_REGISTRY;

    protected final Map<InetSocketAddress, QpidAcceptor> _acceptors = new HashMap<InetSocketAddress, QpidAcceptor>();

    protected ManagedObjectRegistry _managedObjectRegistry;

    protected AuthenticationManager _authenticationManager;

    protected VirtualHostRegistry _virtualHostRegistry;

    protected ACLManager _accessManager;

    protected PrincipalDatabaseManager _databaseManager;

    protected PluginManager _pluginManager;

    protected RootMessageLogger _rootMessageLogger;

    static
    {
        Runtime.getRuntime().addShutdownHook(new Thread(new ShutdownService()));
    }

    private static class ShutdownService implements Runnable
    {
        public void run()
        {
            removeAll();
        }
    }

    public static void initialise(IApplicationRegistry instance) throws Exception
    {
        initialise(instance, DEFAULT_INSTANCE);
    }

    public static void initialise(IApplicationRegistry instance, int instanceID) throws Exception
    {
        if (instance != null)
        {
            _logger.info("Initialising Application Registry:" + instanceID);
            _instanceMap.put(instanceID, instance);

            try
            {
                instance.initialise(instanceID);
            }
            catch (Exception e)
            {
                _instanceMap.remove(instanceID);
                throw e;
            }
        }
        else
        {
            remove(instanceID);
        }
    }

    public static boolean isConfigured()
    {
        return isConfigured(DEFAULT_INSTANCE);
    }

    public static boolean isConfigured(int instanceID)
    {
        return _instanceMap.containsKey(instanceID);
    }

    /**
     * Method to cleanly shutdown the default registry running in this JVM
     */
    public static void remove()
    {
        remove(DEFAULT_INSTANCE);
    }

    /**
     * Method to cleanly shutdown specified registry running in this JVM
     *
     * @param instanceID the instance to shutdown
     */
    public static void remove(int instanceID)
    {
        try
        {
            IApplicationRegistry instance = _instanceMap.get(instanceID);
            if (instance != null)
            {
                if (_logger.isInfoEnabled())
                {
                    _logger.info("Shuting down ApplicationRegistry(" + instanceID + "):" + instance);
                }
                instance.close();
            }
        }
        catch (Exception e)
        {
            _logger.error("Error shutting down Application Registry(" + instanceID + "): " + e, e);
        }
        finally
        {
            _instanceMap.remove(instanceID);
        }
    }

    /** Method to cleanly shutdown all registries currently running in this JVM */
    public static void removeAll()
    {
        Object[] keys = _instanceMap.keySet().toArray();
        for (Object k : keys)
        {
            remove((Integer) k);
        }
    }

    protected ApplicationRegistry(ServerConfiguration configuration)
    {
        _configuration = configuration;
    }

    public static IApplicationRegistry getInstance()
    {
        return getInstance(DEFAULT_INSTANCE);
    }

    public static IApplicationRegistry getInstance(int instanceID)
    {
        synchronized (IApplicationRegistry.class)
        {
            IApplicationRegistry instance = _instanceMap.get(instanceID);

            if (instance == null)
            {
                try
                {
                    _logger.info("Creating DEFAULT_APPLICATION_REGISTRY: " + _APPLICATION_REGISTRY + " : Instance:" + instanceID);
                    IApplicationRegistry registry = (IApplicationRegistry) Class.forName(_APPLICATION_REGISTRY).getConstructor((Class[]) null).newInstance((Object[]) null);
                    ApplicationRegistry.initialise(registry, instanceID);
                    _logger.info("Initialised Application Registry:" + instanceID);
                    return registry;
                }
                catch (Exception e)
                {
                    _logger.error("Error configuring application: " + e, e);
                    //throw new AMQBrokerCreationException(instanceID, "Unable to create Application Registry instance " + instanceID);
                    throw new RuntimeException("Unable to create Application Registry", e);
                }
            }
            else
            {
                return instance;
            }
        }
    }

    public void close() throws Exception
    {
        if (_logger.isInfoEnabled())
        {
            _logger.info("Shutting down ApplicationRegistry:"+this);
        }

        //Stop incomming connections
        unbind();

        //Shutdown virtualhosts
        for (VirtualHost virtualHost : getVirtualHostRegistry().getVirtualHosts())
        {
            virtualHost.close();
        }

        // Replace above with this
//        _virtualHostRegistry.close();

//        _accessManager.close();

//        _databaseManager.close();

        _authenticationManager.close();

//        _databaseManager.close();

        // close the rmi registry(if any) started for management
        if (_managedObjectRegistry != null)
        {
            _managedObjectRegistry.close();
        }

//        _pluginManager.close();

        CurrentActor.get().message(BrokerMessages.BRK_STOPPED());
    }

    private void unbind()
    {
        synchronized (_acceptors)
        {
            for (InetSocketAddress bindAddress : _acceptors.keySet())
            {
                QpidAcceptor acceptor = _acceptors.get(bindAddress);
                acceptor.getNetworkDriver().close();
                CurrentActor.get().message(BrokerMessages.BRK_SHUTTING_DOWN(acceptor.toString(), bindAddress.getPort()));
            }
        }
    }

    public ServerConfiguration getConfiguration()
    {
        return _configuration;
    }

    public void addAcceptor(InetSocketAddress bindAddress, QpidAcceptor acceptor)
    {
        synchronized (_acceptors)
        {
            _acceptors.put(bindAddress, acceptor);
        }
    }

    public static void setDefaultApplicationRegistry(String clazz)
    {
        _APPLICATION_REGISTRY = clazz;
    }

    public VirtualHostRegistry getVirtualHostRegistry()
    {
        return _virtualHostRegistry;
    }

    public ACLManager getAccessManager() throws ConfigurationException
    {
        return new ACLManager(_configuration.getSecurityConfiguration(), _pluginManager);
    }

    public ManagedObjectRegistry getManagedObjectRegistry()
    {
        return _managedObjectRegistry;
    }

    public PrincipalDatabaseManager getDatabaseManager()
    {
        return _databaseManager;
    }

    public AuthenticationManager getAuthenticationManager()
    {
        return _authenticationManager;
    }

    public PluginManager getPluginManager()
    {
        return _pluginManager;
    }

    public RootMessageLogger getRootMessageLogger()
    {
        return _rootMessageLogger;
    }
    
}
