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

import java.util.Collection;
import java.net.InetSocketAddress;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.management.ManagedObjectRegistry;
import org.apache.qpid.server.plugins.PluginManager;
import org.apache.qpid.server.security.auth.manager.AuthenticationManager;
import org.apache.qpid.server.security.auth.database.PrincipalDatabaseManager;
import org.apache.qpid.server.security.access.ACLManager;
import org.apache.qpid.server.security.access.ACLPlugin;
import org.apache.qpid.server.virtualhost.VirtualHostRegistry;
import org.apache.qpid.server.logging.RootMessageLogger;
import org.apache.qpid.server.transport.QpidAcceptor;
import org.apache.mina.common.IoAcceptor;

public interface IApplicationRegistry
{
    /**
     * Initialise the application registry. All initialisation must be done in this method so that any components
     * that need access to the application registry itself for initialisation are able to use it. Attempting to
     * initialise in the constructor will lead to failures since the registry reference will not have been set.
     * @param instanceID the instanceID that we can use to identify this AR.
     */
    void initialise(int instanceID) throws Exception;

    /**
     * Shutdown this Registry
     * @throws Exception - //fixme needs to be made more specific
     */
    void close() throws Exception;

    /**
     * Get the low level configuration. For use cases where the configured object approach is not required
     * you can get the complete configuration information.
     * @return a Commons Configuration instance
     */
    ServerConfiguration getConfiguration();

    ManagedObjectRegistry getManagedObjectRegistry();

    PrincipalDatabaseManager getDatabaseManager();

    AuthenticationManager getAuthenticationManager();

    VirtualHostRegistry getVirtualHostRegistry();

    ACLManager getAccessManager() throws ConfigurationException;

    PluginManager getPluginManager();

    RootMessageLogger getRootMessageLogger();

    /**
     * Register any acceptors for this registry
     * @param bindAddress The address that the acceptor has been bound with
     * @param acceptor The acceptor in use
     */
    void addAcceptor(InetSocketAddress bindAddress, QpidAcceptor acceptor);

}
