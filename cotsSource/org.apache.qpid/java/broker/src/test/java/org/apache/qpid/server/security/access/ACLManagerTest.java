/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */
package org.apache.qpid.server.security.access;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

import junit.framework.TestCase;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.qpid.server.configuration.SecurityConfiguration;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.plugins.MockPluginManager;
import org.apache.qpid.server.plugins.PluginManager;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.protocol.InternalTestProtocolSession;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.MockAMQQueue;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.registry.ApplicationRegistry;

public class ACLManagerTest extends TestCase
{

    private ACLManager _authzManager;
    private AMQProtocolSession _session;
    private SecurityConfiguration _conf;
    private PluginManager _pluginManager;

    @Override
    public void setUp() throws Exception
    {
        File tmpFile = File.createTempFile(getClass().getName(), "testconfig");
        tmpFile.deleteOnExit();
        BufferedWriter out = new BufferedWriter(new FileWriter(tmpFile));
        out.write("<security><queueDenier>notyet</queueDenier><exchangeDenier>yes</exchangeDenier></security>");
        out.close();

        _conf = new SecurityConfiguration(new XMLConfiguration(tmpFile));

        // Create ACLManager

        _pluginManager = new MockPluginManager("");
        _authzManager = new ACLManager(_conf, _pluginManager);


        VirtualHost virtualHost = ApplicationRegistry.getInstance().
                getVirtualHostRegistry().getVirtualHosts().iterator().next();

        // Create a single session for this test.
        _session = new InternalTestProtocolSession(virtualHost);
    }

    @Override
    public void tearDown() throws Exception
    {
        // Correctly Close the AR we created
        ApplicationRegistry.remove();
        super.tearDown();
    }

    public void testACLManagerConfigurationPluginManager() throws Exception
    {
        AMQQueue queue = new MockAMQQueue("notyet");
        AMQQueue otherQueue = new MockAMQQueue("other");

        assertFalse(_authzManager.authoriseDelete(_session, queue));

        // This should only be denied if the config hasn't been correctly passed in
        assertTrue(_authzManager.authoriseDelete(_session, otherQueue));
        assertTrue(_authzManager.authorisePurge(_session, queue));
    }

    public void testACLManagerConfigurationPluginManagerACLPlugin() throws ConfigurationException
    {
        _authzManager = new ACLManager(_conf, _pluginManager, ExchangeDenier.FACTORY);

        Exchange exchange = null;
        assertFalse(_authzManager.authoriseDelete(_session, exchange));
    }

    public void testConfigurePlugins() throws ConfigurationException
    {
        Configuration hostConfig = new PropertiesConfiguration();
        hostConfig.setProperty("queueDenier", "thisoneneither");
        _authzManager.configureHostPlugins(new SecurityConfiguration(hostConfig));
        AMQQueue queue = new MockAMQQueue("thisoneneither");
        assertFalse(_authzManager.authoriseDelete(_session, queue));
    }
}
