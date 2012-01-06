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
package org.apache.qpid.server.logging.actors;

import junit.framework.TestCase;
import org.apache.qpid.AMQException;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.logging.rawloggers.UnitTestMessageLogger;
import org.apache.qpid.server.logging.RootMessageLogger;
import org.apache.qpid.server.logging.RootMessageLoggerImpl;
import org.apache.qpid.server.logging.LogActor;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.PropertiesConfiguration;

public class BaseActorTestCase extends TestCase
{
    protected LogActor _amqpActor;
    protected UnitTestMessageLogger _rawLogger;
    protected RootMessageLogger _rootLogger;

    public void setUp() throws Exception
    {
        super.setUp();
        //Highlight that this test will cause a new AR to be created
        ApplicationRegistry.getInstance();

        Configuration config = new PropertiesConfiguration();
        ServerConfiguration serverConfig = new ServerConfiguration(config);

        serverConfig.getConfig().setProperty(ServerConfiguration.STATUS_UPDATES, "on");

        setUpWithConfig(serverConfig);
    }

    public void tearDown() throws Exception
    {
        _rawLogger.clearLogMessages();

        // Correctly Close the AR we created
        ApplicationRegistry.remove();

        super.tearDown();
    }

    protected void setUpWithConfig(ServerConfiguration serverConfig) throws AMQException
    {
        _rawLogger = new UnitTestMessageLogger();

        _rootLogger =
                new RootMessageLoggerImpl(serverConfig, _rawLogger);
    }


}
