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
package org.apache.qpid.server.logging;

import junit.framework.TestCase;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.logging.rawloggers.UnitTestMessageLogger;

import java.util.List;

public class RootMessageLoggerImplTest extends TestCase
{

    RootMessageLogger _rootLogger;
    UnitTestMessageLogger _rawLogger;

    public void setUp() throws ConfigurationException
    {
        Configuration config = new PropertiesConfiguration();
        ServerConfiguration serverConfig = new ServerConfiguration(config);

        _rawLogger = new UnitTestMessageLogger();

        _rootLogger = new RootMessageLoggerImpl(serverConfig, _rawLogger);
    }

    public void tearDown()
    {
        _rawLogger.clearLogMessages();
    }

    public void testLog()
    {
        String message = "test logging";

        _rootLogger.rawMessage(message);

        List<Object> logs = _rawLogger.getLogMessages();

        assertEquals("Message log size not as expected.", 1, logs.size());

        assertTrue(logs.get(0).toString().contains(message));
    }

    public void testLogWithThrowable()
    {
        String message = "test logging";
        Exception exception = new Exception("Test");

        _rootLogger.rawMessage(message, exception);

        List<Object> logs = _rawLogger.getLogMessages();

        assertEquals("Message log size not as expected.", 2, logs.size());

        String loggedMessage = (String) logs.get(0);
        assertTrue("Message not found in log:" + loggedMessage,
                   loggedMessage.contains(message));

        Exception fromLog = (Exception) logs.get(1);
        assertEquals(exception.getMessage(), fromLog.getMessage());
    }


}
