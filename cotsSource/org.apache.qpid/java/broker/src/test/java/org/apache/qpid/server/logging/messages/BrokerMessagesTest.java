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

import java.util.List;

/**
 * Test BRK log Messages
 */
public class BrokerMessagesTest extends AbstractTestMessages
{
    public void testBrokerStartup()
    {
        String version = "Qpid 0.6";
        String build = "796936M";

        _logMessage = BrokerMessages.BRK_STARTUP(version, build);
        List<Object> log = performLog();

        String[] expected = {"Startup :", "Version:", version, "Build:", build};

        validateLogMessage(log, "BRK-1001", expected);
    }

    public void testBrokerListening()
    {
        String transport = "TCP";
        Integer port = 2765;

        _logMessage = BrokerMessages.BRK_LISTENING(transport, port);

        List<Object> log = performLog();

        String[] expected = {"Starting", "Listening on ",
                             transport, "port ", String.valueOf(port)};

        validateLogMessage(log, "BRK-1002", expected);
    }

    public void testBrokerShuttingDown()
    {
        String transport = "TCP";
        Integer port = 2765;

        _logMessage = BrokerMessages.BRK_SHUTTING_DOWN(transport, port);

        List<Object> log = performLog();

        String[] expected = {"Shuting down", transport, "port ", String.valueOf(port)};

        validateLogMessage(log, "BRK-1003", expected);
    }

    public void testBrokerReady()
    {
        _logMessage = BrokerMessages.BRK_READY();
        List<Object> log = performLog();

        String[] expected = {"Ready"};

        validateLogMessage(log, "BRK-1004", expected);
    }

    public void testBrokerStopped()
    {
        _logMessage = BrokerMessages.BRK_STOPPED();
        List<Object> log = performLog();

        String[] expected = {"Stopped"};

        validateLogMessage(log, "BRK-1005", expected);
    }

    public void testBrokerConfig()
    {
        String path = "/file/path/to/configuration.xml";

        _logMessage = BrokerMessages.BRK_CONFIG(path);
        List<Object> log = performLog();

        String[] expected = {"Using configuration :", path};

        validateLogMessage(log, "BRK-1006", expected);
    }

    public void testBrokerLogConfig()
    {
        String path = "/file/path/to/configuration.xml";

        _logMessage = BrokerMessages.BRK_LOG_CONFIG(path);
        List<Object> log = performLog();

        String[] expected = {"Using logging configuration :", path};

        validateLogMessage(log, "BRK-1007", expected);
    }

}
