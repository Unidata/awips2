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
package org.apache.qpid.server.logging;

import junit.framework.AssertionFailedError;
import org.apache.qpid.util.LogMonitor;

import java.util.List;
import java.io.File;

/**
 * Management Console Test Suite
 *
 * The Management Console test suite validates that the follow log messages as specified in the Functional Specification.
 *
 * This suite of tests validate that the management console messages occur correctly and according to the following format:
 *
 * MNG-1001 : Startup
 * MNG-1002 : Starting : <service> : Listening on port <Port>
 * MNG-1003 : Shutting down : <service> : port <Port>
 * MNG-1004 : Ready
 * MNG-1005 : Stopped
 * MNG-1006 : Using SSL Keystore : <path>
 */
public class ManagementLoggingTest extends AbstractTestLogging
{
    private static final String MNG_PREFIX = "MNG-";

    public void setUp() throws Exception
    {
        // We either do this here or have a null check in tearDown.
        // As when this test is run against profiles other than java it will NPE
        _monitor = new LogMonitor(_outputFile);
        //We explicitly do not call super.setUp as starting up the broker is
        //part of the test case.

    }

    /**
     * Description:
     * Using the startup configuration validate that the management startup
     * message is logged correctly.
     * Input:
     * Standard configuration with management enabled
     * Output:
     *
     * <date> MNG-1001 : Startup
     *
     * Constraints:
     * This is the FIRST message logged by MNG
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. This is the FIRST message logged by MNG
     */
    public void testManagementStartupEnabled() throws Exception
    {
        // This test only works on external java brokers due to the fact that
        // Management is disabled on InVM brokers.
        if (isJavaBroker() && isExternalBroker())
        {
            //Ensure management is on
            setConfigurationProperty("management.enabled", "true");

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            List<String> results = _monitor.findMatches(MNG_PREFIX);
            try
            {
                // Validation

                assertTrue("MNGer message not logged", results.size() > 0);

                String log = getLog(results.get(0));

                //1
                validateMessageID("MNG-1001", log);

                //2
                results = _monitor.findMatches("MNG-1001");
                assertEquals("More than one startup message found.",
                             1, results.size());

                //3
                assertEquals("Startup log message is not 'Startup'.", "Startup",
                             getMessageString(log));
            }
            catch (AssertionFailedError afe)
            {
                System.err.println("Log Dump:");
                for (String log : results)
                {
                    System.err.println(log);
                }
                throw afe;
            }
        }
    }

    /**
     * Description:
     * Verify that when management is disabled in the configuration file the
     * startup message is not logged.
     * Input:
     * Standard configuration with management disabled
     * Output:
     * NO MNG messages
     * Validation Steps:
     *
     * 1. Validate that no MNG messages are produced.
     */
    public void testManagementStartupDisabled() throws Exception
    {
        // This test only works on external java brokers due to the fact that
        // Management is disabled on InVM brokers.
        if (isJavaBroker() && isExternalBroker())
        {
            //Ensure management is off
            setConfigurationProperty("management.enabled", "false");

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            List<String> results = _monitor.findMatches(MNG_PREFIX);
            try
            {
                // Validation

                assertEquals("MNGer messages logged", 0, results.size());
            }
            catch (AssertionFailedError afe)
            {
                System.err.println("Log Dump:");
                for (String log : results)
                {
                    System.err.println(log);
                }
                throw afe;
            }
        }
    }

    /**
     * The two MNG-1002 messages are logged at the same time so lets test them
     * at the same time.
     *
     * Description:
     * Using the default configuration validate that the RMI Registry socket is
     * correctly reported as being opened
     *
     * Input:
     * The default configuration file
     * Output:
     *
     * <date> MESSAGE MNG-1002 : Starting : RMI Registry : Listening on port 8999
     *
     * Constraints:
     * The RMI ConnectorServer and Registry log messages do not have a prescribed order
     * Validation Steps:
     *
     * 1. The MNG ID is correct
     * 2. The specified port is the correct '8999'
     *
     * Description:
     * Using the default configuration validate that the RMI ConnectorServer
     * socket is correctly reported as being opened
     *
     * Input:
     * The default configuration file
     * Output:
     *
     * <date> MESSAGE MNG-1002 : Starting : RMI ConnectorServer : Listening on port 9099
     *
     * Constraints:
     * The RMI ConnectorServer and Registry log messages do not have a prescribed order
     * Validation Steps:
     *
     * 1. The MNG ID is correct
     * 2. The specified port is the correct '9099'
     */
    public void testManagementStartupRMIEntries() throws Exception
    {
        // This test only works on external java brokers due to the fact that
        // Management is disabled on InVM brokers.
        if (isJavaBroker() && isExternalBroker())
        {
            //Ensure management is on
            setConfigurationProperty("management.enabled", "true");

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            List<String> results = _monitor.findMatches("MNG-1002");
            try
            {
                // Validation

                assertEquals("MNGer message not logged expected message", 2, results.size());

                String log = getLog(results.get(0));

                //1
                validateMessageID("MNG-1002", log);

                // Validate we only have one MNG-1002
                results = _monitor.findMatches("MNG-1002");
                assertEquals("More than two RMI entries found.",
                             2, results.size());

                // We expect the RMI Server port to be 100 higher than
                // the RMIConnector Server Port                
                int mPort = getPort() + (DEFAULT_MANAGEMENT_PORT - DEFAULT_PORT);
                assertTrue("RMI Registry port not as expected(" + mPort + ").:" + getMessageString(log),
                           getMessageString(log).endsWith(String.valueOf(mPort)));

                log = getLog(results.get(1));

                //1
                validateMessageID("MNG-1002", log);

                // We expect the RMIConnector Server port to be 100 higher than
                // the RMI Server Port
                mPort = getPort() + (DEFAULT_MANAGEMENT_PORT - DEFAULT_PORT) + 100;
                assertTrue("RMI ConnectorServer port not as expected(" + mPort + ").:" + getMessageString(log),
                           getMessageString(log).endsWith(String.valueOf(mPort)));
            }
            catch (AssertionFailedError afe)
            {
                System.err.println("Log Dump:");
                for (String log : results)
                {
                    System.err.println(log);
                }
                throw afe;
            }
        }
    }
    /**
     * Description:
     * Using the default configuration with SSL enabled for the management port the SSL Keystore path should be reported via MNG-1006
     * Input:
     * Management SSL enabled default configuration.
     * Output:
     *
     * <date> MESSAGE MNG-1006 : Using SSL Keystore : test_resources/ssl/keystore.jks
     *
     * Validation Steps:
     *
     * 1. The MNG ID is correct
     * 2. The keystore path is as specified in the configuration
     */
    public void testManagementStartupSSLKeystore() throws Exception
    {
        // This test only works on external java brokers due to the fact that
        // Management is disabled on InVM brokers.
        if (isJavaBroker() && isExternalBroker())
        {
            //Ensure management is on
            setConfigurationProperty("management.enabled", "true");
            // This test requires we have an ssl connection
            setConfigurationProperty("management.ssl.enabled", "true");

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            List<String> results = _monitor.findMatches("MNG-1006");
            try
            {
                // Validation

                assertTrue("MNGer message not logged", results.size() > 0);

                String log = getLog(results.get(0));

                //1
                validateMessageID("MNG-1006", log);

                // Validate we only have one MNG-1002
                results = _monitor.findMatches("MNG-1006");
                assertEquals("More than one SSL Keystore entry found.",
                             1, results.size());

                // We expect the RMIConnector Server port to be 100 higher than
                // the RMI Server Port
                assertTrue("SSL Keystore entry expected.:" + getMessageString(log),
                           getMessageString(log).endsWith(new File(getConfigurationStringProperty("management.ssl.keyStorePath")).getName()));
            }
            catch (AssertionFailedError afe)
            {
                System.err.println("Log Dump:");
                for (String log : results)
                {
                    System.err.println(log);
                }
                throw afe;
            }
        }

    }
}
