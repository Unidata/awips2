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
import org.apache.qpid.server.Main;
import org.apache.qpid.transport.ConnectionException;
import org.apache.qpid.util.LogMonitor;

import java.io.File;
import java.io.IOException;
import java.net.Socket;
import java.util.List;

/**
 * Broker Test Suite
 *
 * The Broker test suite validates that the follow log messages as specified in the Functional Specification.
 *
 * BRK-1001 : Startup : Version: <Version> Build: <Build>
 * BRK-1002 : Starting : Listening on <Transport> port <Port>
 * BRK-1003 : Shuting down : <Transport> port <Port>
 * BRK-1004 : Ready
 * BRK-1005 : Stopped
 * BRK-1006 : Using configuration : <path>
 * BRK-1007 : Using logging configuration : <path>
 *
 * These messages should only occur during startup. The tests need to verify the order of messages. In the case of the BRK-1002 and BRK-1003 the respective ports should only be available between the two log messages.
 */
public class BrokerLoggingTest extends AbstractTestLogging
{
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
     * On startup the broker must report the active configuration file. The
     * logging system must output this so that we can know what configuration
     * is being used for this broker instance.
     *
     * Input:
     * The value of -c specified on the command line.
     * Output:
     * <date> MESSAGE BRK-1006 : Using configuration : <config file>
     * Constraints:
     * This MUST BE the first BRK log message.
     *
     * Validation Steps:
     * 1. This is first BRK log message.
     * 2. The BRK ID is correct
     * 3. The config file is the full path to the file specified on
     * the commandline.
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerStartupConfiguration() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);


            String configFilePath = _configFile.toString();

            List<String> results = _monitor.findMatches("BRK-");
            try
            {
                // Validation

                assertTrue("BRKer message not logged", results.size() > 0);

                String log = getLog(results.get(0));

                //1
                validateMessageID("BRK-1006", log);

                //2
                results = _monitor.findMatches("BRK-1006");
                assertEquals("More than one configuration message found.",
                             1, results.size());

                //3
                assertTrue("Config file details not correctly logged",
                           log.endsWith(configFilePath));
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
     * On startup the broker must report correctly report the log4j file in use. This is important as it can help diagnose why logging messages are not being reported.
     * Input:
     * No custom -l value should be provided on the command line so that the default value is correctly reported.
     * Output:
     *
     * <date> MESSAGE BRK-1007 : Using logging configuration : <$QPID_HOME>/etc/log4j.xml
     *
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. This occurs before the BRK-1001 startup message.
     * 3. The log4j file is the full path to the file specified on the commandline.
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerStartupDefaultLog4j() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            String TESTID = "BRK-1007";

            //Remove test Log4j config from the commandline
            _broker = _broker.substring(0, _broker.indexOf("-l"));

            // As a result of removing the test log4j config
            // we will pick up the broker default and will write
            // data to the standard qpid.log file. Which means that the start
            // broker process will not be monitoring the right file for startup
            // messages. Therefore:

            // Set the broker.ready string to check for the _log4j default that
            // is still present on standard out. 
            setTestClientSystemProperty(BROKER_READY, "Qpid Broker Ready");

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            // Ensure broker has fully started up.
            getConnection();

            List<String> results = _monitor.findMatches("BRK-");
            try
            {
                // Validation

                assertTrue("BRKer message not logged", results.size() > 0);

                boolean validation = false;
                for (String rawLog : results)
                {
                    // We don't care about messages after we have our log config
                    if (validation)
                    {
                        break;
                    }

                    String log = getLog(rawLog);

                    // Ensure we do not have a BRK-1001 message before
                    if (!getMessageID(log).equals(TESTID))
                    {
                        assertFalse(getMessageID(log).equals("BRK-1001"));
                        continue;
                    }

                    //1
                    validateMessageID(TESTID, log);

                    //2
                    assertEquals("More than one log4j configuration message found.",
                                 1, _monitor.findMatches(TESTID).size());

                    //3
                    String defaultLog4j = _configFile.getParent() + "/" + Main.DEFAULT_LOG_CONFIG_FILENAME;
                    assertTrue("Log4j file(" + defaultLog4j + ") details not correctly logged:" + getMessageString(log),
                               getMessageString(log).endsWith(defaultLog4j));

                    validation = true;
                }

                assertTrue("Validation not performed: " + TESTID + " not logged", validation);
            }
            catch (AssertionFailedError afe)
            {
                System.err.println("Log Dump:");
                for (String log : results)
                {
                    System.err.println(log);
                }

                if (results.size() == 0)
                {
                    System.err.println("Monitored file contents:");
                    System.err.println(_monitor.readFile());
                }

                throw afe;
            }
        }
    }

    /**
     * Description:
     * On startup the broker must report correctly report the log4j file in use. This is important as it can help diagnose why logging messages are not being reported. The broker must also be capable of correctly recognising the command line property to specify the custom logging configuration.
     * Input:
     * The value of -l specified on the command line.
     * Output:
     *
     * <date> MESSAGE BRK-1007 : Using logging configuration : <log4j file>
     *
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. This should occur before the BRK-1001 : Startup message
     * 3. The log4j file is the full path to the file specified on the commandline.
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerStartupCustomLog4j() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            // Get custom -l value used during testing for the broker startup
            String customLog4j = _broker.substring(_broker.indexOf("-l") + 2);

            String TESTID = "BRK-1007";

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);


            // Ensure broker has fully started up.
            getConnection();

            List<String> results = _monitor.findMatches("BRK-");
            try
            {
                // Validation

                assertTrue("BRKer message not logged", results.size() > 0);

                boolean validation = false;
                for (String rawLog : results)
                {
                    // We don't care about messages after we have our log config
                    if (validation)
                    {
                        break;
                    }
                    String log = getLog(rawLog);

                    // Ensure we do not have a BRK-1001 message before
                    if (!getMessageID(log).equals(TESTID))
                    {
                        assertFalse(getMessageID(log).equals("BRK-1001"));
                        continue;
                    }

                    //1
                    validateMessageID(TESTID, log);

                    //2
                    assertEquals("More than one log4j configuration message found.",
                                 1, _monitor.findMatches(TESTID).size());

                    //3
                    assertTrue("Log4j file details not correctly logged:" + getMessageString(log),
                               getMessageString(log).endsWith(customLog4j));

                    validation = true;
                }

                assertTrue("Validation not performed: " + TESTID + " not logged", validation);
            }
            catch (AssertionFailedError afe)
            {
                System.err.println("Log Dump:");
                for (String log : results)
                {
                    System.err.println(log);
                }

                if (results.size() == 0)
                {
                    System.err.println("Monitored file contents:");
                    System.err.println(_monitor.readFile());
                }

                throw afe;
            }
        }
    }

    /**
     * Description: On startup the broker reports the broker version number and svn build revision. This information is retrieved from the resource 'qpidversion.properties' which is located via the classloader.
     * Input: The 'qpidversion.properties' file located on the classpath.
     * Output:
     *
     * <date> MESSAGE BRK-1001 : Startup : qpid Version: 0.6 Build: 767150
     *
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. This occurs before any BRK-1002 listening messages are reported.
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerStartupStartup() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            String TESTID = "BRK-1001";

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            List<String> results = _monitor.findMatches("BRK-");
            try
            {
                // Validation

                assertTrue("BRKer message not logged", results.size() > 0);

                boolean validation = false;
                for (String rawLog : results)
                {
                    if (validation)
                    {
                        //Stop checking once we have got to our startup test
                        break;
                    }
                    String log = getLog(rawLog);

                    // Ensure we do not have a BRK-1002 message
                    if (!getMessageID(log).equals(TESTID))
                    {
                        assertFalse(getMessageID(log).equals("BRK-1002"));
                        continue;
                    }

                    //1
                    validateMessageID(TESTID, log);

                    //2
                    assertEquals("More than one startup message found.",
                                 1, _monitor.findMatches(TESTID).size());

                    validation = true;
                }

                assertTrue("Validation not performed: " + TESTID + " not logged", validation);
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
     * On startup the broker may listen on a number of ports and protocols. Each of these must be reported as they are made available.
     * Input:
     * The default configuration with no SSL
     * Output:
     *
     * <date> MESSAGE BRK-1002 : Starting : Listening on TCP port 5672
     *
     * Constraints:
     * Additional broker configuration will occur between the Startup(BRK-1001) and Starting(BRK-1002) messages depending on what VirtualHosts are configured.
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. This occurs after the BRK-1001 startup message
     * 3. Using the default configuration a single BRK-1002 will be printed showing values TCP / 5672
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerStartupListeningTCPDefault() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            String TESTID = "BRK-1002";

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            // Ensure broker has fully started up.
            getConnection();

            List<String> results = _monitor.findMatches("BRK-");
            try
            {
                // Validation

                assertTrue("BRKer message not logged", results.size() > 0);

                boolean validation = false;
                boolean foundBRK1001 = false;
                for (String rawLog : results)
                {
                    String log = getLog(rawLog);

                    // Ensure we do not have a BRK-1002 message
                    if (!getMessageID(log).equals(TESTID))
                    {
                        if (getMessageID(log).equals("BRK-1001"))
                        {
                            foundBRK1001 = true;
                        }
                        continue;
                    }

                    assertTrue("BRK-1001 not logged before this message", foundBRK1001);

                    //1
                    validateMessageID(TESTID, log);

                    //2
                    assertEquals("More than one listen message found.",
                                 1, _monitor.findMatches(TESTID).size());

                    //3
                    String message = getMessageString(log);
                    assertTrue("Expected Listen log not correct" + message,
                               message.endsWith("Listening on TCP port " + getPort()));

                    validation = true;
                }

                assertTrue("Validation not performed: " + TESTID + " not logged", validation);
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
     * On startup the broker may listen on a number of ports and protocols. Each of these must be reported as they are made available.
     * Input:
     * The default configuration with SSL enabled
     * Output:
     *
     * <date> MESSAGE BRK-1002 : Starting : Listening on TCP port 5672
     * <date> MESSAGE BRK-1002 : Starting : Listening on TCP/SSL port 8672
     *
     * Constraints:
     * Additional broker configuration will occur between the Startup(BRK-1001) and Starting(BRK-1002) messages depending on what VirtualHosts are configured.
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. This occurs after the BRK-1001 startup message
     * 3. With SSL enabled in the configuration two BRK-1002 will be printed (order is not specified)
     * 1. One showing values TCP / 5672
     * 2. One showing values TCP/SSL / 5672
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerStartupListeningTCPSSL() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            String TESTID = "BRK-1002";

            // Enable SSL on the connection
            setConfigurationProperty("connector.ssl.enabled", "true");
            setConfigurationProperty("connector.ssl.sslOnly", "false");
            setConfigurationProperty("connector.ssl.keyStorePath", getConfigurationStringProperty("management.ssl.keyStorePath"));
            setConfigurationProperty("connector.ssl.keyStorePassword", getConfigurationStringProperty("management.ssl.keyStorePassword"));

            Integer sslPort = Integer.parseInt(getConfigurationStringProperty("connector.sslport"));

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            // Ensure broker has fully started up.
            getConnection();

            List<String> results = _monitor.findMatches("BRK-");
            try
            {
                // Validation

                assertTrue("BRKer message not logged", results.size() > 0);

                boolean validation = false;
                boolean foundBRK1001 = false;
                for (String rawLog : results)
                {
                    String log = getLog(rawLog);

                    // Ensure we do not have a BRK-1002 message
                    if (!getMessageID(log).equals(TESTID))
                    {
                        if (getMessageID(log).equals("BRK-1001"))
                        {
                            foundBRK1001 = true;
                        }
                        continue;
                    }

                    assertTrue("BRK-1001 not logged before this message", foundBRK1001);

                    //1
                    validateMessageID(TESTID, log);

                    //2
                    List<String> listenMessages  = _monitor.findMatches(TESTID);
                    assertEquals("Two listen messages should be found.",
                                 2, listenMessages .size());

                    //3
                    String message = getMessageString(getLog(listenMessages .get(0)));
                    assertTrue("Expected Listen log not correct" + message,
                               message.endsWith("Listening on TCP port " + getPort()));

                    // Check second, ssl, listen.
                    message = getMessageString(getLog(listenMessages .get(1)));
                    assertTrue("Expected Listen log not correct" + message,
                               message.endsWith("Listening on TCP/SSL port " + sslPort));

                    //4 Test ports open
                    testSocketOpen(getPort());
                    testSocketOpen(sslPort);

                    validation = true;
                }

                assertTrue("Validation not performed: " + TESTID + " not logged", validation);
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
     * The final message the broker will print when it has performed all initialisation and listener startups will be to log the BRK-1004 Ready message
     * Input:
     * No input, all successful broker startups will show BRK-1004 messages.
     * Output:
     *
     * 2009-07-09 15:50:20 +0100 MESSAGE BRK-1004 : Ready
     *
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. This occurs after the BRK-1001 startup message
     * 3. This must be the last message the broker prints after startup. Currently, if there is no further interaction with the broker then there should be no more logging.
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerStartupReady() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            String TESTID = "BRK-1004";

            startBroker();

            //Ensure the broker has fully started up.
            getConnection();

            List<String> results = _monitor.findMatches("BRK-");
            try
            {
                // Validation

                assertTrue("BRKer message not logged", results.size() > 0);

                boolean validation = false;
                boolean foundBRK1001 = false;
                for (String rawLog : results)
                {
                    assertFalse("More broker log statements present after ready message", validation);
                    String log = getLog(rawLog);

                    // Ensure we do not have a BRK-1002 message
                    if (!getMessageID(log).equals(TESTID))
                    {
                        if (getMessageID(log).equals("BRK-1001"))
                        {
                            foundBRK1001 = true;
                        }
                        continue;
                    }

                    assertTrue("BRK-1001 not logged before this message", foundBRK1001);

                    //1
                    validateMessageID(TESTID, log);

                    //2
                    assertEquals("More than one ready message found.",
                                 1, _monitor.findMatches(TESTID).size());

                    //3
                    assertEquals("Ready message not present", "Ready", getMessageString(log));

                    validation = true;
                }

                assertTrue("Validation not performed: " + TESTID + " not logged", validation);
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
     * On startup the broker may listen on a number of ports and protocols. Each of these must then report a shutting down message as they stop listening.
     * Input:
     * The default configuration with no SSL
     * Output:
     *
     * <date> MESSAGE BRK-1003 : Shutting down : TCP port 5672
     *
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. Only TCP is reported with the default configuration with no SSL.
     * 3. The default port is correct
     * 4. The port is not accessible after this message
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerShutdownListeningTCPDefault() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            String TESTID = "BRK-1003";

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            stopBroker();

            //Give broker time to shutdown and flush log
            checkSocketClosed(getPort());

            List<String> results = _monitor.findMatches("BRK-");
            try
            {
                // Validation

                assertTrue("BRKer message not logged", results.size() > 0);

                boolean validation = false;
                boolean foundBRK1001 = false;
                for (String rawLog : results)
                {
                    String log = getLog(rawLog);

                    // Ensure we do not have a BRK-1002 message
                    if (!getMessageID(log).equals(TESTID))
                    {
                        if (getMessageID(log).equals("BRK-1001"))
                        {
                            foundBRK1001 = true;
                        }
                        continue;
                    }

                    assertTrue("BRK-1001 not logged before this message", foundBRK1001);

                    //1
                    validateMessageID(TESTID, log);

                    //2
                    assertEquals("More than one listen message found.",
                                 1, _monitor.findMatches(TESTID).size());

                    //3
                    String message = getMessageString(log);
                    assertTrue("Expected shutdown log not correct" + message,
                               message.endsWith("TCP port " + getPort()));

                    //4
                    checkSocketClosed(getPort());

                    validation = true;
                }

                assertTrue("Validation not performed: " + TESTID + " not logged", validation);
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
     * On startup the broker may listen on a number of ports and protocols. Each of these must be reported as they are made available.
     * Input:
     * The default configuration with SSL enabled
     * Output:
     *
     * <date> MESSAGE BRK-1002 : Starting : Listening on TCP port 5672
     * <date> MESSAGE BRK-1002 : Starting : Listening on TCP/SSL port 8672
     *
     * Constraints:
     * Additional broker configuration will occur between the Startup(BRK-1001) and Starting(BRK-1002) messages depending on what VirtualHosts are configured.
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. This occurs after the BRK-1001 startup message
     * 3. With SSL enabled in the configuration two BRK-1002 will be printed (order is not specified)
     * 1. One showing values TCP / 5672
     * 2. One showing values TCP/SSL / 5672
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerShutdownListeningTCPSSL() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            String TESTID = "BRK-1003";

            // Enable SSL on the connection
            setConfigurationProperty("connector.ssl.enabled", "true");
            setConfigurationProperty("connector.ssl.keyStorePath", getConfigurationStringProperty("management.ssl.keyStorePath"));
            setConfigurationProperty("connector.ssl.keyStorePassword", getConfigurationStringProperty("management.ssl.keyStorePassword"));

            Integer sslPort = Integer.parseInt(getConfigurationStringProperty("connector.sslport"));

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);


//            //Clear any startup messages as we don't need them for validation
//            _monitor.reset();
            //Stop the broker to get the log messages for testing
            stopBroker();

            //Give broker time to shutdown and flush log
            checkSocketClosed(getPort());

            List<String> results = _monitor.findMatches(TESTID);
            try
            {
                // Validation

                assertTrue(TESTID + " messages not logged", results.size() > 0);

                String log = getLog(results.get(0));

                //1
                validateMessageID(TESTID, log);

                //2
                List<String> listenMessages = _monitor.findMatches(TESTID);
                assertEquals("Two shutdown messages should be found.",
                             2, listenMessages.size());

                //3
                String message = getMessageString(getLog(listenMessages.get(0)));
                assertTrue("Expected shutdown log not correct" + message,
                           message.endsWith("TCP port " + getPort()));

                // Check second, ssl, listen.
                message = getMessageString(getLog(listenMessages.get(1)));
                assertTrue("Expected shutdown log not correct" + message,
                           message.endsWith("TCP/SSL port " + sslPort));

                //4
                //Test Port closed
                checkSocketClosed(getPort());
                //Test SSL Port closed
                checkSocketClosed(sslPort);
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
     * Input:
     * No input, all clean broker shutdowns will show BRK-1005 messages.
     * Output:
     *
     * <date> MESSAGE BRK-1005 : Stopped
     *
     * Constraints:
     * This is the LAST message the broker will log.
     * Validation Steps:
     *
     * 1. The BRK ID is correct
     * 2. This is the last message the broker will log.
     *
     * @throws Exception caused by broker startup
     */
    public void testBrokerShutdownStopped() throws Exception
    {
        // This logging startup code only occurs when you run a Java broker,
        // that broker must be started via Main so not an InVM broker.
        if (isJavaBroker() && isExternalBroker())
        {
            String TESTID = "BRK-1005";

            startBroker();

            // Now we can create the monitor as _outputFile will now be defined
            _monitor = new LogMonitor(_outputFile);

            getConnection().close();

            stopBroker();

            // Ensure the broker has shutdown before retreving results
            checkSocketClosed(getPort());

            List<String> results = _monitor.findMatches("BRK-");
            try
            {
                // Validation

                assertTrue("BRKer message not logged", results.size() > 0);

                boolean validation = false;
                for (String rawLog : results)
                {
                    assertFalse("More broker log statements present after ready message", validation);
                    String log = getLog(rawLog);

                    // Ignore all logs until we get to the test id.
                    if (!getMessageID(log).equals(TESTID))
                    {
                        continue;
                    }

                    //1
                    validateMessageID(TESTID, log);

                    //2
                    assertEquals("More than one ready message found.",
                                 1, _monitor.findMatches(TESTID).size());

                    //3
                    assertEquals("Stopped message not present", "Stopped", getMessageString(log));

                    validation = true;
                }

                assertTrue("Validation not performed: " + TESTID + " not logged", validation);
            }
            catch (AssertionFailedError afe)
            {
                System.err.println("Log Dump:");
                for (String log : results)
                {
                    System.err.println(log);
                }

                System.err.println("Monitored file contents:");
                System.err.println(_monitor.readFile());

                throw afe;
            }
        }
    }

    /**
     * Test that a socket on the given port is closed.
     *
     * Does this by attempting to connect to the port and expecting a
     * ConnectionRefused IOException or a ConnectionException
     *
     * @param port the port number
     */
    private void checkSocketClosed(int port)
    {
        try
        {
            Socket socket = new Socket((String) null, port);
            fail("Socket not closed on port:" + port);
        }
        catch (ConnectionException e)
        {
            //normal path
        }
        catch (IOException e)
        {
            if (!e.getMessage().equals("Connection refused"))
            {
                fail("Socket not closed on port:" + port + ":" + e.getMessage());
                // Keep stack trace for diagnosis.
                e.printStackTrace(System.err);
            }
        }
    }

    /**
     * Test that a socket on the given port is open.
     *
     * Does this by attempting to connect to the port and expecting a
     * The connection to succeed.
     * It then closes the socket and expects that to work cleanly.
     *
     * @param port the port number
     */
    private void testSocketOpen(int port)
    {
        try
        {
            Socket socket = new Socket((String) null, port);
            socket.close();
        }
        catch (IOException e)
        {
            fail("Unable to open and close socket to port:" + port
                 + ". Due to:" + e.getMessage());
        }
    }
}
