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

import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.util.FileUtils;
import org.apache.qpid.util.LogMonitor;

import javax.jms.Connection;
import javax.jms.Queue;
import javax.jms.Session;
import java.io.File;

public class AlertingTest extends AbstractTestLogging
{
    private String VIRTUALHOST = "test";
    private Session _session;
    private Connection _connection;
    private Queue _destination;
    private int _numMessages;

    private static final int ALERT_LOG_WAIT_PERIOD = 5000;
    private static final String MESSAGE_COUNT_ALERT = "MESSAGE_COUNT_ALERT";

    public void setUp() throws Exception
    {
        // set QPID_WORK to be [QPID_WORK|io.tmpdir]/<testName>
        // This ensures that each of these tests operate independantly.
        setSystemProperty("QPID_WORK",
                          System.getProperty("QPID_WORK",
                                             System.getProperty("java.io.tmpdir"))
                          + File.separator + getName());

        // Update the configuration to make our virtualhost Persistent.
        makeVirtualHostPersistent(VIRTUALHOST);

        _numMessages = 50;

        // Then we do the normal setup stuff like starting the broker, getting a connection etc.
        super.setUp();

        setupConnection();
    }

    @Override
    public void tearDown() throws Exception
    {
        // Ensure queue is clean for next run.
        drainQueue(_destination);
        super.tearDown();
    }


    /**
     * Create a new connection and ensure taht our destination queue is created
     * and bound.
     *
     * Note that the tests here that restart the broker rely on persistence.
     * However, the queue creation here is transient. So the queue will not be
     * rebound on restart. Hence the consumer creation here rather than just the
     * once.
     *
     * The persistent messages will recreate the queue but not bind it (as it
     * was not a durable queue) However, the consumer creation here will ensure
     * that the queue is correctly bound and can receive new messages.
     *
     * @throws Exception
     */
    private void setupConnection()
            throws Exception
    {
        _connection = getConnection();
        _session = _connection.createSession(true, Session.SESSION_TRANSACTED);
        _destination = _session.createQueue(getTestQueueName());

        // Consumer is only used to actually create the destination
        _session.createConsumer(_destination).close();
    }

    /**
     * Checks the log file for MESSAGE_COUNT_ALERT, fails() the test if it's not found and
     * places the entire contents in the message to help debug cruise control failures.
     *
     * @throws Exception
     */
    private void wasAlertFired() throws Exception
    {
        if (!_monitor.waitForMessage(MESSAGE_COUNT_ALERT, ALERT_LOG_WAIT_PERIOD))
        {
            StringBuffer message = new StringBuffer("Could not find 'MESSAGE_COUNT_ALERT' in log file: " + _monitor.getMonitoredFile().getAbsolutePath());
            message.append("\n");

            // Add the current contents of the log file to test output
            message.append(_monitor.readFile());

            // Write the test config file to test output
            message.append("Server configuration overrides in use:\n");
            message.append(FileUtils.readFileAsString(getTestConfigFile()));

            message.append("\nVirtualhost maxMessageCount:\n");                        
            message.append((new ServerConfiguration(_configFile)).getConfig().getString("virtualhosts.virtualhost." + VIRTUALHOST + ".queues.maximumMessageCount"));

            fail(message.toString());
        }
    }

    public void testAlertingReallyWorks() throws Exception
    {
        // Send 5 messages, make sure that the alert was fired properly. 
        sendMessage(_session, _destination, _numMessages + 1);
        _session.commit();
        wasAlertFired();
    }

    public void testAlertingReallyWorksWithRestart() throws Exception
    {
        sendMessage(_session, _destination, _numMessages + 1);
        _session.commit();
        stopBroker();

        // Rest the monitoring clearing the current output file.
        _monitor.reset();
        startBroker();
        wasAlertFired();
    }

    /**
     * Test that if the alert value is change from the previous value we can
     * still get alerts.
     *
     * Test sends two messages to the broker then restarts the broker with new
     * configuration.
     *
     * If the test is running inVM the test validates that the new configuration
     * has been applied.
     *
     * Validates that we only have two messages on the queue and then sends
     * enough messages to trigger the alert.
     *
     * The alert is then validate.
     *
     *
     * @throws Exception
     */
    public void testAlertingReallyWorksWithChanges() throws Exception
    {
        // send some messages and nuke the logs
        sendMessage(_session, _destination, 2);
        _session.commit();
        // To prevent any failover/retry/connection dropped errors
        _connection.close();

        stopBroker();

        _monitor.reset();

        // Change max message count to 5, start broker and make sure that that's triggered at the right time
        setConfigurationProperty("virtualhosts.virtualhost." + VIRTUALHOST + ".queues.maximumMessageCount", "5");

        startBroker();

        if (!isExternalBroker())
        {
            assertEquals("Alert Max Msg Count is not correct", 5, ApplicationRegistry.getInstance().getVirtualHostRegistry().
                    getVirtualHost(VIRTUALHOST).getQueueRegistry().getQueue(new AMQShortString(_destination.getQueueName())).
                    getMaximumMessageCount());
        }

        setupConnection();

        // Validate the queue depth is as expected
        long messageCount = ((AMQSession) _session).getQueueDepth((AMQDestination) _destination);
        assertEquals("Broker has invalid message count for test", 2, messageCount);

        // Ensure the alert has not occured yet
        assertLoggingNotYetOccured(MESSAGE_COUNT_ALERT);

        // Trigger the new value
        sendMessage(_session, _destination, 3);
        _session.commit();

        // Validate that the alert occured.
        wasAlertFired();
    }

}
