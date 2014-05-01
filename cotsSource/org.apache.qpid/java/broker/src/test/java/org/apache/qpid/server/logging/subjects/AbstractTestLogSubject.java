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
package org.apache.qpid.server.logging.subjects;

import junit.framework.TestCase;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.logging.LogActor;
import org.apache.qpid.server.logging.LogMessage;
import org.apache.qpid.server.logging.LogSubject;
import org.apache.qpid.server.logging.RootMessageLogger;
import org.apache.qpid.server.logging.RootMessageLoggerImpl;
import org.apache.qpid.server.logging.actors.TestLogActor;
import org.apache.qpid.server.logging.rawloggers.UnitTestMessageLogger;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.protocol.InternalTestProtocolSession;
import org.apache.qpid.server.protocol.AMQProtocolSession;

import java.util.List;

/**
 * Abstract Test for LogSubject testing
 * Includes common validation code and two common tests.
 *
 * Each test class sets up the LogSubject and contains details of how to
 * validate this class then performs a log statement with logging enabled and
 * logging disabled.
 *
 * The resulting log file is then validated. 
 *
 */
public abstract class AbstractTestLogSubject extends TestCase
{
    protected Configuration _config = new PropertiesConfiguration();
    protected LogSubject _subject = null;

    AMQProtocolSession _session;

    public void setUp() throws Exception
    {
        super.setUp();

        _config.setProperty(ServerConfiguration.STATUS_UPDATES, "ON");

        VirtualHost virtualHost = ApplicationRegistry.getInstance().
                getVirtualHostRegistry().getVirtualHosts().iterator().next();

        // Create a single session for this test.
        _session = new InternalTestProtocolSession(virtualHost);
    }

    public void tearDown() throws Exception
    {
        // Correctly Close the AR that we created above
        ApplicationRegistry.remove();

        super.tearDown();
    }

    protected List<Object> performLog() throws ConfigurationException
    {
        if (_subject == null)
        {
            throw new NullPointerException("LogSubject has not been set");
        }

        ServerConfiguration serverConfig = new ServerConfiguration(_config);

        UnitTestMessageLogger logger = new UnitTestMessageLogger();
        RootMessageLogger rootLogger =
                new RootMessageLoggerImpl(serverConfig, logger);

        LogActor actor = new TestLogActor(rootLogger);

        actor.message(_subject, new LogMessage()
        {
            public String toString()
            {
                return "<Log Message>";
            }
        });

        return logger.getLogMessages();
    }

    /**
     * Verify that the connection section has the expected items
     *
     * @param connectionID - The connection id (int) to check for
     * @param user         - the Connected username
     * @param ipString     - the ipString/hostname
     * @param vhost        - the virtualhost that the user connected to.
     * @param message      - the message these values should appear in.
     */
    protected void verifyConnection(long connectionID, String user, String ipString, String vhost, String message)
    {
        // This should return us MockProtocolSessionUser@null/test
        String connectionSlice = getSlice("con:" + connectionID, message);

        assertNotNull("Unable to find connection 'con:" + connectionID + "'",
                      connectionSlice);

        // Exract the userName
        String[] userNameParts = connectionSlice.split("@");

        assertEquals("Unable to split Username from rest of Connection:"
                     + connectionSlice, 2, userNameParts.length);

        assertEquals("Username not as expected", userNameParts[0], user);

        // Extract IP.
        // The connection will be of the format - guest@/127.0.0.1:1/test
        // and so our userNamePart will be '/127.0.0.1:1/test'
        String[] ipParts = userNameParts[1].split("/");

        // We will have three sections
        assertEquals("Unable to split IP from rest of Connection:"
                     + userNameParts[1], 3, ipParts.length);

        // We need to skip the first '/' split will be empty so validate 1 as IP
        assertEquals("IP not as expected", ipString, ipParts[1]);

        //Finally check vhost which is section 2
        assertEquals("Virtualhost name not as expected.", vhost, ipParts[2]);
    }

    /**
     * Verify that the RoutingKey is present in the provided message.
     *
     * @param message    The message to check
     * @param routingKey The routing key to check against
     */
    protected void verifyRoutingKey(String message, AMQShortString routingKey)
    {
        String routingKeySlice = getSlice("rk", message);

        assertNotNull("Routing Key not found:" + message, routingKey);

        assertEquals("Routing key not correct",
                     routingKey.toString(), routingKeySlice);
    }

    /**
     * Verify that the given Queue's name exists in the provided message
     *
     * @param message The message to check
     * @param queue   The queue to check against
     */
    protected void verifyQueue(String message, AMQQueue queue)
    {
        String queueSlice = getSlice("qu", message);

        assertNotNull("Queue not found:" + message, queueSlice);

        assertEquals("Queue name not correct",
                     queue.getName().toString(), queueSlice);
    }

    /**
     * Verify that the given exchange (name and type) are present in the
     * provided message.
     *
     * @param message  The message to check
     * @param exchange the exchange to check against
     */
    protected void verifyExchange(String message, Exchange exchange)
    {
        String exchangeSilce = getSlice("ex", message);

        assertNotNull("Exchange not found:" + message, exchangeSilce);

        String[] exchangeParts = exchangeSilce.split("/");

        assertEquals("Exchange should be in two parts ex(type/name)", 2,
                     exchangeParts.length);

        assertEquals("Exchange type not correct",
                     exchange.getType().toString(), exchangeParts[0]);

        assertEquals("Exchange name not correct",
                     exchange.getName().toString(), exchangeParts[1]);

    }

    /**
     * Verify that a VirtualHost with the given name appears in the given
     * message.
     *
     * @param message the message to search
     * @param vhost   the vhostName to check against
     */
    static public void verifyVirtualHost(String message, VirtualHost vhost)
    {
        String vhostSlice = getSlice("vh", message);

        assertNotNull("Virtualhost not found:" + message, vhostSlice);

        assertEquals("Virtualhost not correct", "/" + vhost.getName(), vhostSlice);
    }

    /**
     * Parse the log message and return the slice according to the following:
     * Given Example:
     * con:1(guest@127.0.0.1/test)/ch:2/ex(amq.direct)/qu(myQueue)/rk(myQueue)
     *
     * Each item (except channel) is of the format <key>(<values>)
     *
     * So Given an ID to slice on:
     * con:1 - Connection 1
     * ex - exchange
     * qu - queue
     * rk - routing key
     *
     * @param sliceID the slice to locate
     * @param message the message to search in
     *
     * @return the slice if found otherwise null is returned
     */
    static public String getSlice(String sliceID, String message)
    {
        int indexOfSlice = message.indexOf(sliceID + "(");

        if (indexOfSlice == -1)
        {
            return null;
        }

        int endIndex = message.indexOf(')', indexOfSlice);

        if (endIndex == -1)
        {
            return null;
        }

        return message.substring(indexOfSlice + 1 + sliceID.length(),
                                 endIndex);
    }

    /**
     * Test that when Logging occurs a single log statement is provided
     *
     * @throws ConfigurationException
     */
    public void testEnabled() throws ConfigurationException
    {
        List<Object> logs = performLog();

        assertEquals("Log has incorrect message count", 1, logs.size());

        validateLogStatement(String.valueOf(logs.get(0)));
    }

    /**
     * Call to the individiual tests to validate the message is formatted as
     * expected
     *
     * @param message the message whos format needs validation
     */
    protected abstract void validateLogStatement(String message);

    /**
     * Ensure that when status-updates are off this does not perform logging
     *
     * @throws ConfigurationException
     */
    public void testDisabled() throws ConfigurationException
    {
        _config.setProperty(ServerConfiguration.STATUS_UPDATES, "OFF");

        List<Object> logs = performLog();

        assertEquals("Log has incorrect message count", 0, logs.size());
    }

}
