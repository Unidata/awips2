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
package org.apache.qpid.test.framework;

import org.apache.log4j.Logger;

import static org.apache.qpid.test.framework.MessagingTestConfigProperties.*;

import org.apache.qpid.junit.extensions.util.ParsedProperties;

import javax.jms.*;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import java.util.Map;

/**
 * TestUtils provides static helper methods that are usefull for writing tests against QPid.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Create connections from test properties. <td> {@link MessagingTestConfigProperties}
 * <tr><td> Create test messages.
 * <tr><td> Inject a short pause in a test.
 * <tr><td> Serialize properties into a message.
 * </table>
 */
public class TestUtils
{
    /** Used for debugging. */
    private static Logger log = Logger.getLogger(TestUtils.class);

    /** Some dummy data to stuff all test messages with. */
    private static final byte[] MESSAGE_DATA_BYTES =
        "Test Message -- Test Message -- Test Message -- Test Message -- Test Message -- Test Message -- Test Message -- "
        .getBytes();

    /**
     * Establishes a JMS connection using a set of properties and qpids built in JNDI implementation. This is a simple
     * convenience method for code that does not anticipate handling connection failures. All exceptions that indicate
     * that the connection has failed, are wrapped as rutime exceptions, presumably handled by a top level failure
     * handler.
     *
     * <p/>This utility makes use of the following test parameters from {@link MessagingTestConfigProperties} to control
     * the connection creation:
     *
     * <p/><table>
     * <tr><td> {@link MessagingTestConfigProperties#USERNAME_PROPNAME} <td> The username.
     * <tr><td> {@link MessagingTestConfigProperties#PASSWORD_PROPNAME} <td> The password.
     * <tr><td> {@link MessagingTestConfigProperties#VIRTUAL_HOST_PROPNAME} <td> The virtual host name.
     * <tr><td> {@link MessagingTestConfigProperties#BROKER_PROPNAME} <td> The broker URL.
     * <tr><td> {@link MessagingTestConfigProperties#CONNECTION_NAME} <td> The broker name in the initial context.
     *
     * @param messagingProps Connection properties as defined in {@link MessagingTestConfigProperties}.
     *
     * @return A JMS conneciton.
     */
    public static Connection createConnection(ParsedProperties messagingProps)
    {
        log.debug("public static Connection createConnection(ParsedProperties messagingProps = " + messagingProps
            + "): called");

        try
        {
            // Extract the configured connection properties from the test configuration.
            String conUsername = messagingProps.getProperty(USERNAME_PROPNAME);
            String conPassword = messagingProps.getProperty(PASSWORD_PROPNAME);
            String virtualHost = messagingProps.getProperty(VIRTUAL_HOST_PROPNAME);
            String brokerUrl = messagingProps.getProperty(BROKER_PROPNAME);

            // Create the broker connection url.
            String connectionString =
                "amqp://" + conUsername + ":" + conPassword + "@clientid/" + ((virtualHost != null) ? virtualHost : "")
                + "?brokerlist='" + brokerUrl + "'";

            // Create properties to create the initial context from, and inject the connection factory configuration
            // for the defined connection name into it.
            messagingProps.setProperty("connectionfactory." + CONNECTION_NAME, connectionString);

            Context ctx = new InitialContext(messagingProps);

            ConnectionFactory cf = (ConnectionFactory) ctx.lookup(CONNECTION_NAME);

            return cf.createConnection();
        }
        catch (NamingException e)
        {
            throw new RuntimeException("Got JNDI NamingException whilst looking up the connection factory.", e);
        }
        catch (JMSException e)
        {
            throw new RuntimeException("Could not establish connection due to JMSException.", e);
        }
    }

    /**
     * Creates a test message of the specified size, on the given JMS session.
     *
     * @param session The JMS session.
     * @param size    The size of the message in bytes.
     *
     * @return A bytes message, of the specified size, filled with dummy data.
     *
     * @throws JMSException Any underlying JMSExceptions are allowed to fall through.
     */
    public static Message createTestMessageOfSize(Session session, int size) throws JMSException
    {
        BytesMessage message = session.createBytesMessage();

        if (size > 0)
        {
            int div = MESSAGE_DATA_BYTES.length / size;
            int mod = MESSAGE_DATA_BYTES.length % size;

            for (int i = 0; i < div; i++)
            {
                message.writeBytes(MESSAGE_DATA_BYTES);
            }

            if (mod != 0)
            {
                message.writeBytes(MESSAGE_DATA_BYTES, 0, mod);
            }
        }

        return message;
    }

    /**
     * Pauses for the specified length of time. In the event of failing to pause for at least that length of time
     * due to interuption of the thread, a RutimeException is raised to indicate the failure. The interupted status
     * of the thread is restores in that case. This method should only be used when it is expected that the pause
     * will be succesfull, for example in test code that relies on inejecting a pause.
     *
     * @param t The minimum time to pause for in milliseconds.
     */
    public static void pause(long t)
    {
        try
        {
            Thread.sleep(t);
        }
        catch (InterruptedException e)
        {
            // Restore the interrupted status
            Thread.currentThread().interrupt();

            throw new RuntimeException("Failed to generate the requested pause length.", e);
        }
    }

    /**
     * Sets properties of different types on a JMS Message.
     *
     * @param message    The message to set properties on.
     * @param properties The property name/value pairs to set.
     *
     * @throws javax.jms.JMSException All underlying JMSExceptions are allowed to fall through.
     *
     * @todo Move this helper method somewhere else. For example, TestUtils.
     */
    public static void setPropertiesOnMessage(Message message, Map<Object, Object> properties) throws JMSException
    {
        for (Map.Entry<Object, Object> entry : properties.entrySet())
        {
            String name = entry.getKey().toString();
            Object value = entry.getValue();

            message.setObjectProperty(name, value);
        }
    }
}

