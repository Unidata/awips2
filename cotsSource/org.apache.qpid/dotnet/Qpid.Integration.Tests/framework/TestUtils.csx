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
using log4net;

using static Apache.Qpid.Integration.Tests.framework.MessagingTestConfigProperties.*;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.*;
using javax.naming.Context;
using javax.naming.InitialContext;
using javax.naming.NamingException;

using System.Collections.Generic.IDictionary;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// TestUtils provides static helper methods that are usefull for writing tests against QPid.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Create connections from test properties. <td> <see cref="MessagingTestConfigProperties"/>
    /// <tr><td> Create test messages.
    /// <tr><td> Inject a short pause in a test.
    /// <tr><td> Serialize properties into a message.
    /// </table>
    /// </summary>
    public class TestUtils
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(TestUtils));

        /// <summary> Some dummy data to stuff all test messages with. </summary>
        private static final byte[] MESSAGE_DATA_BYTES =
            "Test Message -- Test Message -- Test Message -- Test Message -- Test Message -- Test Message -- Test Message -- "
            .getBytes();

        /// <summary>
        /// Establishes a JMS connection using a set of properties and qpids built in JNDI implementation. This is a simple
        /// convenience method for code that does not anticipate handling connection failures. All exceptions that indicate
        /// that the connection has failed, are wrapped as rutime exceptions, presumably handled by a top level failure
        /// handler.
        ///
        /// <p/>This utility makes use of the following test parameters from <see cref="MessagingTestConfigProperties"/> to control
        /// the connection creation:
        ///
        /// <p/><table>
        /// <tr><td> <see cref="MessagingTestConfigProperties#USERNAME_PROPNAME"/> <td> The username.
        /// <tr><td> <see cref="MessagingTestConfigProperties#PASSWORD_PROPNAME"/> <td> The password.
        /// <tr><td> <see cref="MessagingTestConfigProperties#VIRTUAL_HOST_PROPNAME"/> <td> The virtual host name.
        /// <tr><td> <see cref="MessagingTestConfigProperties#BROKER_PROPNAME"/> <td> The broker URL.
        /// <tr><td> <see cref="MessagingTestConfigProperties#CONNECTION_NAME"/> <td> The broker name in the initial context.
        /// </summary>
        /// <param name="messagingProps"> Connection properties as defined in <see cref="MessagingTestConfigProperties"/>. </param>
        ///
        /// <return> A JMS conneciton. </return>
        public static Connection createConnection(ParsedProperties messagingProps)
        {
            log.debug("public static Connection createConnection(ParsedProperties messagingProps = " + messagingProps
                      + "): called");

            try
            {
                // Extract the configured connection properties from the test configuration.
                string conUsername = messagingProps.getProperty(USERNAME_PROPNAME);
                string conPassword = messagingProps.getProperty(PASSWORD_PROPNAME);
                string virtualHost = messagingProps.getProperty(VIRTUAL_HOST_PROPNAME);
                string brokerUrl = messagingProps.getProperty(BROKER_PROPNAME);

                // Create the broker connection url.
                string connectionstring =
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

        /// <summary>
        /// Creates a test message of the specified size, on the given JMS session.
        /// </summary>
        /// <param name="session"> The JMS session. </param>
        /// <param name="size">    The size of the message in bytes. </param>
        ///
        /// <return> A bytes message, of the specified size, filled with dummy data. </return>
        ///
        /// <exception cref="JMSException"> Any underlying JMSExceptions are allowed to fall through. </exception>
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

        /// <summary>
        /// Pauses for the specified length of time. In the event of failing to pause for at least that length of time
        /// due to interuption of the thread, a RutimeException is raised to indicate the failure. The interupted status
        /// of the thread is restores in that case. This method should only be used when it is expected that the pause
        /// will be succesfull, for example in test code that relies on inejecting a pause.
        /// </summary>
        /// <param name="t"> The minimum time to pause for in milliseconds. </param>
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

        /// <summary>
        /// Sets properties of different types on a JMS Message.
        /// </summary>
        /// <param name="message">    The message to set properties on. </param>
        /// <param name="properties"> The property name/value pairs to set. </param>
        ///
        /// <exception cref="javax.jms.JMSException"> All underlying JMSExceptions are allowed to fall through. </exception>
        ///
        /// <remarks> Move this helper method somewhere else. For example, TestUtils.</remarks>
        public static void setPropertiesOnMessage(Message message, Map<Object, Object> properties) throws JMSException
        {
            for (Map.Entry<Object, Object> entry : properties.entrySet())
            {
                string name = entry.getKey().ToString();
                Object value = entry.getValue();

                message.setObjectProperty(name, value);
            }
        }
    }
}