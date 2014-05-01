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
package org.apache.qpid.interop.clienttestcases;

import org.apache.log4j.Logger;

import org.apache.qpid.test.framework.TestUtils;
import org.apache.qpid.test.framework.distributedtesting.TestClient;
import org.apache.qpid.test.framework.distributedtesting.TestClientControlledTest;

import javax.jms.*;

/**
 * Implements test case 4, P2P messages with message size. Sends/received a specified number of messages to a specified
 * route on the default direct exchange, of a specified size. Produces reports on the actual number of messages
 * sent/received.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Supply the name of the test case that this implements.
 * <tr><td> Accept/Reject invites based on test parameters.
 * <tr><td> Adapt to assigned roles.
 * <tr><td> Send required number of test messages.
 * <tr><td> Generate test reports.
 * </table>
 */
public class TestCase4P2PMessageSize implements TestClientControlledTest, MessageListener
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(TestCase4P2PMessageSize.class);

    /** Holds the count of test messages received. */
    private int messageCount;

    /** The role to be played by the test. */
    private Roles role;

    /** The number of test messages to send. */
    private int numMessages;

    /** The size of the test messages to send. */
    private int messageSize;

    /** The connection to send the test messages on. */
    private Connection connection;

    /** The controlSession to send the test messages on. */
    private Session session;

    /** The producer to send the test messages with. */
    MessageProducer producer;

    /**
     * Should provide the name of the test case that this class implements. The exact names are defined in the
     * interop testing spec.
     *
     * @return The name of the test case that this implements.
     */
    public String getName()
    {
        log.debug("public String getName(): called");

        return "TC4_P2PMessageSize";
    }

    /**
     * Determines whether the test invite that matched this test case is acceptable.
     *
     * @param inviteMessage The invitation to accept or reject.
     *
     * @return <tt>true</tt> to accept the invitation, <tt>false</tt> to reject it.
     *
     * @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
     */
    public boolean acceptInvite(Message inviteMessage) throws JMSException
    {
        log.debug("public boolean acceptInvite(Message inviteMessage = " + inviteMessage + "): called");

        // All invites are acceptable.
        return true;
    }

    /**
     * Assigns the role to be played by this test case. The test parameters are fully specified in the
     * assignment message. When this method return the test case will be ready to execute.
     *
     * @param role              The role to be played; sender or receivers.
     *
     * @param assignRoleMessage The role assingment message, contains the full test parameters.
     *
     * @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
     */
    public void assignRole(Roles role, Message assignRoleMessage) throws JMSException
    {
        log.debug("public void assignRole(Roles role = " + role + ", Message assignRoleMessage = " + assignRoleMessage
            + "): called");

        // Reset the message count for a new test.
        messageCount = 0;

        // Take note of the role to be played.
        this.role = role;

        // Create a new connection to pass the test messages on.
        connection = TestUtils.createConnection(TestClient.testContextProperties);
        session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        // Extract and retain the test parameters.
        numMessages = assignRoleMessage.getIntProperty("P2P_NUM_MESSAGES");
        messageSize = assignRoleMessage.getIntProperty("messageSize");
        Destination sendDestination = session.createQueue(assignRoleMessage.getStringProperty("P2P_QUEUE_AND_KEY_NAME"));

        log.debug("numMessages = " + numMessages);
        log.debug("sendDestination = " + sendDestination);
        log.debug("role = " + role);

        switch (role)
        {
        // Check if the sender role is being assigned, and set up a message producer if so.
        case SENDER:
            producer = session.createProducer(sendDestination);
            break;

        // Otherwise the receivers role is being assigned, so set this up to listen for messages.
        case RECEIVER:
            MessageConsumer consumer = session.createConsumer(sendDestination);
            consumer.setMessageListener(this);
            break;
        }

        connection.start();
    }

    /**
     * Performs the test case actions. Returning from here, indicates that the sending role has completed its test.
     *
     * @param numMessages The number of test messages to send.
     *
     * @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
     */
    public void start(int numMessages) throws JMSException
    {
        log.debug("public void start(): called");

        // Check that the sender role is being performed.
        if (role.equals(Roles.SENDER))
        {
            Message testMessage = TestUtils.createTestMessageOfSize(session, messageSize);

            for (int i = 0; i < this.numMessages; i++)
            {
                producer.send(testMessage);

                // Increment the message count.
                messageCount++;
            }
        }
    }

    /**
     * Gets a report on the actions performed by the test case in its assigned role.
     *
     * @param session The controlSession to create the report message in.
     *
     * @return The report message.
     *
     * @throws JMSException Any JMSExceptions resulting from creating the report are allowed to fall through.
     */
    public Message getReport(Session session) throws JMSException
    {
        log.debug("public Message getReport(Session controlSession): called");

        // Close the test connection.
        connection.close();

        // Generate a report message containing the count of the number of messages passed.
        Message report = session.createMessage();
        report.setStringProperty("CONTROL_TYPE", "REPORT");
        report.setIntProperty("MESSAGE_COUNT", messageCount);

        return report;
    }

    /**
     * Counts incoming test messages.
     *
     * @param message The incoming test message.
     */
    public void onMessage(Message message)
    {
        log.debug("public void onMessage(Message message = " + message + "): called");

        // Increment the message count.
        messageCount++;
    }
}
