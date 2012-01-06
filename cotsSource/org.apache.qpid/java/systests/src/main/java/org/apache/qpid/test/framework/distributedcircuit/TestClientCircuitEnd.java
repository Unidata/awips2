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
package org.apache.qpid.test.framework.distributedcircuit;

import org.apache.log4j.Logger;

import org.apache.qpid.test.framework.*;
import org.apache.qpid.test.framework.distributedtesting.TestClientControlledTest;

import org.apache.qpid.junit.extensions.util.ParsedProperties;
import org.apache.qpid.junit.extensions.util.TestContextProperties;

import javax.jms.*;

/**
 * A TestClientCircuitEnd is a {@link CircuitEnd} that may be controlled from a
 * {@link org.apache.qpid.test.framework.distributedtesting.TestClient}, and that forms a single publishing or
 * receiving end point in a distributed test {@link org.apache.qpid.test.framework.Circuit}.
 *
 * <p/>When operating in the SENDER role, this circuit end is capable of acting as part of the default circuit test
 * procedure (described in the class comment for {@link org.apache.qpid.test.framework.Circuit}). That is, it will
 * send the number of test messages required, using the test configuration parameters given in the test invite, and
 * return a report on its activities to the circuit controller.
 *
 * <p/>When operation in the RECEIVER role, this circuit end acts as part of the default circuit test procedure. It will
 * receive test messages, on the setup specified in the test configuration parameters, and keep count of the messages
 * received, and time taken to receive them. When requested by the circuit controller to provide a report, it will
 * return this report of its activities.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Provide a message producer for sending messages.
 *     <td> {@link CircuitEnd}, {@link LocalCircuitFactory}, {@link TestUtils}
 * <tr><td> Provide a message consumer for receiving messages.
 *     <td> {@link CircuitEnd}, {@link LocalCircuitFactory}, {@link TestUtils}
 * <tr><td> Supply the name of the test case that this implements.
 * <tr><td> Accept/Reject invites based on test parameters. <td> {@link MessagingTestConfigProperties}
 * <tr><td> Adapt to assigned roles. <td> {@link TestClientControlledTest.Roles}
 * <tr><td> Perform test case actions. <td> {@link MessageMonitor}
 * <tr><td> Generate test reports. <td> {@link MessageMonitor}
 * </table>
 */
public class TestClientCircuitEnd implements CircuitEnd, TestClientControlledTest
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(TestClientCircuitEnd.class);

    /** Holds the test parameters. */
    ParsedProperties testProps;

    /** The number of test messages to send. */
    private int numMessages;

    /** The role to be played by the test. */
    private Roles role;

    /** The connection to send the test messages on. */
    private Connection connection;

    /** Holds the circuit end for this test. */
    CircuitEnd circuitEnd;

    /**
     * Holds a message monitor for this circuit end, either the monitor on the consumer when in RECEIVER more, or
     * a monitor updated on every message sent, when acting as a SENDER.
     */
    MessageMonitor messageMonitor;

    /**
     * Should provide the name of the test case that this class implements. The exact names are defined in the
     * interop testing spec.
     *
     * @return The name of the test case that this implements.
     */
    public String getName()
    {
        return "DEFAULT_CIRCUIT_TEST";
    }

    /**
     * Determines whether the test invite that matched this test case is acceptable.
     *
     * @param inviteMessage The invitation to accept or reject.
     * @return <tt>true</tt> to accept the invitation, <tt>false</tt> to reject it.
     *
     * @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
     */
    public boolean acceptInvite(Message inviteMessage) throws JMSException
    {
        log.debug("public boolean acceptInvite(Message inviteMessage): called");

        // Populate the test parameters from the invitation.
        testProps = TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);

        for (Object key : testProps.keySet())
        {
            String propName = (String) key;

            // If the test parameters is overridden by the invitation, use it instead.
            String inviteValue = inviteMessage.getStringProperty(propName);

            if (inviteValue != null)
            {
                testProps.setProperty(propName, inviteValue);
                log.debug("Test invite supplied override to " + propName + " of " + inviteValue);
            }

        }

        // Accept the invitation.
        return true;
    }

    /**
     * Assigns the role to be played by this test case. The test parameters are fully specified in the
     * assignment message. When this method return the test case will be ready to execute.
     *
     * @param role              The role to be played; sender or receivers.
     * @param assignRoleMessage The role assingment message, contains the full test parameters.
     *
     * @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
     */
    public void assignRole(Roles role, Message assignRoleMessage) throws JMSException
    {
        log.debug("public void assignRole(Roles role, Message assignRoleMessage): called");

        // Take note of the role to be played.
        this.role = role;

        // Extract and retain the test parameters.
        numMessages = 1; // assignRoleMessage.getIntProperty("NUM_MESSAGES");

        // Connect using the test parameters.
        connection = TestUtils.createConnection(testProps);

        // Create a circuit end that matches the assigned role and test parameters.
        LocalCircuitFactory circuitFactory = new LocalCircuitFactory();

        switch (role)
        {
        // Check if the sender role is being assigned, and set up a message producer if so.
        case SENDER:

            // Set up the publisher.
            circuitEnd = circuitFactory.createPublisherCircuitEnd(connection, testProps, 0L);

            // Create a custom message monitor that will be updated on every message sent.
            messageMonitor = new MessageMonitor();

            break;

        // Otherwise the receivers role is being assigned, so set this up to listen for messages.
        case RECEIVER:

            // Set up the receiver.
            circuitEnd = circuitFactory.createReceiverCircuitEnd(connection, testProps, 0L);

            // Use the message monitor from the consumer for stats.
            messageMonitor = getMessageMonitor();

            break;
        }

        // Reset all messaging stats for the report.
        messageMonitor.reset();

        connection.start();
    }

    /**
     * Performs the test case actions. Returning from here, indicates that the sending role has completed its test.
     *
     * @param numMessages The number of test messages to send.
     *
     * @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
     *
     * @todo Add round robin on destinations where multiple destinations being used.
     *
     * @todo Add rate limiting when rate limit specified on publishers.
     *
     * @todo Add Max pending message size protection. The receiver will have to send back some acks once in a while,
     *       to notify the publisher that its messages are being consumed. This makes the safety valve harder to
     *       implement than in the single VM case. For example, if the limit is 1000 messages, might want to get back
     *       an ack every 500, to notify the publisher that it can keep sending. What about pub/sub tests? Will it be
     *       necessary to wait for an ack from every receiver? This will have the effect of rate limiting to slow
     *       consumers too.
     *
     * @todo Add commits on every commit batch size boundary.
     */
    public void start(int numMessages) throws JMSException
    {
        log.debug("public void start(): called");

        // If in the SENDER role, send the specified number of test messages to the circuit destinations.
        if (role.equals(Roles.SENDER))
        {
            Message testMessage = getSession().createMessage();

            for (int i = 0; i < numMessages; i++)
            {
                getProducer().send(testMessage);

                // Increment the message count and timings.
                messageMonitor.onMessage(testMessage);
            }
        }
    }

    /**
     * Gets a report on the actions performed by the test case in its assigned role.
     *
     * @param session The controlSession to create the report message in.
     * @return The report message.
     *
     * @throws JMSException Any JMSExceptions resulting from creating the report are allowed to fall through.
     */
    public Message getReport(Session session) throws JMSException
    {
        Message report = session.createMessage();
        report.setStringProperty("CONTROL_TYPE", "REPORT");

        // Add the count of messages sent/received to the report.
        report.setIntProperty("MESSAGE_COUNT", messageMonitor.getNumMessage());

        // Add the time to send/receive messages to the report.
        report.setLongProperty("TEST_TIME", messageMonitor.getTime());

        // Add any exceptions detected to the report.

        return report;
    }

    /**
     * Gets the message producer at this circuit end point.
     *
     * @return The message producer at with this circuit end point.
     */
    public MessageProducer getProducer()
    {
        return circuitEnd.getProducer();
    }

    /**
     * Gets the message consumer at this circuit end point.
     *
     * @return The message consumer at this circuit end point.
     */
    public MessageConsumer getConsumer()
    {
        return circuitEnd.getConsumer();
    }

    /**
     * Send the specified message over the producer at this end point.
     *
     * @param message The message to send.
     *
     * @throws JMSException Any JMS exception occuring during the send is allowed to fall through.
     */
    public void send(Message message) throws JMSException
    {
        // Send the message on the circuit ends producer.
        circuitEnd.send(message);
    }

    /**
     * Gets the JMS Session associated with this circuit end point.
     *
     * @return The JMS Session associated with this circuit end point.
     */
    public Session getSession()
    {
        return circuitEnd.getSession();
    }

    /**
     * Closes the message producers and consumers and the sessions, associated with this circuit end point.
     *
     * @throws JMSException Any JMSExceptions occurring during the close are allowed to fall through.
     */
    public void close() throws JMSException
    {
        // Close the producer and consumer.
        circuitEnd.close();
    }

    /**
     * Returns the message monitor for reporting on received messages on this circuit end.
     *
     * @return The message monitor for this circuit end.
     */
    public MessageMonitor getMessageMonitor()
    {
        return circuitEnd.getMessageMonitor();
    }

    /**
     * Returns the exception monitor for reporting on exceptions received on this circuit end.
     *
     * @return The exception monitor for this circuit end.
     */
    public ExceptionMonitor getExceptionMonitor()
    {
        return circuitEnd.getExceptionMonitor();
    }
}
