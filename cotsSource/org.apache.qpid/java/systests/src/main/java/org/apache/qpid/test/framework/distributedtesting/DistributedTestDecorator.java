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
package org.apache.qpid.test.framework.distributedtesting;

import junit.framework.TestResult;

import org.apache.log4j.Logger;

import org.apache.qpid.test.framework.FrameworkBaseCase;
import org.apache.qpid.test.framework.TestClientDetails;
import org.apache.qpid.test.framework.sequencers.CircuitFactory;
import org.apache.qpid.test.utils.ConversationFactory;

import org.apache.qpid.junit.extensions.WrappedSuiteTestDecorator;

import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;

import java.util.*;

/**
 * DistributedTestDecorator is a base class for writing test decorators that invite test clients to participate in
 * distributed test cases. It provides a helper method, {@link #signupClients}, that broadcasts an invitation and
 * returns the set of test clients that are available to particiapte in the test.
 *
 * <p/>When used to wrap a {@link FrameworkBaseCase} test, it replaces the default {@link CircuitFactory} implementations
 * with a suitable circuit factory for distributed tests. Concrete implementations can use this to configure the sending
 * and receiving roles on the test.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Broadcast test invitations and collect enlists. <td> {@link ConversationFactory}.
 * </table>
 */
public abstract class DistributedTestDecorator extends WrappedSuiteTestDecorator
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(DistributedTestDecorator.class);

    /** Holds the contact information for all test clients that are available and that may take part in the test. */
    Set<TestClientDetails> allClients;

    /** Holds the conversation helper for the control level conversation for coordinating the test through. */
    ConversationFactory conversationFactory;

    /** Holds the connection that the control conversation is held over. */
    Connection connection;

    /** Holds the underlying test suite that this decorator wraps. */
    WrappedSuiteTestDecorator testSuite;

    /** Holds the control topic, on which test invitations are broadcast. */
    protected Destination controlTopic;

    /**
     * Creates a wrapped suite test decorator from another one.
     *
     * @param suite               The test suite.
     * @param availableClients    The list of all clients that responded to the compulsory invite.
     * @param controlConversation The conversation helper for the control level, test coordination conversation.
     * @param controlConnection   The connection that the coordination messages are sent over.
     */
    public DistributedTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> availableClients,
        ConversationFactory controlConversation, Connection controlConnection)
    {
        super(suite);

        log.debug("public DistributedTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> allClients = "
            + availableClients + ", ConversationHelper controlConversation = " + controlConversation + "): called");

        testSuite = suite;
        allClients = availableClients;
        conversationFactory = controlConversation;
        connection = controlConnection;

        // Set up the test control topic.
        try
        {
            controlTopic = conversationFactory.getSession().createTopic("iop.control");
        }
        catch (JMSException e)
        {
            throw new RuntimeException("Unable to create the coordinating control topic to broadcast test invites on.", e);
        }
    }

    /**
     * Should run all of the tests in the wrapped test suite.
     *
     * @param testResult The the results object to monitor the test results with.
     */
    public abstract void run(TestResult testResult);

    /**
     * Should provide the distributed test sequencer to pass to {@link org.apache.qpid.test.framework.FrameworkBaseCase}
     * tests.
     *
     * @return A distributed test sequencer.
     */
    public abstract CircuitFactory getTestSequencer();

    /**
     * Broadcasts an invitation to participate in a coordinating test case to find out what clients are available to
     * run the test case.
     *
     * @param coordTest The coordinating test case to broadcast an inviate for.
     *
     * @return A set of test clients that accepted the invitation.
     */
    protected Set<TestClientDetails> signupClients(FrameworkBaseCase coordTest)
    {
        // Broadcast the invitation to find out what clients are available to test.
        Set<TestClientDetails> enlists;
        try
        {
            Message invite = conversationFactory.getSession().createMessage();

            ConversationFactory.Conversation conversation = conversationFactory.startConversation();

            invite.setStringProperty("CONTROL_TYPE", "INVITE");
            invite.setStringProperty("TEST_NAME", coordTest.getTestCaseNameForTestMethod(coordTest.getName()));

            conversation.send(controlTopic, invite);

            // Wait for a short time, to give test clients an opportunity to reply to the invitation.
            Collection<Message> replies = conversation.receiveAll(allClients.size(), 500);
            enlists = Coordinator.extractEnlists(replies);
        }
        catch (JMSException e)
        {
            throw new RuntimeException("There was a JMSException during the invite/enlist conversation.", e);
        }

        return enlists;
    }

    /**
     * Prints a string summarizing this test decorator, mainly for debugging purposes.
     *
     * @return String representation for debugging purposes.
     */
    public String toString()
    {
        return "DistributedTestDecorator: [ testSuite = " + testSuite + " ]";
    }
}
