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

import junit.framework.Test;
import junit.framework.TestResult;

import org.apache.log4j.Logger;

import org.apache.qpid.test.framework.DropInTest;
import org.apache.qpid.test.framework.FrameworkBaseCase;
import org.apache.qpid.test.framework.TestClientDetails;
import org.apache.qpid.test.framework.sequencers.CircuitFactory;
import org.apache.qpid.test.framework.sequencers.FanOutCircuitFactory;
import org.apache.qpid.test.utils.ConversationFactory;

import org.apache.qpid.junit.extensions.WrappedSuiteTestDecorator;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;

import java.util.Iterator;
import java.util.Set;

/**
 * FanOutTestDecorator is an {@link DistributedTestDecorator} that runs one test client in the sender role, and the remainder
 * in the receivers role. It also has the capability to listen for new test cases joining the test beyond the initial start
 * point. This feature can be usefull when experimenting with adding more load, in the form of more test clients, to assess
 * its impact on a running test.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Execute coordinated test cases. <td> {@link FrameworkBaseCase}
 * <tr><td> Accept test clients joining a running test.
 * </table>
 */
public class FanOutTestDecorator extends DistributedTestDecorator implements MessageListener
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(FanOutTestDecorator.class);

    /** Holds the currently running test case. */
    FrameworkBaseCase currentTest = null;

    /**
     * Creates a wrapped suite test decorator from another one.
     *
     * @param suite               The test suite.
     * @param availableClients    The list of all clients that responded to the compulsory invite.
     * @param controlConversation The conversation helper for the control level, test coordination conversation.
     * @param controlConnection   The connection that the coordination messages are sent over.
     */
    public FanOutTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> availableClients,
        ConversationFactory controlConversation, Connection controlConnection)
    {
        super(suite, availableClients, controlConversation, controlConnection);

        log.debug("public DistributedTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> allClients = "
            + availableClients + ", ConversationHelper controlConversation = " + controlConversation + "): called");

        testSuite = suite;
        allClients = availableClients;
        conversationFactory = controlConversation;
        connection = controlConnection;

        // Sign available clients up to the test.
        for (Test test : getAllUnderlyingTests())
        {
            FrameworkBaseCase coordTest = (FrameworkBaseCase) test;

            // Get all of the clients able to participate in the test.
            Set<TestClientDetails> enlists = signupClients(coordTest);

            // Check that there were some clients available.
            if (enlists.size() == 0)
            {
                throw new RuntimeException("No clients to test with");
            }

            // Create a distributed test circuit factory for the test.
            CircuitFactory circuitFactory = getTestSequencer();

            // Set up the first client in the sender role, and the remainder in the receivers role.
            Iterator<TestClientDetails> clients = enlists.iterator();
            circuitFactory.setSender(clients.next());

            while (clients.hasNext())
            {
                // Set the sending and receiving client details on the test case.
                circuitFactory.setReceiver(clients.next());
            }

            // Pass down the connection to hold the coordinating conversation over.
            circuitFactory.setConversationFactory(conversationFactory);

            // If the current test case is a drop-in test, set it up as the currently running test for late joiners to
            // add in to. Otherwise the current test field is set to null, to indicate that late joiners are not allowed.
            currentTest = (coordTest instanceof DropInTest) ? coordTest : null;

            // Execute the test case.
            coordTest.setCircuitFactory(circuitFactory);
        }
    }

    /**
     * Broadcasts a test invitation and accepts enlists from participating clients. The wrapped test cases are run
     * with one test client in the sender role, and the remaining test clients in the receiving role.
     *
     * <p/>Any JMSExceptions during the invite/enlist conversation will be allowed to fall through as runtime
     * exceptions, resulting in the non-completion of the test run.
     *
     * @param testResult The the results object to monitor the test results with.
     *
     * @todo Better error recovery for failure of the invite/enlist conversation could be added.
     */
    public void run(TestResult testResult)
    {
        log.debug("public void run(TestResult testResult): called");

        // Listen for late joiners on the control topic.
        try
        {
            conversationFactory.getSession().createConsumer(controlTopic).setMessageListener(this);
        }
        catch (JMSException e)
        {
            throw new RuntimeException("Unable to set up the message listener on the control topic.", e);
        }

        // Run all of the test cases in the test suite.
        /*for (Test test : getAllUnderlyingTests())
        {
            FrameworkBaseCase coordTest = (FrameworkBaseCase) test;

            // Get all of the clients able to participate in the test.
            Set<TestClientDetails> enlists = signupClients(coordTest);

            // Check that there were some clients available.
            if (enlists.size() == 0)
            {
                throw new RuntimeException("No clients to test with");
            }

            // Create a distributed test circuit factory for the test.
            CircuitFactory circuitFactory = getTestSequencer();

            // Set up the first client in the sender role, and the remainder in the receivers role.
            Iterator<TestClientDetails> clients = enlists.iterator();
            circuitFactory.setSender(clients.next());

            while (clients.hasNext())
            {
                // Set the sending and receiving client details on the test case.
                circuitFactory.setReceiver(clients.next());
            }

            // Pass down the connection to hold the coordinating conversation over.
            circuitFactory.setConversationFactory(conversationFactory);

            // If the current test case is a drop-in test, set it up as the currently running test for late joiners to
            // add in to. Otherwise the current test field is set to null, to indicate that late joiners are not allowed.
            currentTest = (coordTest instanceof DropInTest) ? coordTest : null;

            // Execute the test case.
            coordTest.setCircuitFactory(circuitFactory);
        }*/

        // Run all of the test cases in the test suite.
        for (Test test : getAllUnderlyingTests())
        {
            FrameworkBaseCase coordTest = (FrameworkBaseCase) test;

            coordTest.run(testResult);

            currentTest = null;
        }
    }

    /**
     * Should provide the distributed test sequencer to pass to {@link org.apache.qpid.test.framework.FrameworkBaseCase}
     * tests.
     *
     * @return A distributed test sequencer.
     */
    public CircuitFactory getTestSequencer()
    {
        return new FanOutCircuitFactory();
    }

    /**
     * Listens to incoming messages on the control topic. If the messages are 'join' messages, signalling a new
     * test client wishing to join the current test, then the new client will be added to the current test in the
     * receivers role.
     *
     * @param message The incoming control message.
     */
    public void onMessage(Message message)
    {
        try
        {
            // Check if the message is from a test client attempting to join a running test, and join it to the current
            // test case if so.
            if (message.getStringProperty("CONTROL_TYPE").equals("JOIN") && (currentTest != null))
            {
                ((DropInTest) currentTest).lateJoin(message);
            }
        }
        // There is not a lot can be done with this error, so it is deliberately ignored.
        catch (JMSException e)
        {
            log.debug("Unable to process message:" + message);
        }
    }

    /**
     * Prints a string summarizing this test decorator, mainly for debugging purposes.
     *
     * @return String representation for debugging purposes.
     */
    public String toString()
    {
        return "FanOutTestDecorator: [ testSuite = " + testSuite + " ]";
    }
}
