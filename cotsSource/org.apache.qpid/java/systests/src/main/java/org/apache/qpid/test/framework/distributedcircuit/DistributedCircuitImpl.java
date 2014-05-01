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
import org.apache.qpid.test.utils.ConversationFactory;

import org.apache.qpid.junit.extensions.TimingController;
import org.apache.qpid.junit.extensions.TimingControllerAware;
import org.apache.qpid.junit.extensions.util.ParsedProperties;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import java.util.LinkedList;
import java.util.List;

/**
 * DistributedCircuitImpl is a distributed implementation of the test {@link Circuit}. Many publishers and receivers
 * accross multiple machines may be combined to form a single test circuit. The test circuit extracts reports from
 * all of its publishers and receivers, and applies its assertions to these reports.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Supply the publishing and receiving ends of a test messaging circuit.
 * <tr><td> Start the circuit running.
 * <tr><td> Close the circuit down.
 * <tr><td> Take a reading of the circuits state.
 * <tr><td> Apply assertions against the circuits state.
 * <tr><td> Send test messages over the circuit.
 * <tr><td> Perform the default test procedue on the circuit.
 * </table>
 *
 * @todo There is a short pause after receiving sender reports before asking for receiver reports, because receivers may
 *       not have finished receiving all their test messages before the report request arrives. This is going to be a
 *       problem for taking test timings and needs to be eliminiated. Suggested solution: have receiver send back reports
 *       asynchronously, on test batch size boundaries, and do so automatically rather than having to have the report
 *       request sent to them. Number each test run, or otherwise uniquely identify it, when a receiver does not get
 *       any more messages on a test run for more than a timeout, it can assume the test is complete and send a final
 *       report. On the coordinator end a future will need to be created to wait for all final reports to come in, and
 *       to register results and timings for the test. This must work in such a way that a new test cycle can be started
 *       without waiting for the results of the old one to come in.
 *
 * @todo Add in setting of timing controller, from timing aware test cases.
 */
public class DistributedCircuitImpl implements Circuit, TimingControllerAware
{
    /** Used for debugging purposes. */
    private static final Logger log = Logger.getLogger(DistributedCircuitImpl.class);

    /** Holds the conversation factory over which to coordinate the test. */
    protected ConversationFactory conversationFactory;

    /** Holds the controlSession over which to hold the control conversation. */
    protected Session controlSession;

    /** Holds the sender nodes in the test circuit. */
    protected List<TestClientDetails> senders;

    /** Holds the receiver nodes in the test circuit. */
    protected List<TestClientDetails> receivers;

    /** Holds the sender control conversations. */
    protected ConversationFactory.Conversation[] senderConversation;

    /** Holds the receiver control conversations. */
    protected ConversationFactory.Conversation[] receiverConversation;

    /** Holds the control topics for the senders in the test circuit. */
    protected Destination[] senderControlTopic;

    /** Holds the control topics for the receivers in the test circuit. */
    protected Destination[] receiverControlTopic;

    /** Holds the number of messages to send per test run. */
    protected int numMessages;

    /**
     * Holds the timing controller for the circuit. This is used to log test times asynchronously, when reciever nodes
     * return their reports after senders have completed a test case.
     */
    TimingController timingController;

    /**
     * Creates a distributed test circuit on the specified senders and receivers.
     *
     * @param session              The controlSession for all control conversations.
     * @param senders              The senders.
     * @param receivers            The receivers.
     * @param senderConversation   A control conversation with the senders.
     * @param receiverConversation A control conversation with the receivers.
     * @param senderControlTopic   The senders control topic.
     * @param receiverControlTopic The receivers control topic.
     */
    protected DistributedCircuitImpl(Session session, List<TestClientDetails> senders, List<TestClientDetails> receivers,
        ConversationFactory.Conversation[] senderConversation, ConversationFactory.Conversation[] receiverConversation,
        Destination[] senderControlTopic, Destination[] receiverControlTopic)
    {
        this.controlSession = session;
        this.senders = senders;
        this.receivers = receivers;
        this.senderConversation = senderConversation;
        this.receiverConversation = receiverConversation;
        this.senderControlTopic = senderControlTopic;
        this.receiverControlTopic = receiverControlTopic;
    }

    /**
     * Creates a distributed test circuit from the specified test parameters, on the senders and receivers
     * given.
     *
     * @param testProps           The test parameters.
     * @param senders             The sender ends in the test circuit.
     * @param receivers           The receiver ends in the test circuit.
     * @param conversationFactory A conversation factory for creating the control conversations with senders and receivers.
     *
     * @return A connected and ready to start, test circuit.
     */
    public static Circuit createCircuit(ParsedProperties testProps, List<TestClientDetails> senders,
        List<TestClientDetails> receivers, ConversationFactory conversationFactory)
    {
        log.debug("public static Circuit createCircuit(ParsedProperties testProps, List<TestClientDetails> senders, "
            + " List<TestClientDetails> receivers, ConversationFactory conversationFactory)");

        try
        {
            Session session = conversationFactory.getSession();

            // Create control conversations with each of the senders.
            ConversationFactory.Conversation[] senderConversation = new ConversationFactory.Conversation[senders.size()];
            Destination[] senderControlTopic = new Destination[senders.size()];

            for (int i = 0; i < senders.size(); i++)
            {
                TestClientDetails sender = senders.get(i);

                senderControlTopic[i] = session.createTopic(sender.privateControlKey);
                senderConversation[i] = conversationFactory.startConversation();
            }

            log.debug("Sender conversations created.");

            // Create control conversations with each of the receivers.
            ConversationFactory.Conversation[] receiverConversation = new ConversationFactory.Conversation[receivers.size()];
            Destination[] receiverControlTopic = new Destination[receivers.size()];

            for (int i = 0; i < receivers.size(); i++)
            {
                TestClientDetails receiver = receivers.get(i);

                receiverControlTopic[i] = session.createTopic(receiver.privateControlKey);
                receiverConversation[i] = conversationFactory.startConversation();
            }

            log.debug("Receiver conversations created.");

            // Assign the sender role to each of the sending test clients.
            for (int i = 0; i < senders.size(); i++)
            {
                TestClientDetails sender = senders.get(i);

                Message assignSender = conversationFactory.getSession().createMessage();
                TestUtils.setPropertiesOnMessage(assignSender, testProps);
                assignSender.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
                assignSender.setStringProperty("ROLE", "SENDER");

                senderConversation[i].send(senderControlTopic[i], assignSender);
            }

            log.debug("Sender role assignments sent.");

            // Assign the receivers role to each of the receiving test clients.
            for (int i = 0; i < receivers.size(); i++)
            {
                TestClientDetails receiver = receivers.get(i);

                Message assignReceiver = session.createMessage();
                TestUtils.setPropertiesOnMessage(assignReceiver, testProps);
                assignReceiver.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
                assignReceiver.setStringProperty("ROLE", "RECEIVER");

                receiverConversation[i].send(receiverControlTopic[i], assignReceiver);
            }

            log.debug("Receiver role assignments sent.");

            // Wait for the senders and receivers to confirm their roles.
            for (int i = 0; i < senders.size(); i++)
            {
                senderConversation[i].receive();
            }

            log.debug("Got all sender role confirmations");

            for (int i = 0; i < receivers.size(); i++)
            {
                receiverConversation[i].receive();
            }

            log.debug("Got all receiver role confirmations");

            // Package everything up as a circuit.
            return new DistributedCircuitImpl(session, senders, receivers, senderConversation, receiverConversation,
                    senderControlTopic, receiverControlTopic);
        }
        catch (JMSException e)
        {
            throw new RuntimeException("JMSException not handled.");
        }
    }

    /**
     * Used by tests cases that can supply a {@link org.apache.qpid.junit.extensions.TimingController} to set the
     * controller on an aware test.
     *
     * @param controller The timing controller.
     */
    public void setTimingController(TimingController controller)
    {
        this.timingController = controller;
    }

    /**
     * Gets the interface on the publishing end of the circuit.
     *
     * @return The publishing end of the circuit.
     */
    public Publisher getPublisher()
    {
        throw new RuntimeException("Not Implemented.");
    }

    /**
     * Gets the interface on the receiving end of the circuit.
     *
     * @return The receiving end of the circuit.
     */
    public Receiver getReceiver()
    {
        throw new RuntimeException("Not Implemented.");
    }

    /**
     * Connects and starts the circuit. After this method is called the circuit is ready to send messages.
     */
    public void start()
    {
        log.debug("public void start(): called");

        try
        {
            // Start the test on each of the senders.
            Message start = controlSession.createMessage();
            start.setStringProperty("CONTROL_TYPE", "START");
            start.setIntProperty("MESSAGE_COUNT", numMessages);

            for (int i = 0; i < senders.size(); i++)
            {
                senderConversation[i].send(senderControlTopic[i], start);
            }

            log.debug("All senders told to start their tests.");
        }
        catch (JMSException e)
        {
            throw new RuntimeException("Unhandled JMSException.", e);
        }
    }

    /**
     * Checks the test circuit. The effect of this is to gather the circuits state, for both ends of the circuit,
     * into a report, against which assertions may be checked.
     *
     * @todo Replace the asynch receiver report thread with a choice of direct or asynch executor, so that asynch
     *       or synch logging of test timings is optional. Also need to provide an onMessage method that is capable
     *       of receiving timing reports that receivers will generate during an ongoing test, on the test sample
     *       size boundaries. The message timing logging code should be factored out as a common method that can
     *       be called in response to the final report responses, or the onMessage method. Another alternative is
     *       to abandon the final report request altogether and just use the onMessage method? I think the two
     *       differ though, as the final report is used to apply assertions, and the ongoing report is just for
     *       periodic timing results... In which case, maybe there needs to be a way for the onMessage method
     *       to process just some of the incoming messages, and forward the rest on to the conversion helper, as
     *       a sort of pre-conversation helper filter? Make conversation expose its onMessage method (it should
     *       already) and allow another delivery thread to filter the incoming messages to the conversation.
     */
    public void check()
    {
        log.debug("public void check(): called");

        try
        {
            // Wait for all the test senders to return their reports.
            for (int i = 0; i < senders.size(); i++)
            {
                Message senderReport = senderConversation[i].receive();
                log.debug("Sender " + senderReport.getStringProperty("CLIENT_NAME") + " reports message count: "
                    + senderReport.getIntProperty("MESSAGE_COUNT"));
                log.debug("Sender " + senderReport.getStringProperty("CLIENT_NAME") + " reports message time: "
                    + senderReport.getLongProperty("TEST_TIME"));
            }

            log.debug("Got all sender test reports.");

            // Apply sender assertions to pass/fail the tests.

            // Inject a short pause to give the receivers time to finish receiving their test messages.
            TestUtils.pause(500);

            // Ask the receivers for their reports.
            Message statusRequest = controlSession.createMessage();
            statusRequest.setStringProperty("CONTROL_TYPE", "STATUS_REQUEST");

            for (int i = 0; i < receivers.size(); i++)
            {
                receiverConversation[i].send(receiverControlTopic[i], statusRequest);
            }

            log.debug("All receiver test reports requested.");

            // Wait for all receiver reports to come in, but do so asynchronously.
            Runnable gatherAllReceiverReports =
                new Runnable()
                {
                    public void run()
                    {
                        try
                        {
                            // Wait for all the receivers to send their reports.
                            for (int i = 0; i < receivers.size(); i++)
                            {
                                Message receiverReport = receiverConversation[i].receive();

                                String clientName = receiverReport.getStringProperty("CLIENT_NAME");
                                int messageCount = receiverReport.getIntProperty("MESSAGE_COUNT");
                                long testTime = receiverReport.getLongProperty("TEST_TIME");

                                log.debug("Receiver " + clientName + " reports message count: " + messageCount);
                                log.debug("Receiver " + receiverReport.getStringProperty("CLIENT_NAME")
                                    + " reports message time: " + testTime);

                                // Apply receiver assertions to pass/fail the tests.

                                // Log the test timings on the asynchronous test timing controller.
                                /*try
                                {
                                    timingController.completeTest(true, messageCount, testTime);
                                }
                                // The timing controll can throw InterruptedException is the current test is to be
                                // interrupted.
                                catch (InterruptedException e)
                                {
                                    e.printStackTrace();
                                }*/
                            }

                            log.debug("All receiver test reports received.");
                        }
                        catch (JMSException e)
                        {
                            throw new RuntimeException(e);
                        }
                    }
                };

            Thread receiverReportsThread = new Thread(gatherAllReceiverReports);
            receiverReportsThread.start();

            // return new Message[] { senderReport, receiverReport };

        }
        catch (JMSException e)
        {
            throw new RuntimeException("Unhandled JMSException.", e);
        }
    }

    /**
     * Closes the circuit. All associated resources are closed.
     */
    public void close()
    {
        log.debug("public void close(): called");

        // End the current test on all senders and receivers.
    }

    /**
     * Applies a list of assertions against the test circuit. The {@link #check()} method should be called before doing
     * this, to ensure that the circuit has gathered its state into a report to assert against.
     *
     * @param assertions The list of assertions to apply.
     *
     * @return Any assertions that failed.
     */
    public List<Assertion> applyAssertions(List<Assertion> assertions)
    {
        log.debug("public List<Assertion> applyAssertions(List<Assertion> assertions = " + assertions + "): called");

        List<Assertion> failures = new LinkedList<Assertion>();

        for (Assertion assertion : assertions)
        {
            if (!assertion.apply())
            {
                failures.add(assertion);
            }
        }

        return failures;
    }

    /**
     * Runs the default test procedure against the circuit, and checks that all of the specified assertions hold.
     *
     * @param numMessages The number of messages to send using the default test procedure.
     * @param assertions  The list of assertions to apply.
     *
     * @return Any assertions that failed.
     *
     * @todo From check onwards needs to be handled as a future. The future must call back onto the test case to
     *       report results asynchronously.
     */
    public List<Assertion> test(int numMessages, List<Assertion> assertions)
    {
        log.debug("public List<Assertion> test(int numMessages = " + numMessages + ", List<Assertion> assertions = "
            + assertions + "): called");

        // Keep the number of messages to send per test run, where the send method can reference it.
        this.numMessages = numMessages;

        // Start the test running on all sender circuit ends.
        start();

        // Request status reports to be handed in.
        check();

        // Assert conditions on the publishing end of the circuit.
        // Assert conditions on the receiving end of the circuit.
        List<Assertion> failures = applyAssertions(assertions);

        // Close the circuit ending the current test case.
        close();

        // Pass with no failed assertions or fail with a list of failed assertions.
        return failures;
    }
}
