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
package org.apache.qpid.test.framework.sequencers;

import org.apache.log4j.Logger;

import org.apache.qpid.test.framework.Assertion;
import org.apache.qpid.test.framework.Circuit;
import org.apache.qpid.test.framework.TestClientDetails;
import org.apache.qpid.test.framework.TestUtils;
import org.apache.qpid.test.framework.distributedcircuit.DistributedCircuitImpl;
import org.apache.qpid.test.utils.ConversationFactory;

import org.apache.qpid.junit.extensions.util.ParsedProperties;

import javax.jms.*;

import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

/**
 * FanOutCircuitFactory is a circuit factory that creates distributed test circuits. Given a set of participating
 * test client nodes, it assigns one node to the SENDER role and the remainder to the RECEIVER role.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Create distributed circuits from one to many test nodes, for fanout style testing.
 * </table>
 *
 * @todo Adapt this to be an n*m topology circuit factory. Need to add circuit topology definitions to the test
 *       parameters. Place n senders onto the available test clients, and m receivers. Where n or m is larger than
 *       the available nodes, start stacking multiple test clients on each node. There will also be an option that
 *       indicates whether nodes can play both roles, and how many nodes out of all available may be assigned to
 *       each role.
 *
 * @todo The createCircuit methods on this and InteropCircuitFactory are going to be identical. This is because the
 *       partitioning into senders and receivers is already done by the test decorators. Either eliminate these factories
 *       as unnesesary, or move the partitioning functionality into the factories, in which case the test decorators
 *       can probably be merged or eliminated. There is confusion over the placement of responsibilities between the
 *       factories and the test decorators... although the test decorators may well do more than just circuit creation
 *       in the future. For example, there may have to be a special decorator for test repetition that does one circuit
 *       creation, but the runs many tests over it, in which case the handling of responsibilities becomes clearer.
 */
public class FanOutCircuitFactory extends BaseCircuitFactory
{
    /** Used for debugging. */
    Logger log = Logger.getLogger(FanOutCircuitFactory.class);

    /**
     * Creates a test circuit for the test, configered by the test parameters specified.
     *
     * @param testProperties The test parameters.
     * @return A test circuit.
     */
    public Circuit createCircuit(Connection connection, ParsedProperties testProperties)
    {
        log.debug("public Circuit createCircuit(ParsedProperties testProperties): called");

        List<TestClientDetails> senders = new LinkedList<TestClientDetails>();
        senders.add(getSender());
        List<TestClientDetails> receivers = getReceivers();
        ConversationFactory conversationFactory = getConversationFactory();

        return DistributedCircuitImpl.createCircuit(testProperties, senders, receivers, conversationFactory);
    }

    /**
     * Holds a test coordinating conversation with the test clients. This should consist of assigning the test roles,
     * begining the test, gathering the test reports from the participants, and checking for assertion failures against
     * the test reports.
     *
     * @param testCircuit    The test circuit.
     * @param assertions     The list of assertions to apply to the test circuit.
     * @param testProperties The test case definition.
     *
     * @deprecated Scheduled for removal once existing tests converted over to use test circuits.
     */
    public void sequenceTest(Circuit testCircuit, List<Assertion> assertions, Properties testProperties)
    {
        log.debug("protected Message[] sequenceTest(Object... testProperties = " + testProperties + "): called");

        TestClientDetails sender = getSender();
        List<TestClientDetails> receivers = getReceivers();
        ConversationFactory conversationFactory = getConversationFactory();

        try
        {
            // Create a conversation on the sender clients private control route.
            Session session = conversationFactory.getSession();
            Destination senderControlTopic = session.createTopic(sender.privateControlKey);
            ConversationFactory.Conversation senderConversation = conversationFactory.startConversation();

            // Assign the sender role to the sending test client.
            Message assignSender = conversationFactory.getSession().createMessage();
            TestUtils.setPropertiesOnMessage(assignSender, testProperties);
            assignSender.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
            assignSender.setStringProperty("ROLE", "SENDER");
            assignSender.setStringProperty("CLIENT_NAME", "Sustained_SENDER");

            senderConversation.send(senderControlTopic, assignSender);

            // Wait for the sender to confirm its role.
            senderConversation.receive();

            // Assign the receivers roles.
            for (TestClientDetails receiver : receivers)
            {
                assignReceiverRole(receiver, testProperties, true);
            }

            // Start the test on the sender.
            Message start = session.createMessage();
            start.setStringProperty("CONTROL_TYPE", "START");

            senderConversation.send(senderControlTopic, start);

            // Wait for the test sender to return its report.
            Message senderReport = senderConversation.receive();
            TestUtils.pause(500);

            // Ask the receivers for their reports.
            Message statusRequest = session.createMessage();
            statusRequest.setStringProperty("CONTROL_TYPE", "STATUS_REQUEST");

            // Gather the reports from all of the receiving clients.

            // Return all of the test reports, the senders report first.
            // return new Message[] { senderReport };
        }
        catch (JMSException e)
        {
            throw new RuntimeException("Unhandled JMSException.");
        }
    }

    /**
     * Assigns the receivers role to the specified test client that is to act as a receivers during the test. This method
     * does not always wait for the receiving clients to confirm their role assignments. This is because this method
     * may be called from an 'onMessage' method, when a client is joining the test at a later point in time, and it
     * is not possible to do a synchronous receive during an 'onMessage' method. There is a flag to indicate whether
     * or not to wait for role confirmations.
     *
     * @param receiver       The test client to assign the receivers role to.
     * @param testProperties The test parameters.
     * @param confirm        Indicates whether role confirmation should be waited for.
     *
     * @throws JMSException Any JMSExceptions occurring during the conversation are allowed to fall through.
     *
     * @deprecated Scheduled for removal once existing tests converted over to use test circuits.
     */
    protected void assignReceiverRole(TestClientDetails receiver, Properties testProperties, boolean confirm)
        throws JMSException
    {
        log.info("assignReceiverRole(TestClientDetails receivers = " + receiver + ", Map<String, Object> testProperties = "
            + testProperties + "): called");

        ConversationFactory conversationFactory = getConversationFactory();

        // Create a conversation with the receiving test client.
        Session session = conversationFactory.getSession();
        Destination receiverControlTopic = session.createTopic(receiver.privateControlKey);
        ConversationFactory.Conversation receiverConversation = conversationFactory.startConversation();

        // Assign the receivers role to the receiving client.
        Message assignReceiver = session.createMessage();
        TestUtils.setPropertiesOnMessage(assignReceiver, testProperties);
        assignReceiver.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
        assignReceiver.setStringProperty("ROLE", "RECEIVER");
        assignReceiver.setStringProperty("CLIENT_NAME", receiver.clientName);

        receiverConversation.send(receiverControlTopic, assignReceiver);

        // Wait for the role confirmation to come back.
        if (confirm)
        {
            receiverConversation.receive();
        }
    }
}
