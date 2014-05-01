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
 * InteropCircuitFactory is a circuit factory that creates distributed test circuits. Given a set of participating
 * test client nodes, it assigns one node to the SENDER role and one the RECEIVER role.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Create distributed circuits from pairs of test nodes, for interop style testing.
 * </table>
 *
 * @todo The partitioning of a set of nodes into sender and receiver roles is actually done by the interop test
 *       decorator. See the todo comment in FanOutCircuitFactory about merging the factories with the decorators, or
 *       more carefully dividing up responsibilities between them.
 *
 * @todo The squenceTest code is deprecated, but currently still used by the interop tests. It will be removed once it
 *       have been fully replaced by the default test procedure.
 */
public class InteropCircuitFactory extends BaseCircuitFactory
{
    /** Used for debugging. */
    Logger log = Logger.getLogger(InteropCircuitFactory.class);

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
     */
    public void sequenceTest(Circuit testCircuit, List<Assertion> assertions, Properties testProperties)
    {
        log.debug("protected Message[] sequenceTest(Object... testProperties = " + testProperties + "): called");

        TestClientDetails sender = getSender();
        List<TestClientDetails> receivers = getReceivers();
        ConversationFactory conversationFactory = getConversationFactory();

        try
        {
            Session session = conversationFactory.getSession();
            Destination senderControlTopic = session.createTopic(sender.privateControlKey);
            Destination receiverControlTopic = session.createTopic(receivers.get(0).privateControlKey);

            ConversationFactory.Conversation senderConversation = conversationFactory.startConversation();
            ConversationFactory.Conversation receiverConversation = conversationFactory.startConversation();

            Message assignSender = conversationFactory.getSession().createMessage();
            TestUtils.setPropertiesOnMessage(assignSender, testProperties);
            assignSender.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
            assignSender.setStringProperty("ROLE", "SENDER");

            senderConversation.send(senderControlTopic, assignSender);

            // Assign the receivers role the receiving client.
            Message assignReceiver = session.createMessage();
            TestUtils.setPropertiesOnMessage(assignReceiver, testProperties);
            assignReceiver.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
            assignReceiver.setStringProperty("ROLE", "RECEIVER");

            receiverConversation.send(receiverControlTopic, assignReceiver);

            // Wait for the senders and receivers to confirm their roles.
            senderConversation.receive();
            receiverConversation.receive();

            // Start the test.
            Message start = session.createMessage();
            start.setStringProperty("CONTROL_TYPE", "START");

            senderConversation.send(senderControlTopic, start);

            // Wait for the test sender to return its report.
            Message senderReport = senderConversation.receive();
            TestUtils.pause(500);

            // Ask the receivers for its report.
            Message statusRequest = session.createMessage();
            statusRequest.setStringProperty("CONTROL_TYPE", "STATUS_REQUEST");

            receiverConversation.send(receiverControlTopic, statusRequest);

            // Wait for the receivers to send its report.
            Message receiverReport = receiverConversation.receive();

            // return new Message[] { senderReport, receiverReport };

            // Apply assertions.
        }
        catch (JMSException e)
        {
            throw new RuntimeException("JMSException not handled.");
        }
    }
}
