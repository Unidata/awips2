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

import org.apache.qpid.test.framework.localcircuit.LocalCircuitImpl;
import org.apache.qpid.test.framework.localcircuit.LocalPublisherImpl;
import org.apache.qpid.test.framework.localcircuit.LocalReceiverImpl;
import org.apache.qpid.test.framework.sequencers.CircuitFactory;
import org.apache.qpid.test.utils.ConversationFactory;

import org.apache.qpid.junit.extensions.util.ParsedProperties;

import javax.jms.*;

import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicLong;

/**
 * LocalCircuitFactory is a circuit factory that creates test circuits with publishing and receiving ends rooted
 * on the same JVM. The ends of the circuit are presented as {@link Publisher} and {@link Receiver} interfaces, which
 * in turn provide methods to apply assertions to the circuit. The creation of the circuit ends, and the presentation
 * of the ends as publisher/receiver interfaces, are designed to be overriden, so that circuits and assertions that
 * use messaging features not available in JMS can be written. This provides an extension point for writing tests
 * against proprietary features of JMS implementations.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Provide a standard test procedure over a test circuit.
 * <tr><td> Construct test circuits appropriate to a tests context.
 * </table>
 */
public class LocalCircuitFactory implements CircuitFactory
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(LocalCircuitFactory.class);

    /** Used to create unique destination names for each test. */
    protected static AtomicLong uniqueDestsId = new AtomicLong();

    /**
     * Holds a test coordinating conversation with the test clients. This should consist of assigning the test roles,
     * begining the test and gathering the test reports from the participants.
     *
     * @param testCircuit    The test circuit.
     * @param assertions     The list of assertions to apply to the test circuit.
     * @param testProperties The test case definition.
     */
    public void sequenceTest(Circuit testCircuit, List<Assertion> assertions, Properties testProperties)
    {
        if (testCircuit != null)
        {
    	    FrameworkBaseCase.assertNoFailures(testCircuit.test(1, assertions));
        }
    }

    /**
     * Creates a test circuit for the test, configered by the test parameters specified.
     *
     * @param testProperties The test parameters.
     *
     * @return A test circuit.
     */
    public Circuit createCircuit(Connection connection, ParsedProperties testProperties)
    {
        Circuit result;

        // Cast the test properties into a typed interface for convenience.
        MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProperties);

        // Create a standard publisher/receivers test client pair on a shared connection, individual sessions.
        try
        {
            // Get a unique offset to append to destination names to make them unique to the connection.
            long uniqueId = uniqueDestsId.incrementAndGet();

            // Add the connection exception listener to assert on exception conditions with.
            // ExceptionMonitor exceptionMonitor = new ExceptionMonitor();
            // connection.setExceptionListener(exceptionMonitor);

            // Set up the publisher.
            CircuitEndBase publisherEnd = createPublisherCircuitEnd(connection, props, uniqueId);

            // Set up the receiver.
            CircuitEndBase receiverEnd = createReceiverCircuitEnd(connection, props, uniqueId);

            // Start listening for incoming messages.
            connection.start();

            // Package everything up.
            LocalPublisherImpl publisher = createPublisherFromCircuitEnd(publisherEnd);
            LocalReceiverImpl receiver = createReceiverFromCircuitEnd(receiverEnd);

            result = new LocalCircuitImpl(testProperties, publisher, receiver, connection, publisher.getExceptionMonitor());
        }
        catch (JMSException e)
        {
            throw new RuntimeException("Could not create publisher/receivers pair due to a JMSException.", e);
        }

        return result;
    }

    /**
     * Creates a local {@link Receiver} from a {@link CircuitEnd}. Sub-classes may override this to provide more
     * specialized receivers if necessary.
     *
     * @param receiverEnd The receiving circuit end.
     *
     * @return A {@link Receiver}.
     */
    protected LocalReceiverImpl createReceiverFromCircuitEnd(CircuitEndBase receiverEnd)
    {
        return new LocalReceiverImpl(receiverEnd);
    }

    /**
     * Creates a local {@link Publisher} from a {@link CircuitEnd}. Sub-classes may override this to provide more
     * specialized receivers if necessary.
     *
     * @param publisherEnd The publishing circuit end.
     *
     * @return A {@link Receiver}.
     */
    protected LocalPublisherImpl createPublisherFromCircuitEnd(CircuitEndBase publisherEnd)
    {
        return new LocalPublisherImpl(publisherEnd);
    }

    /**
     * Builds a circuit end suitable for the publishing side of a test circuit, from standard test parameters.
     *
     * @param connection The connection to build the circuit end on.
     * @param testProps  The test parameters to configure the circuit end construction.
     * @param uniqueId   A unique number to being numbering destinations from, to make this circuit unique.
     *
     * @return A circuit end suitable for the publishing side of a test circuit.
     *
     * @throws JMSException Any underlying JMSExceptions are allowed to fall through and fail the creation.
     */
    public CircuitEndBase createPublisherCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId)
        throws JMSException
    {
        log.debug(
            "public CircuitEndBase createPublisherCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId = "
            + uniqueId + "): called");

        // Cast the test properties into a typed interface for convenience.
        MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProps);

        // Check that the test properties do not contain AMQP/Qpid specific settings, and fail if they do.
        if (props.getImmediate() || props.getMandatory())
        {
            throw new RuntimeException(
                "Cannot create a pure JMS circuit as the test properties require AMQP specific options.");
        }

        Session session = connection.createSession(props.getPublisherTransacted(), props.getAckMode());

        Destination destination =
            props.getPubsub() ? session.createTopic(props.getSendDestinationNameRoot() + "_" + uniqueId)
                              : session.createQueue(props.getSendDestinationNameRoot() + "_" + uniqueId);

        MessageProducer producer = props.getPublisherProducerBind() ? session.createProducer(destination) : null;

        MessageConsumer consumer =
            props.getPublisherConsumerBind()
            ? session.createConsumer(session.createQueue(props.getReceiveDestinationNameRoot() + "_" + uniqueId)) : null;

        MessageMonitor messageMonitor = new MessageMonitor();

        if (consumer != null)
        {
            consumer.setMessageListener(messageMonitor);
        }

        ExceptionMonitor exceptionMonitor = new ExceptionMonitor();
        connection.setExceptionListener(exceptionMonitor);

        if (!props.getPublisherConsumerActive() && (consumer != null))
        {
            consumer.close();
        }

        return new CircuitEndBase(producer, consumer, session, messageMonitor, exceptionMonitor);
    }

    /**
     * Builds a circuit end suitable for the receiving side of a test circuit, from standard test parameters.
     *
     * @param connection The connection to build the circuit end on.
     * @param testProps  The test parameters to configure the circuit end construction.
     * @param uniqueId   A unique number to being numbering destinations from, to make this circuit unique.
     *
     * @return A circuit end suitable for the receiving side of a test circuit.
     *
     * @throws JMSException Any underlying JMSExceptions are allowed to fall through and fail the creation.
     */
    public CircuitEndBase createReceiverCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId)
        throws JMSException
    {
        log.debug(
            "public CircuitEndBase createReceiverCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId = "
            + uniqueId + "): called");

        // Cast the test properties into a typed interface for convenience.
        MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProps);

        // Check that the test properties do not contain AMQP/Qpid specific settings, and fail if they do.
        if (props.getImmediate() || props.getMandatory())
        {
            throw new RuntimeException(
                "Cannot create a pure JMS circuit as the test properties require AMQP specific options.");
        }

        Session session = connection.createSession(props.getPublisherTransacted(), props.getAckMode());

        MessageProducer producer =
            props.getReceiverProducerBind()
            ? session.createProducer(session.createQueue(props.getReceiveDestinationNameRoot() + "_" + uniqueId)) : null;

        Destination destination =
            props.getPubsub() ? session.createTopic(props.getSendDestinationNameRoot() + "_" + uniqueId)
                              : session.createQueue(props.getSendDestinationNameRoot() + "_" + uniqueId);

        MessageConsumer consumer =
            props.getReceiverConsumerBind()
            ? ((props.getDurableSubscription() && props.getPubsub())
                ? session.createDurableSubscriber((Topic) destination, "testsub") : session.createConsumer(destination))
            : null;

        MessageMonitor messageMonitor = new MessageMonitor();

        if (consumer != null)
        {
            consumer.setMessageListener(messageMonitor);
        }

        if (!props.getReceiverConsumerActive() && (consumer != null))
        {
            consumer.close();
        }

        return new CircuitEndBase(producer, consumer, session, messageMonitor, null);
    }

    /**
     * Sets the sender test client to coordinate the test with.
     *
     * @param sender The contact details of the sending client in the test.
     */
    public void setSender(TestClientDetails sender)
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
     * Sets the receiving test client to coordinate the test with.
     *
     * @param receiver The contact details of the sending client in the test.
     */
    public void setReceiver(TestClientDetails receiver)
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
     * Supplies the sending test client.
     *
     * @return The sending test client.
     */
    public TestClientDetails getSender()
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
     * Supplies the receiving test client.
     *
     * @return The receiving test client.
     */
    public List<TestClientDetails> getReceivers()
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
     * Accepts the conversation factory over which to hold the test coordinating conversation.
     *
     * @param conversationFactory The conversation factory to coordinate the test over.
     */
    public void setConversationFactory(ConversationFactory conversationFactory)
    {
        throw new RuntimeException("Not implemented.");
    }
}
