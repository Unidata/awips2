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

import org.apache.qpid.client.AMQSession;
import org.apache.qpid.test.framework.localcircuit.LocalAMQPPublisherImpl;
import org.apache.qpid.test.framework.localcircuit.LocalPublisherImpl;

import org.apache.qpid.junit.extensions.util.ParsedProperties;

import javax.jms.*;

/**
 * LocalAMQPCircuitFactory is a test sequencer that creates test circuits with publishing and receiving ends rooted
 * on the same JVM, allowing AMQP/Qpid specific options to be applied to the circuit.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Provide a standard test procedure over a test circuit.
 * <tr><td> Construct test circuits appropriate to a tests context.
 * <tr><td> Construct test circuits the support AMQP specific options.
 * </table>
 */
public class LocalAMQPCircuitFactory extends LocalCircuitFactory
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(LocalAMQPCircuitFactory.class);

    /**
     * Builds a circuit end suitable for the publishing side of a test circuit, from standard test parameters.
     *
     * @param connection The connection to build the circuit end on.
     * @param testProps  The test parameters to configure the circuit end construction.
     * @param uniqueId   A unique number to being numbering destinations from, to make this circuit unique.
     *
     * @return A circuit end suitable for the publishing side of a test circuit.
     *
     * @throws javax.jms.JMSException Any underlying JMSExceptions are allowed to fall through and fail the creation.
     */
    public CircuitEndBase createPublisherCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId)
        throws JMSException
    {
        log.debug(
            "public CircuitEndBase createPublisherCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId = "
            + uniqueId + "): called");

        // Cast the test properties into a typed interface for convenience.
        MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProps);

        Session session = connection.createSession(props.getPublisherTransacted(), props.getAckMode());

        Destination destination =
            props.getPubsub() ? session.createTopic(props.getSendDestinationNameRoot() + "_" + uniqueId)
                              : session.createQueue(props.getSendDestinationNameRoot() + "_" + uniqueId);

        MessageProducer producer =
            props.getPublisherProducerBind()
            ? ((props.getImmediate() | props.getMandatory())
                ? ((AMQSession) session).createProducer(destination, props.getMandatory(), props.getImmediate())
                : session.createProducer(destination)) : null;

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
     * Creates a local {@link Publisher} from a {@link CircuitEnd}. The publisher implementation provides AMQP
     * specific assertion methods, for testing beyond JMS.
     *
     * @param publisherEnd The publishing circuit end.
     *
     * @return A {@link Receiver}.
     */
    protected LocalPublisherImpl createPublisherFromCircuitEnd(CircuitEndBase publisherEnd)
    {
        return new LocalAMQPPublisherImpl(publisherEnd);
    }
}
