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

using Apache.Qpid.Integration.Tests.framework.localcircuit;//.LocalCircuitImpl;
//using Apache.Qpid.Integration.Tests.framework.localcircuit.LocalPublisherImpl;
//using Apache.Qpid.Integration.Tests.framework.localcircuit.LocalReceiverImpl;
//using Apache.Qpid.Integration.Tests.framework.sequencers.CircuitFactory;
//using org.apache.qpid.util.ConversationFactory;

//using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

//using javax.jms.*;

using System.Collections.Generic;//.IList;
//using java.util.Properties;
//using java.util.concurrent.atomic.AtomicLong;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// LocalCircuitFactory is a circuit factory that creates test circuits with publishing and receiving ends rooted
    /// on the same JVM. The ends of the circuit are presented as <see cref="Publisher"/> and <see cref="Receiver"/> interfaces, which
    /// in turn provide methods to apply assertions to the circuit. The creation of the circuit ends, and the presentation
    /// of the ends as publisher/receiver interfaces, are designed to be overriden, so that circuits and assertions that
    /// use messaging features not available in JMS can be written. This provides an extension point for writing tests
    /// against proprietary features of JMS implementations.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide a standard test procedure over a test circuit.
    /// <tr><td> Construct test circuits appropriate to a tests context.
    /// </table>
    /// </summary>
    public class LocalCircuitFactory : CircuitFactory
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(LocalCircuitFactory));

        /// <summary> Used to create unique destination names for each test. </summary>
        protected static AtomicLong uniqueDestsId = new AtomicLong();

        /// <summary>
        /// Holds a test coordinating conversation with the test clients. This should consist of assigning the test roles,
        /// begining the test and gathering the test reports from the participants.
        /// </summary>
        /// <param name="testCircuit">    The test circuit. </param>
        /// <param name="assertions">     The list of assertions to apply to the test circuit. </param>
        /// <param name="testProperties"> The test case definition. </param>
        public void sequenceTest(Circuit testCircuit, IList<Assertion> assertions, Properties testProperties)
        {
            FrameworkBaseCase.assertNoFailures(testCircuit.test(1, assertions));
        }

        /// <summary>
        /// Creates a test circuit for the test, configered by the test parameters specified.
        /// </summary>
        /// <param name="testProperties"> The test parameters. </param>
        ///
        /// <return> A test circuit. </return>
        public Circuit createCircuit(TestModel testProperties)
        {
            Circuit result;

            // Create a standard publisher/receivers test client pair on a shared connection, individual sessions.
            try
            {
                // Get a unique offset to append to destination names to make them unique to the connection.
                long uniqueId = uniqueDestsId.incrementAndGet();

                // Set up the connection.
                Connection connection = TestUtils.createConnection(testProperties);

                // Add the connection exception listener to assert on exception conditions with.
                // ExceptionMonitor exceptionMonitor = new ExceptionMonitor();
                // connection.setExceptionListener(exceptionMonitor);

                // Set up the publisher.
                CircuitEndBase publisherEnd = createPublisherCircuitEnd(connection, testProps, uniqueId);

                // Set up the receiver.
                CircuitEndBase receiverEnd = createReceiverCircuitEnd(connection, testProps, uniqueId);

                // Start listening for incoming messages.
                connection.start();

                // Namespace everything up.
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

        /// <summary>
        /// Creates a local <see cref="Receiver"/> from a <see cref="CircuitEnd"/>. Sub-classes may override this to provide more
        /// specialized receivers if necessary.
        /// </summary>
        /// <param name="receiverEnd"> The receiving circuit end. </param>
        ///
        /// <return> A <see cref="Receiver"/>. </return>
        protected LocalReceiverImpl createReceiverFromCircuitEnd(CircuitEndBase receiverEnd)
        {
            return new LocalReceiverImpl(receiverEnd);
        }

        /// <summary>
        /// Creates a local <see cref="Publisher"/> from a <see cref="CircuitEnd"/>. Sub-classes may override this to provide more
        /// specialized receivers if necessary.
        /// </summary>
        /// <param name="publisherEnd"> The publishing circuit end. </param>
        ///
        /// <return> A <see cref="Receiver"/>. </return>
        protected LocalPublisherImpl createPublisherFromCircuitEnd(CircuitEndBase publisherEnd)
        {
            return new LocalPublisherImpl(publisherEnd);
        }

        /// <summary>
        /// Builds a circuit end suitable for the publishing side of a test circuit, from standard test parameters.
        /// </summary>
        /// <param name="connection"> The connection to build the circuit end on. </param>
        /// <param name="testProps">  The test parameters to configure the circuit end construction. </param>
        /// <param name="uniqueId">   A unique number to being numbering destinations from, to make this circuit unique. </param>
        ///
        /// <return> A circuit end suitable for the publishing side of a test circuit. </return>
        ///
        /// <exception cref="JMSException"> Any underlying JMSExceptions are allowed to fall through and fail the creation. </exception>
        public CircuitEndBase createPublisherCircuitEnd(Connection connection, TestModel testProps, long uniqueId)
            throws JMSException
        {
            log.debug(
                      "public CircuitEndBase createPublisherCircuitEnd(Connection connection, TestModel testProps, long uniqueId = "
                      + uniqueId + "): called");

            // Check that the test properties do not contain AMQP/Qpid specific settings, and fail if they do.
            if (testProps.getImmediate() || testProps.getMandatory())
            {
                throw new RuntimeException(
                                           "Cannot create a pure JMS circuit as the test properties require AMQP specific options.");
            }

            Session session = connection.createSession(testProps.getPublisherTransacted(), testProps.getAckMode());

            Destination destination =
                testProps.getPubsub() ? session.createTopic(testProps.getSendDestinationNameRoot() + "_" + uniqueId)
                : session.createQueue(testProps.getSendDestinationNameRoot() + "_" + uniqueId);

            MessageProducer producer = testProps.getPublisherProducerBind() ? session.createProducer(destination) : null;

            MessageConsumer consumer =
                testProps.getPublisherConsumerBind()
                ? session.createConsumer(session.createQueue(testProps.getReceiveDestinationNameRoot() + "_" + uniqueId)) : null;

            MessageMonitor messageMonitor = new MessageMonitor();

            if (consumer != null)
            {
                consumer.setMessageListener(messageMonitor);
            }

            ExceptionMonitor exceptionMonitor = new ExceptionMonitor();
            connection.setExceptionListener(exceptionMonitor);

            if (!testProps.getPublisherConsumerActive() && (consumer != null))
            {
                consumer.close();
            }

            return new CircuitEndBase(producer, consumer, session, messageMonitor, exceptionMonitor);
        }

        /// <summary>
        /// Builds a circuit end suitable for the receiving side of a test circuit, from standard test parameters.
        /// </summary>
        /// <param name="connection"> The connection to build the circuit end on. </param>
        /// <param name="testProps">  The test parameters to configure the circuit end construction. </param>
        /// <param name="uniqueId">   A unique number to being numbering destinations from, to make this circuit unique. </param>
        ///
        /// <return> A circuit end suitable for the receiving side of a test circuit. </return>
        ///
        /// <exception cref="JMSException"> Any underlying JMSExceptions are allowed to fall through and fail the creation. </exception>
        public CircuitEndBase createReceiverCircuitEnd(Connection connection, TestModel testProps, long uniqueId)
            throws JMSException
        {
            log.debug(
                      "public CircuitEndBase createReceiverCircuitEnd(Connection connection, TestModel testProps, long uniqueId = "
                      + uniqueId + "): called");

            // Check that the test properties do not contain AMQP/Qpid specific settings, and fail if they do.
            if (testProps.getImmediate() || testProps.getMandatory())
            {
                throw new RuntimeException(
                                           "Cannot create a pure JMS circuit as the test properties require AMQP specific options.");
            }

            Session session = connection.createSession(testProps.getPublisherTransacted(), testProps.getAckMode());

            MessageProducer producer =
                testProps.getReceiverProducerBind()
                ? session.createProducer(session.createQueue(testProps.getReceiveDestinationNameRoot() + "_" + uniqueId)) : null;

            Destination destination =
                testProps.getPubsub() ? session.createTopic(testProps.getSendDestinationNameRoot() + "_" + uniqueId)
                : session.createQueue(testProps.getSendDestinationNameRoot() + "_" + uniqueId);

            MessageConsumer consumer =
                testProps.getReceiverConsumerBind()
                ? ((testProps.getDurableSubscription() && testProps.getPubsub())
                   ? session.createDurableSubscriber((Topic) destination, "testsub") : session.createConsumer(destination))
                : null;

            MessageMonitor messageMonitor = new MessageMonitor();

            if (consumer != null)
            {
                consumer.setMessageListener(messageMonitor);
            }

            if (!testProps.getReceiverConsumerActive() && (consumer != null))
            {
                consumer.close();
            }

            return new CircuitEndBase(producer, consumer, session, messageMonitor, null);
        }

        /*
        /// <summary>
        /// Sets the sender test client to coordinate the test with.
        /// </summary>
        /// <param name="sender"> The contact details of the sending client in the test. </param>
        public void setSender(TestClientDetails sender)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Sets the receiving test client to coordinate the test with.
        /// </summary>
        /// <param name="receiver"> The contact details of the sending client in the test. </param>
        public void setReceiver(TestClientDetails receiver)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Supplies the sending test client.
        /// </summary>
        /// <return> The sending test client. </return>
        public TestClientDetails getSender()
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Supplies the receiving test client.
        /// </summary>
        /// <return> The receiving test client. </return>
        public IList<TestClientDetails> getReceivers()
        {
            throw new RuntimeException("Not implemented.");
        }
        */

        /*
        /// <summary>
        /// Accepts the conversation factory over which to hold the test coordinating conversation.
        /// </summary>
        /// <param name="conversationFactory"> The conversation factory to coordinate the test over. </param>
        public void setConversationFactory(ConversationFactory conversationFactory)
        {
            throw new RuntimeException("Not implemented.");
        }
        */
    }
}