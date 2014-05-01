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

using Apache.Qpid.Integration.Tests.framework.*;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.*;

using System.Collections.Generic.LinkedList;
using System.Collections.Generic.IList;

namespace Apache.Qpid.Integration.Tests.framework.localcircuit
{
    /// <summary>
    /// LocalCircuitImpl provides an implementation of the test circuit. This is a local only circuit implementation that
    /// supports a single producer/consumer on each end of the circuit, with both ends of the circuit on the same JVM.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Supply the publishing and receiving ends of a test messaging circuit.
    ///     <td> <see cref="LocalPublisherImpl"/>, <see cref="LocalReceiverImpl"/>
    /// <tr><td> Start the circuit running.
    /// <tr><td> Close the circuit down.
    /// <tr><td> Take a reading of the circuits state.
    /// <tr><td> Apply assertions against the circuits state. <td> <see cref="Assertion"/>
    /// <tr><td> Send test messages over the circuit.
    /// <tr><td> Perform the default test procedure on the circuit.
    /// <tr><td> Provide access to connection and controlSession exception monitors. <td> <see cref="ExceptionMonitor"/>
    /// </table>
    /// </summary>
    public class LocalCircuitImpl : Circuit
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(LocalCircuitImpl));

        /// <summary> Holds the test configuration for the circuit. </summary>
        private ParsedProperties testProps;

        /// <summary> Holds the publishing end of the circuit. </summary>
        private LocalPublisherImpl publisher;

        /// <summary> Holds the receiving end of the circuit. </summary>
        private LocalReceiverImpl receiver;

        /// <summary> Holds the connection for the publishing end of the circuit. </summary>
        private Connection connection;

        /// <summary> Holds the exception listener for the connection on the publishing end of the circuit. </summary>
        private ExceptionMonitor connectionExceptionMonitor;

        /// <summary> Holds the exception listener for the controlSession on the publishing end of the circuit. </summary>
        private ExceptionMonitor exceptionMonitor;

        /// <summary>
        /// Creates a test circuit using the specified test parameters. The publisher, receivers, connection and
        /// connection monitor must already have been created, to assemble the circuit.
        /// </summary>
        /// <param name="testProps">                  The test parameters. </param>
        /// <param name="publisher">                  The test publisher. </param>
        /// <param name="receiver">                   The test receivers. </param>
        /// <param name="connection">                 The connection. </param>
        /// <param name="connectionExceptionMonitor"> The connection exception monitor. </param>
        public LocalCircuitImpl(ParsedProperties testProps, LocalPublisherImpl publisher, LocalReceiverImpl receiver,
                                Connection connection, ExceptionMonitor connectionExceptionMonitor)
        {
            this.testProps = testProps;
            this.publisher = publisher;
            this.receiver = receiver;
            this.connection = connection;
            this.connectionExceptionMonitor = connectionExceptionMonitor;
            this.exceptionMonitor = new ExceptionMonitor();

            // Set this as the parent circuit on the publisher and receivers.
            publisher.setCircuit(this);
            receiver.setCircuit(this);
        }

        /// <summary>
        /// Gets the interface on the publishing end of the circuit.
        /// </summary>
        /// <return> The publishing end of the circuit. </return>
        public Publisher getPublisher()
        {
            return publisher;
        }

        /// <summary>
        /// Gets the local publishing circuit end, for direct manipulation.
        /// </summary>
        /// <return> The local publishing circuit end. </return>
        public CircuitEnd getLocalPublisherCircuitEnd()
        {
            return publisher;
        }

        /// <summary>
        /// Gets the interface on the receiving end of the circuit.
        /// </summary>
        /// <return> The receiving end of the circuit. </return>
        public Receiver getReceiver()
        {
            return receiver;
        }

        /// <summary>
        /// Gets the local receiving circuit end, for direct manipulation.
        /// </summary>
        /// <return> The local receiving circuit end. </return>
        public CircuitEnd getLocalReceiverCircuitEnd()
        {
            return receiver;
        }

        /// <summary>
        /// Checks the test circuit. The effect of this is to gather the circuits state, for both ends of the circuit,
        /// into a report, against which assertions may be checked.
        /// </summary>
        public void check()
        { }

        /// <summary>
        /// Applied a list of assertions against the test circuit. The <see cref="#check()"/> method should be called before doing
        /// this, to ensure that the circuit has gathered its state into a report to assert against.
        /// </summary>
        /// <param name="assertions"> The list of assertions to apply. </param>
        /// <return> Any assertions that failed. </return>
        public IList<Assertion> applyAssertions(List<Assertion> assertions)
        {
            IList<Assertion> failures = new LinkedList<Assertion>();

            for (Assertion assertion : assertions)
            {
                if (!assertion.apply())
                {
                    failures.add(assertion);
                }
            }

            return failures;
        }

        /// <summary> Connects and starts the circuit. After this method is called the circuit is ready to send messages. </summary>
        public void start()
        { }

        /// <summary> Closes the circuit. All associated resources are closed. </summary>
        public void close()
        {
            try
            {
                publisher.close();
                receiver.close();
                connection.close();
            }
            catch (JMSException e)
            {
                throw new RuntimeException("Got JMSException during close:" + e.getMessage(), e);
            }
        }

        /// <summary> Sends a message on the test circuit. The exact nature of the message sent is controlled by the test parameters. </summary>
        protected void send()
        {
            // Cast the test properties into a typed interface for convenience.
            MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProps);

            bool transactional = props.getPublisherTransacted();
            bool rollback = props.getRollbackPublisher();

            // Send a message through the publisher and log any exceptions raised.
            try
            {
                CircuitEnd end = getLocalPublisherCircuitEnd();

                end.send(createTestMessage(end));

                if (rollback)
                {
                    end.getSession().rollback();
                }
                else if (transactional)
                {
                    end.getSession().commit();
                }
            }
            catch (JMSException e)
            {
                exceptionMonitor.onException(e);
            }
        }

        /// <summary>
        /// Runs the default test procedure against the circuit, and checks that all of the specified assertions hold. The
        /// outline of the default test procedure is:
        ///
        /// <p/><pre>
        /// Start the circuit.
        /// Send test messages.
        /// Request a status report.
        /// Assert conditions on the publishing end of the circuit.
        /// Assert conditions on the receiving end of the circuit.
        /// Close the circuit.
        /// Pass with no failed assertions or fail with a list of failed assertions.
        /// </pre>
        /// </summary>
        /// <param name="numMessages"> The number of messages to send using the default test procedure. </param>
        /// <param name="assertions">  The list of assertions to apply. </param>
        /// <return> Any assertions that failed. </return>
        public IList<Assertion> test(int numMessages, List<Assertion> assertions)
        {
            // Start the test circuit.
            start();

            // Send the requested number of test messages.
            for (int i = 0; i < numMessages; i++)
            {
                send();
            }

            // Inject a short pause to allow time for exceptions to come back asynchronously.
            TestUtils.pause(500L);

            // Request a status report.
            check();

            // Clean up the publisher/receivers/controlSession/connections.
            close();

            // Apply all of the requested assertions, keeping record of any that fail.
            IList<Assertion> failures = applyAssertions(assertions);

            // Return any failed assertions to the caller.
            return failures;
        }

        /// <summary>
        /// Creates a message with the properties defined as per the test parameters.
        /// </summary>
        /// <param name="client"> The circuit end to create the message on. </param>
        ///
        /// <return> The test message. </return>
        ///
        /// <exception cref="JMSException"> Any JMSException occurring during creation of the message is allowed to fall through. </exception>
        private Message createTestMessage(CircuitEnd client) throws JMSException
        {
            // Cast the test properties into a typed interface for convenience.
            MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProps);

            return TestUtils.createTestMessageOfSize(client.getSession(), props.getMessageSize());
        }

        /// <summary>
        /// Gets the exception monitor for the publishing ends connection.
        /// </summary>
        /// <return> The exception monitor for the publishing ends connection. </return>
        public ExceptionMonitor getConnectionExceptionMonitor()
        {
            return connectionExceptionMonitor;
        }

        /// <summary>
        /// Gets the exception monitor for the publishing ends controlSession.
        /// </summary>
        /// <return> The exception monitor for the publishing ends controlSession. </return>
        public ExceptionMonitor getExceptionMonitor()
        {
            return exceptionMonitor;
        }
    }
}
