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
using Apache.Qpid.Integration.Tests.framework.*;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.MessageConsumer;
using javax.jms.MessageProducer;
using javax.jms.Session;

namespace Apache.Qpid.Integration.Tests.framework.localcircuit
{
    /// <summary>
    /// Provides an implementation of the <see cref="Publisher"/> interface and wraps a single message producer and consumer on
    /// a single controlSession, as a <see cref="CircuitEnd"/>. A local publisher also acts as a circuit end, because for a locally
    /// located circuit the assertions may be applied directly, there does not need to be any inter-process messaging
    /// between the publisher and its single circuit end, in order to ascertain its status.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide a message producer for sending messages.
    /// <tr><td> Provide a message consumer for receiving messages.
    /// <tr><td> Provide assertion that the publisher received no exceptions.
    /// <tr><td> Provide assertion that the publisher received a no consumers error code.
    /// <tr><td> Provide assertion that the publisher received a no route error code.
    /// </table>
    /// </summary>
    public class LocalPublisherImpl extends CircuitEndBase : Publisher
    {
        /// <summary> Holds a reference to the containing circuit. </summary>
        protected LocalCircuitImpl circuit;

        /// <summary>
        /// Creates a circuit end point on the specified producer, consumer and controlSession. Monitors are also configured
        /// for messages and exceptions received by the circuit end.
        /// </summary>
        /// <param name="producer"> The message producer for the circuit end point. </param>
        /// <param name="consumer"> The message consumer for the circuit end point. </param>
        /// <param name="session">  The controlSession for the circuit end point. </param>
        /// <param name="messageMonitor">   The monitor to notify of all messages received by the circuit end. </param>
        /// <param name="exceptionMonitor"> The monitor to notify of all exceptions received by the circuit end. </param>
        public LocalPublisherImpl(MessageProducer producer, MessageConsumer consumer, Session session,
                                  MessageMonitor messageMonitor, ExceptionMonitor exceptionMonitor)
        {
            super(producer, consumer, session, messageMonitor, exceptionMonitor);
        }

        /// <summary>
        /// Creates a circuit end point from the producer, consumer and controlSession in a circuit end base implementation.
        /// </summary>
        /// <param name="end"> The circuit end base implementation to take producers and consumers from. </param>
        public LocalPublisherImpl(CircuitEndBase end)
        {
            super(end.getProducer(), end.getConsumer(), end.getSession(), end.getMessageMonitor(), end.getExceptionMonitor());
        }

        /// <summary> Provides an assertion that the publisher encountered no exceptions. </summary>
        ///
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the publisher encountered no exceptions. </return>
        public Assertion noExceptionsAssertion(ParsedProperties testProps)
        {
            return new AssertionBase()
                {
                    public bool apply()
                    {
                        bool passed = true;
                        ExceptionMonitor sessionExceptionMonitor = circuit.getExceptionMonitor();
                        ExceptionMonitor connectionExceptionMonitor = circuit.getConnectionExceptionMonitor();

                        if (!connectionExceptionMonitor.assertNoExceptions())
                        {
                            passed = false;

                            addError("Was expecting no exceptions.\n");
                            addError("Got the following exceptions on the connection, "
                                     + circuit.getConnectionExceptionMonitor());
                        }

                        if (!sessionExceptionMonitor.assertNoExceptions())
                        {
                            passed = false;

                            addError("Was expecting no exceptions.\n");
                            addError("Got the following exceptions on the producer, " + circuit.getExceptionMonitor());
                        }

                        return passed;
                    }
            };
        }

        /// <summary>
        /// Provides an assertion that the AMQP channel was forcibly closed by an error condition.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the AMQP channel was forcibly closed by an error condition. </return>
        public Assertion channelClosedAssertion(ParsedProperties testProps)
        {
            return new NotApplicableAssertion(testProps);
        }

        /// <summary>
        /// Provides an assertion that the publisher got a given exception during the test.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <param name="exceptionClass"> The exception class to check for. </param>
        ///
        /// <return> An assertion that the publisher got a given exception during the test. </return>
        public Assertion exceptionAssertion(ParsedProperties testProps, final Class<? extends Exception> exceptionClass)
        {
            return new AssertionBase()
                {
                    public bool apply()
                    {
                        bool passed = true;
                        ExceptionMonitor connectionExceptionMonitor = circuit.getConnectionExceptionMonitor();

                        if (!connectionExceptionMonitor.assertExceptionOfType(exceptionClass))
                        {
                            passed = false;

                            addError("Was expecting linked exception type " + exceptionClass.getName()
                                     + " on the connection.\n");
                            addError((connectionExceptionMonitor.size() > 0)
                                     ? ("Actually got the following exceptions on the connection, " + connectionExceptionMonitor)
                                     : "Got no exceptions on the connection.");
                        }

                        return passed;
                    }
            };
        }

        /// <summary>
        /// Sets the contianing circuit.
        /// </summary>
        /// <param name="circuit"> The containing circuit. </param>
        public void setCircuit(LocalCircuitImpl circuit)
        {
            this.circuit = circuit;
        }
    }
}