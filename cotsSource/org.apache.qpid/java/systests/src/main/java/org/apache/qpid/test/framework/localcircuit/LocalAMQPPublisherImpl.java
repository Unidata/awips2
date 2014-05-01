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
package org.apache.qpid.test.framework.localcircuit;

import org.apache.qpid.client.AMQNoConsumersException;
import org.apache.qpid.client.AMQNoRouteException;
import org.apache.qpid.test.framework.*;

import org.apache.qpid.junit.extensions.util.ParsedProperties;

import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Session;

/**
 * LocalAMQPPublisherImpl is an extension of {@link LocalPublisherImpl} that adds AMQP specific features. Specifically
 * extra assertions for AMQP features not available through generic JMS.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td>
 * </table>
 */
public class LocalAMQPPublisherImpl extends LocalPublisherImpl implements AMQPPublisher
{
    /**
     * Creates a circuit end point on the specified producer, consumer and controlSession. Monitors are also configured
     * for messages and exceptions received by the circuit end.
     *
     * @param producer         The message producer for the circuit end point.
     * @param consumer         The message consumer for the circuit end point.
     * @param session          The controlSession for the circuit end point.
     * @param messageMonitor   The monitor to notify of all messages received by the circuit end.
     * @param exceptionMonitor The monitor to notify of all exceptions received by the circuit end.
     */
    public LocalAMQPPublisherImpl(MessageProducer producer, MessageConsumer consumer, Session session,
        MessageMonitor messageMonitor, ExceptionMonitor exceptionMonitor)
    {
        super(producer, consumer, session, messageMonitor, exceptionMonitor);
    }

    /**
     * Creates a circuit end point from the producer, consumer and controlSession in a circuit end base implementation.
     *
     * @param end The circuit end base implementation to take producers and consumers from.
     */
    public LocalAMQPPublisherImpl(CircuitEndBase end)
    {
        super(end);
    }

    /**
     * Provides an assertion that the publisher got a no consumers exception on every message.
     *
     * @param testProps The test configuration properties.
     *
     * @return An assertion that the publisher got a no consumers exception on every message.
     */
    public Assertion noConsumersAssertion(ParsedProperties testProps)
    {
        return new AssertionBase()
            {
                public boolean apply()
                {
                    boolean passed = true;
                    ExceptionMonitor connectionExceptionMonitor = circuit.getConnectionExceptionMonitor();

                    if (!connectionExceptionMonitor.assertOneJMSExceptionWithLinkedCause(AMQNoConsumersException.class))
                    {
                        passed = false;

                        addError("Was expecting linked exception type " + AMQNoConsumersException.class.getName()
                            + " on the connection.\n");
                        addError((connectionExceptionMonitor.size() > 0)
                            ? ("Actually got the following exceptions on the connection, " + connectionExceptionMonitor)
                            : "Got no exceptions on the connection.");
                    }

                    return passed;
                }
            };
    }

    /**
     * Provides an assertion that the publisher got a no rout exception on every message.
     *
     * @param testProps The test configuration properties.
     *
     * @return An assertion that the publisher got a no rout exception on every message.
     */
    public Assertion noRouteAssertion(ParsedProperties testProps)
    {
        return new AssertionBase()
            {
                public boolean apply()
                {
                    boolean passed = true;
                    ExceptionMonitor connectionExceptionMonitor = circuit.getConnectionExceptionMonitor();

                    if (!connectionExceptionMonitor.assertOneJMSExceptionWithLinkedCause(AMQNoRouteException.class))
                    {
                        passed = false;

                        addError("Was expecting linked exception type " + AMQNoRouteException.class.getName()
                            + " on the connection.\n");
                        addError((connectionExceptionMonitor.size() > 0)
                            ? ("Actually got the following exceptions on the connection, " + connectionExceptionMonitor)
                            : "Got no exceptions on the connection.");
                    }

                    return passed;
                }
            };
    }
}
