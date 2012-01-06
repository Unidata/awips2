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

import javax.jms.*;

/**
 * A CircuitEndBase is a pair consisting of one message producer and one message consumer, that represents one end of a
 * test circuit. It is a standard unit of connectivity allowing a full-duplex conversation to be held, provided both
 * the consumer and producer are instantiated and configured.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Provide a message producer for sending messages.
 * <tr><td> Provide a message consumer for receiving messages.
 * </table>
 */
public class CircuitEndBase implements CircuitEnd
{
    /** Holds the single message producer. */
    MessageProducer producer;

    /** Holds the single message consumer. */
    MessageConsumer consumer;

    /** Holds the controlSession for the circuit end. */
    Session session;

    /** Holds the message monitor for the circuit end. */
    MessageMonitor messageMonitor;

    /** Holds the exception monitor for the circuit end. */
    ExceptionMonitor exceptionMonitor;

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
    public CircuitEndBase(MessageProducer producer, MessageConsumer consumer, Session session, MessageMonitor messageMonitor,
        ExceptionMonitor exceptionMonitor)
    {
        this.producer = producer;
        this.consumer = consumer;
        this.session = session;

        this.messageMonitor = messageMonitor;
        this.exceptionMonitor = exceptionMonitor;
    }

    /**
     * Gets the message producer at this circuit end point.
     *
     * @return The message producer at with this circuit end point.
     */
    public MessageProducer getProducer()
    {
        return producer;
    }

    /**
     * Gets the message consumer at this circuit end point.
     *
     * @return The message consumer at this circuit end point.
     */
    public MessageConsumer getConsumer()
    {
        return consumer;
    }

    /**
     * Send the specified message over the producer at this end point.
     *
     * @param message The message to send.
     * @throws javax.jms.JMSException Any JMS exception occuring during the send is allowed to fall through.
     */
    public void send(Message message) throws JMSException
    {
        producer.send(message);
    }

    /**
     * Gets the JMS Session associated with this circuit end point.
     *
     * @return The JMS Session associated with this circuit end point.
     */
    public Session getSession()
    {
        return session;
    }

    /**
     * Closes the message producers and consumers and the sessions, associated with this circuit end point.
     *
     * @throws javax.jms.JMSException Any JMSExceptions occurring during the close are allowed to fall through.
     */
    public void close() throws JMSException
    {
        if (producer != null)
        {
            producer.close();
        }

        if (consumer != null)
        {
            consumer.close();
        }
    }

    /**
     * Returns the message monitor for reporting on received messages on this circuit end.
     *
     * @return The message monitor for this circuit end.
     */
    public MessageMonitor getMessageMonitor()
    {
        return messageMonitor;
    }

    /**
     * Returns the exception monitor for reporting on exceptions received on this circuit end.
     *
     * @return The exception monitor for this circuit end.
     */
    public ExceptionMonitor getExceptionMonitor()
    {
        return exceptionMonitor;
    }
}
