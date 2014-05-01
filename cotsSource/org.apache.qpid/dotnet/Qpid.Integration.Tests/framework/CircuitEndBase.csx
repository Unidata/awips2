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
//using javax.jms.*;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// A CircuitEndBase is a pair consisting of one message producer and one message consumer, that represents one end of a
    /// test circuit. It is a standard unit of connectivity allowing a full-duplex conversation to be held, provided both
    /// the consumer and producer are instantiated and configured.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Provide a message producer for sending messages.
    /// <tr><td> Provide a message consumer for receiving messages.
    /// </table>
    /// </summary>
    public class CircuitEndBase : CircuitEnd
    {
        /// <summary> Holds the single message producer. </summary>
        MessageProducer producer;

        /// <summary> Holds the single message consumer. </summary>
        MessageConsumer consumer;

        /// <summary> Holds the controlSession for the circuit end. </summary>
        Session session;

        /// <summary> Holds the message monitor for the circuit end. </summary>
        MessageMonitor messageMonitor;

        /// <summary> Holds the exception monitor for the circuit end. </summary>
        ExceptionMonitor exceptionMonitor;

        /// <summary>
        /// Creates a circuit end point on the specified producer, consumer and controlSession. Monitors are also configured
        /// for messages and exceptions received by the circuit end.
        /// </summary>
        /// <param name="producer">         The message producer for the circuit end point. </param>
        /// <param name="consumer">         The message consumer for the circuit end point. </param>
        /// <param name="session">          The controlSession for the circuit end point. </param>
        /// <param name="messageMonitor">   The monitor to notify of all messages received by the circuit end. </param>
        /// <param name="exceptionMonitor"> The monitor to notify of all exceptions received by the circuit end. </param>
        public CircuitEndBase(MessageProducer producer, MessageConsumer consumer, Session session, MessageMonitor messageMonitor,
                              ExceptionMonitor exceptionMonitor)
        {
            this.producer = producer;
            this.consumer = consumer;
            this.session = session;

            this.messageMonitor = messageMonitor;
            this.exceptionMonitor = exceptionMonitor;
        }

        /// <summary>
        /// Gets the message producer at this circuit end point.
        /// </summary>
        /// <return> The message producer at with this circuit end point. </return>
        public IMessagePublisher GetProducer()
        {
            return producer;
        }

        /// <summary>
        /// Gets the message consumer at this circuit end point.
        /// </summary>
        /// <return> The message consumer at this circuit end point. </return>
        public IMessageConsumer GetConsumer()
        {
            return consumer;
        }

        /// <summary>
        /// Send the specified message over the producer at this end point.
        /// </summary>
        /// <param name="message"> The message to send. </param>
        /// <exception cref="javax.jms.JMSException"> Any JMS exception occuring during the send is allowed to fall through. </exception>
        public void Send(IMessage message)
        {
            producer.send(message);
        }

        /// <summary>
        /// Gets the JMS Session associated with this circuit end point.
        /// </summary>
        /// <return> The JMS Session associated with this circuit end point. </return>
        public IChannel GetSession()
        {
            return session;
        }

        /// <summary>
        /// Closes the message producers and consumers and the sessions, associated with this circuit end point.
        /// </summary>
        /// <exception cref="javax.jms.JMSException"> Any JMSExceptions occurring during the close are allowed to fall through. </exception>
        public void Close()
        {
            if (producer != null)
            {
                producer.Close();
            }

            if (consumer != null)
            {
                consumer.Close();
            }
        }

        /// <summary>
        /// Returns the message monitor for reporting on received messages on this circuit end.
        /// </summary>
        /// <return> The message monitor for this circuit end. </return>
        public MessageMonitor GetMessageMonitor()
        {
            return messageMonitor;
        }

        /// <summary>
        /// Returns the exception monitor for reporting on exceptions received on this circuit end.
        /// </summary>
        /// <return> The exception monitor for this circuit end. </return>
        public ExceptionMonitor GetExceptionMonitor()
        {
            return exceptionMonitor;
        }
    }
}