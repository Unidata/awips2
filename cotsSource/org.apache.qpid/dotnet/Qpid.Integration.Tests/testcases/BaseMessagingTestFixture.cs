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
using System;
using System.Text;
using log4net;
using NUnit.Framework;
using Apache.Qpid.Messaging;
using Apache.Qpid.Client.Qms;
using Apache.Qpid.Client;

namespace Apache.Qpid.Integration.Tests.testcases
{
    /// <summary>
    /// Provides a basis for writing Unit tests that communicate with an AMQ protocol broker. By default it creates a connection
    /// to a message broker running on localhost on the standard AMQ port, 5672, using guest:guest login credentials. It also
    /// creates a standard auto-ack channel on this connection.
    /// </summary>
    public class BaseMessagingTestFixture
    {
        private static ILog log = LogManager.GetLogger(typeof(BaseMessagingTestFixture));

        /// <summary> Used to build dummy data to fill test messages with. </summary>
        private const string MESSAGE_DATA_BYTES = "-- Test Message -- Test Message -- Test Message -- Test Message -- Test Message ";

        /// <summary> The default timeout in milliseconds to use on receives. </summary>
        private const long RECEIVE_WAIT = 2000;

        /// <summary> The default AMQ connection URL to use for tests. </summary>
        public const string connectionUri = "amqp://guest:guest@test/test?brokerlist='tcp://localhost:5672'";

        /// <summary> The default AMQ connection URL parsed as a connection info. </summary>
        protected IConnectionInfo connectionInfo;

        /// <summary> Holds an array of connections for building mutiple test end-points. </summary>
        protected IConnection[] testConnection = new IConnection[10];

        /// <summary> Holds an array of channels for building mutiple test end-points. </summary>
        protected IChannel[] testChannel = new IChannel[10];

         /// <summary> Holds an array of queues for building mutiple test end-points. </summary>
        protected String[] testQueue = new String[10];
        
        /// <summary> Holds an array of producers for building mutiple test end-points. </summary>
        protected IMessagePublisher[] testProducer = new IMessagePublisher[10];

        /// <summary> Holds an array of consumers for building mutiple test end-points. </summary>
        protected IMessageConsumer[] testConsumer = new IMessageConsumer[10];

        /// <summary> A counter used to supply unique ids. </summary>
        private static int uniqueId = 0;

        /// <summary> Used to hold unique ids per test. </summary>
        protected Guid testId;

        /// <summary> Creates the test connection and channel. </summary>
        [SetUp]
        public virtual void Init()
        {
            log.Debug("public virtual void Init(): called");

            // Set up a unique id for this test.
            testId = System.Guid.NewGuid();
        }

        /// <summary>
        /// Disposes of the test connection. This is called manually because the connection is a field so dispose will not be automatically 
        /// called on it.
        /// </summary>
        [TearDown]
        public virtual void Shutdown()
        {
            log.Debug("public virtual void Shutdown(): called");
        }

        /// <summary> Sets up the nth test end-point. </summary>
        ///
        /// <param name="n">The index of the test end-point to set up.</param>
        /// <param name="producer"><tt>true</tt> to set up a producer on the end-point.</param>
        /// <param name="consumer"><tt>true</tt> to set up a consumer on the end-point.</param>
        /// <param name="routingKey">The routing key for the producer to send on.</param>
        /// <param name="ackMode">The ack mode for the end-points channel.</param>
        /// <param name="transacted"><tt>true</tt> to use transactions on the end-points channel.</param>
        /// <param name="exchangeName">The exchange to produce or consume on.</param>
        /// <param name="declareBind"><tt>true</tt> if the consumers queue should be declared and bound, <tt>false</tt> if it has already been.</param>
        /// <param name="durable"><tt>true</tt> to declare the consumers queue as durable.</param>
        /// <param name="subscriptionName">If durable is true, the fixed unique queue name to use.</param>
        public void SetUpEndPoint(int n, bool producer, bool consumer, string routingKey, AcknowledgeMode ackMode, bool transacted,
                                  string exchangeName, bool declareBind, bool durable, string subscriptionName)
        {
            // Allow client id to be fixed, or undefined.
            {
                // Use unique id for end point.
                connectionInfo = QpidConnectionInfo.FromUrl(connectionUri);

                connectionInfo.ClientName = "test" + n;
            }

            testConnection[n] = new AMQConnection(connectionInfo);            
            testConnection[n].Start();
            testChannel[n] = testConnection[n].CreateChannel(transacted, ackMode);
            
            if (producer)
            {
                testProducer[n] = testChannel[n].CreatePublisherBuilder()
                    .WithExchangeName(exchangeName)
                    .WithRoutingKey(routingKey)
                    .Create();
            }

            if (consumer)
            {
                string queueName;

                // Use the subscription name as the queue name if the subscription is durable, otherwise use a generated name.
                if (durable)
                {
                    // The durable queue is declared without auto-delete, and passively, in case it has already been declared.
                    queueName = subscriptionName;
                    
                    if (declareBind)
                    {
                        testChannel[n].DeclareQueue(queueName, durable, true, false);
                        testChannel[n].Bind(queueName, exchangeName, routingKey);
                    }
                }
                else
                {
                    queueName = testChannel[n].GenerateUniqueName();

                    if (declareBind)
                    {
                    	if (durable) 
                    	{
                    		testQueue[n] = queueName;
                    	}
                        testChannel[n].DeclareQueue(queueName, durable, true, true);
                        testChannel[n].Bind(queueName, exchangeName, routingKey);
                    }
                }

                testConsumer[n] = testChannel[n].CreateConsumerBuilder(queueName).Create();
            }
        }

        /// <summary> Closes down the nth test end-point. </summary>
        public void CloseEndPoint(int n)
        {
            log.Debug("public void CloseEndPoint(int n): called");

            if (testProducer[n] != null)
            {
                testProducer[n].Close();
                testProducer[n].Dispose();
                testProducer[n] = null;
            }

            if (testConsumer[n] != null)
            {
            	if (testQueue[n] != null)
            	{
            		testChannel[n].DeleteQueue(testQueue[n], false, false, true);
            	}
                testConsumer[n].Close();
                testConsumer[n].Dispose();
                testConsumer[n] = null;
            }

            if (testConnection[n] != null)
            {
                testConnection[n].Stop();            
                testConnection[n].Close();
                testConnection[n].Dispose();
                testConnection[n] = null;
            }
        }

        /// <summary>
        /// Consumes n messages, checking that the n+1th is not available within a timeout, and that the consumed messages
        /// are text messages with contents equal to the specified message body.
        /// </summary>
        ///
        /// <param name="n">The number of messages to consume.</param>
        /// <param name="body">The body text to match against all messages.</param>
        /// <param name="consumer">The message consumer to recieve the messages on.</param>
        public static void ConsumeNMessagesOnly(int n, string body, IMessageConsumer consumer)
        {
            ConsumeNMessages(n, body, consumer);
            
            // Check that one more than n cannot be received.
            IMessage msg = consumer.Receive(RECEIVE_WAIT);
            Assert.IsNull(msg, "Consumer got more messages than the number requested (" + n + ").");
        }

        /// <summary>
        /// Consumes n messages, checking that the n+1th is not available within a timeout, and that the consumed messages
        /// are text messages with contents equal to the specified message body.
        /// </summary>
        ///
        /// <param name="n">The number of messages to consume.</param>
        /// <param name="body">The body text to match against all messages.</param>
        /// <param name="consumer">The message consumer to recieve the messages on.</param>
        public static void ConsumeNMessages(int n, string body, IMessageConsumer consumer)
        {
            IMessage msg;
            
            // Try to receive n messages.
            for (int i = 0; i < n; i++)
            {
                msg = consumer.Receive(RECEIVE_WAIT);
                Assert.IsNotNull(msg, "Consumer did not receive message number: " + i);
                Assert.AreEqual(body, ((ITextMessage)msg).Text, "Incorrect Message recevied on consumer1.");
            }
        }

        /// <summary>Creates the requested number of bytes of dummy text. Usually used for filling test messages. </summary>
        ///
        /// <param name="size">The number of bytes of dummy text to generate.</param>
        ///
        /// <return>The requested number of bytes of dummy text.</return>
        public static String GetData(int size)
        {
            StringBuilder buf = new StringBuilder(size);

            if (size > 0)
            {
                int div = MESSAGE_DATA_BYTES.Length / size;
                int mod = MESSAGE_DATA_BYTES.Length % size;

                for (int i = 0; i < div; i++)
                {
                    buf.Append(MESSAGE_DATA_BYTES);
                }

                if (mod != 0)
                {
                    buf.Append(MESSAGE_DATA_BYTES, 0, mod);
                }
            }
            
            return buf.ToString();
        }
    }
}
