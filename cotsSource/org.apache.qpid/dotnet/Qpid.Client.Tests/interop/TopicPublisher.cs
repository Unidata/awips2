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
using System.Threading;
using log4net;
using Apache.Qpid.Messaging;
using Apache.Qpid.Client.Qms;

namespace Apache.Qpid.Client.Tests.interop
{
    public class TopicPublisher
    {
        private static ILog log = LogManager.GetLogger(typeof(TopicPublisher));

        /// <summary> The default AMQ connection URL to use for tests. </summary>
        const string DEFAULT_URI = "amqp://guest:guest@default/test?brokerlist='tcp://localhost:5672'";

        /// <summary> Holds the default test timeout for broker communications before tests give up. </summary>
        const int TIMEOUT = 10000;

        /// <summary> Holds the routing key for the topic to receive test messages on. </summary>
        const string CONTROL_ROUTING_KEY = "topic_control";

        /// <summary> Holds the routing key for the queue to send reports to. </summary>
        const string RESPONSE_ROUTING_KEY = "response";

        /// <summary> Holds the number of messages to send in each test run. </summary>
        private int numMessages;

        /// <summary> Holds the number of subscribers listening to the messsages. </summary>
        private int numSubscribers;

        /// <summary> A monitor used to wait for all reports to arrive back from consumers on. </summary>
        private AutoResetEvent allReportsReceivedEvt = new AutoResetEvent(false);

        /// <summary> Holds the connection to listen on. </summary>
        private IConnection connection;

        /// <summary> Holds the channel for all test messages.</summary>
        private IChannel channel;

        /// <summary> Holds the producer to send test messages on. </summary>
        private IMessagePublisher publisher;

        /// <summary>
        /// Creates a topic publisher that will send the specifed number of messages and expect the specifed number of report back from test
        /// subscribers.
        /// </summary>
        /// 
        /// <param name="connectionUri">The broker URL.</param>
        /// <param name="numMessages">The number of messages to send in each test.</param>
        /// <param name="numSubscribers">The number of subscribes that are expected to reply with a report.</param>
        TopicPublisher(string connectionUri, int numMessages, int numSubscribers)
        {
            log.Debug("TopicPublisher(string connectionUri = " + connectionUri + ", int numMessages = "+ numMessages + 
                      ", int numSubscribers = " + numSubscribers + "): called");

            // Create a connection to the broker.
            IConnectionInfo connectionInfo = QpidConnectionInfo.FromUrl(connectionUri);
            connection = new AMQConnection(connectionInfo);

            // Establish a session on the broker.
            channel = connection.CreateChannel(false, AcknowledgeMode.AutoAcknowledge, 1);

            // Set up a queue to listen for reports on.
            string responseQueueName = channel.GenerateUniqueName();
            channel.DeclareQueue(responseQueueName, false, true, true);
            
            // Set this listener up to listen for reports on the response queue.
            channel.Bind(responseQueueName, ExchangeNameDefaults.DIRECT, RESPONSE_ROUTING_KEY);
            //channel.Bind(responseQueueName, "<<default>>", RESPONSE_ROUTING_KEY);
            IMessageConsumer consumer = channel.CreateConsumerBuilder(responseQueueName)               
                .Create();
            consumer.OnMessage += new MessageReceivedDelegate(OnMessage);

            // Set up this listener with a producer to send the test messages and report requests on.
            publisher = channel.CreatePublisherBuilder()
                .WithExchangeName(ExchangeNameDefaults.TOPIC)
                .WithRoutingKey(CONTROL_ROUTING_KEY)
                .Create();

            // Keep the test parameters.
            this.numMessages = numMessages;
            this.numSubscribers = numSubscribers;

            connection.Start();
            Console.WriteLine("Sending messages and waiting for reports...");
        }

        /// <summary>
        /// Start a test subscriber. The broker URL must be specified as the first command line argument.
        /// </summary>
        /// 
        /// <param name="argv">The command line arguments, broker URL first.</param>
        public static void Main(String[] argv)
        {
            // Create an instance of this publisher with the command line parameters.
            TopicPublisher publisher = new TopicPublisher(DEFAULT_URI, 1, 1);

            // Publish the test messages.
            publisher.DoTest();
        }

        /// <summary>
        /// Sends the test messages and waits for all subscribers to reply with a report.
        /// </summary>
        public void DoTest()
        {
            log.Debug("public void DoTest(): called");

            // Create a test message to send.
            IMessage testMessage = channel.CreateTextMessage("test");

            // Send the desired number of test messages.
            for (int i = 0; i < numMessages; i++)
            {
                publisher.Send(testMessage);
            }

            log.Debug("Sent " + numMessages + " test messages.");

            // Send the report request.
            IMessage reportRequestMessage = channel.CreateTextMessage("Report request message.");
            reportRequestMessage.Headers["TYPE"] = "REPORT_REQUEST";
            
            reportRequestMessage.Headers.SetBoolean("BOOLEAN", false);
            //reportRequestMessage.Headers.SetByte("BYTE", 5);
            reportRequestMessage.Headers.SetDouble("DOUBLE", 3.141);
            reportRequestMessage.Headers.SetFloat("FLOAT", 1.0f);
            reportRequestMessage.Headers.SetInt("INT", 1);
            reportRequestMessage.Headers.SetLong("LONG", 1);
            reportRequestMessage.Headers.SetString("STRING", "hello");
            reportRequestMessage.Headers.SetShort("SHORT", 2);

            publisher.Send(reportRequestMessage);

            log.Debug("Sent the report request message, waiting for all replies...");

            // Wait until all the reports come in.
            allReportsReceivedEvt.WaitOne(TIMEOUT, true);

            // Check if all reports were really received or if the timeout occurred.
            if (numSubscribers == 0)
            {
                log.Debug("Got all reports.");
            }
            else
            {
                log.Debug("Waiting for reports timed out, still waiting for " + numSubscribers + ".");
            }

            // Send the termination request.
            IMessage terminationRequestMessage = channel.CreateTextMessage("Termination request message.");
            terminationRequestMessage.Headers["TYPE"] =  "TERMINATION_REQUEST";
            publisher.Send(terminationRequestMessage);

            log.Debug("Sent the termination request message.");

            // Close all message producers and consumers and the connection to the broker.
            Shutdown();
        }

        /// <summary>
        /// Handles all report messages from subscribers. This decrements the count of subscribers that are still to reply, until this becomes
        /// zero, at which time waiting threads are notified of this event.
        /// </summary>
        /// 
        /// <param name="message">The received report message.</param>
        public void OnMessage(IMessage message)
        {
            log.Debug("public void OnMessage(IMessage message = " + message + "): called");

            // Decrement the count of expected messages and release the wait monitor when this becomes zero.
            if (--numSubscribers == 0)
            {
                log.Debug("Got reports from all subscribers.");
                allReportsReceivedEvt.Set();
            }
        }

        /// <summary> Stops the message consumers and closes the connection. </summary>
        private void Shutdown()
        {
            connection.Stop();
            publisher.Dispose();
            channel.Dispose();
            connection.Dispose();
        }
    }
}
