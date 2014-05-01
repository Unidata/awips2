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
using log4net;
using Apache.Qpid.Messaging;
using Apache.Qpid.Client.Qms;

namespace Apache.Qpid.Client.Tests.interop
{
    public class TopicListener
    {
        private static ILog log = LogManager.GetLogger(typeof(TopicListener));

        /// <summary> The default AMQ connection URL to use for tests. </summary>
        const string DEFAULT_URI = "amqp://guest:guest@default/test?brokerlist='tcp://localhost:5672'";

        /// <summary> Holds the routing key for the topic to receive test messages on. </summary>
        public static string CONTROL_ROUTING_KEY = "topic_control";

        /// <summary> Holds the routing key for the queue to send reports to. </summary>
        public static string RESPONSE_ROUTING_KEY = "response";

        /// <summary> Holds the connection to listen on. </summary>
        private IConnection connection;

        /// <summary> Holds the channel for all test messages.</summary>
        private IChannel channel;

        /// <summary> Holds the producer to send report messages on. </summary>
        private IMessagePublisher publisher;

        /// <summary> Holds a flag to indicate that a timer has begun on the first message. Reset when report is sent. </summary> */
        private bool init;
    
        /// <summary> Holds the count of messages received by this listener. </summary> */
        private int count;

        /// <summary> Creates a topic listener using the specified broker URL. </summary>
        /// 
        /// <param name="connectionUri">The broker URL to listen on.</param>
        TopicListener(string connectionUri)
        {
            log.Debug("TopicListener(string connectionUri = " + connectionUri + "): called");

            // Create a connection to the broker.
            IConnectionInfo connectionInfo = QpidConnectionInfo.FromUrl(connectionUri);
            connection = new AMQConnection(connectionInfo);

            // Establish a session on the broker.
            channel = connection.CreateChannel(false, AcknowledgeMode.AutoAcknowledge, 1);
            
            // Set up a queue to listen for test messages on.
            string topicQueueName = channel.GenerateUniqueName();
            channel.DeclareQueue(topicQueueName, false, true, true);

            // Set this listener up to listen for incoming messages on the test topic queue.
            channel.Bind(topicQueueName, ExchangeNameDefaults.TOPIC, CONTROL_ROUTING_KEY);
            IMessageConsumer consumer = channel.CreateConsumerBuilder(topicQueueName)
                .Create();
            consumer.OnMessage += new MessageReceivedDelegate(OnMessage);

            // Set up this listener with a producer to send the reports on.
            publisher = channel.CreatePublisherBuilder()
                .WithExchangeName(ExchangeNameDefaults.DIRECT)
                .WithRoutingKey(RESPONSE_ROUTING_KEY)
                .Create();

            connection.Start();
            Console.WriteLine("Waiting for messages...");
        }

        public static void Main(String[] argv)
        {
            // Create an instance of this listener with the command line parameters.
            new TopicListener(DEFAULT_URI);
        }

        /// <summary> 
        /// Handles all message received by this listener. Test messages are counted, report messages result in a report being sent and
        /// shutdown messages result in this listener being terminated.
        /// </summary>
        /// 
        /// <param name="message">The received message.</param>
        public void OnMessage(IMessage message)
        {
            log.Debug("public void onMessage(Message message = " + message + "): called");

            // Take the start time of the first message if this is the first message.
            if (!init)
            {
                count = 0;
                init = true;
            }

            // Check if the message is a control message telling this listener to shut down.
            if (IsShutdown(message))
            {
                log.Debug("Got a shutdown message.");
                Shutdown();
            }
            // Check if the message is a report request message asking this listener to respond with the message count.
            else if (IsReport(message))
            {
                log.Debug("Got a report request message.");

                // Send the message count report.
                SendReport();

                // Reset the initialization flag so that the next message is considered to be the first.
                init = false;
            }
            // Otherwise it is an ordinary test message, so increment the message count.
            else
            {
                count++;
            }
        }
        
        /// <summary> Checks a message to see if it is a shutdown control message. </summary>
        /// 
        /// <param name="m">The message to check.</param>
        /// 
        /// <returns><tt>true</tt> if it is a shutdown control message, <tt>false</tt> otherwise.</returns>
        private bool IsShutdown(IMessage m) 
        {
            bool result = CheckTextField(m, "TYPE", "TERMINATION_REQUEST");

            //log.Debug("isShutdown = " + result);

            return result;
        }

        /// <summary> Checks a message to see if it is a report request control message. </summary>
        /// 
        /// <param name="m">The message to check.</param>
        /// 
        /// <returns><tt>true</tt> if it is a report request control message, <tt>false</tt> otherwise.</returns>
        private bool IsReport(IMessage m) 
        {
            bool result = CheckTextField(m, "TYPE", "REPORT_REQUEST");

            //log.Debug("isReport = " + result);

            return result;
        }

        /// <summary> Checks whether or not a text field on a message has the specified value. </summary>
        /// 
        /// <param name="e">The message to check.</param>
        /// <param name="e">The name of the field to check.</param>
        /// <param name="e">The expected value of the field to compare with.</param>
        /// 
        /// <returns> <tt>true</tt>If the specified field has the specified value, <tt>fals</tt> otherwise. </returns>
        private static bool CheckTextField(IMessage m, string fieldName, string value)
        {
            /*log.Debug("private static boolean checkTextField(Message m = " + m + ", String fieldName = " + fieldName
                      + ", String value = " + value + "): called");*/

            string comp = m.Headers.GetString(fieldName);

            return (comp != null) && comp == value;
        }

        /// <summary> Stops the message consumer and closes the connection. </summary>
        private void Shutdown()
        {
            connection.Stop();
            channel.Dispose();
            connection.Dispose();
        }

        /// <summary> Sends the report message to the response location. </summary>
        private void SendReport()
        {
            string report = "Received " + count + ".";

            IMessage reportMessage = channel.CreateTextMessage(report);

            reportMessage.Headers.SetBoolean("BOOLEAN", false);
            //reportMessage.Headers.SetByte("BYTE", 5);
            reportMessage.Headers.SetDouble("DOUBLE", 3.141);
            reportMessage.Headers.SetFloat("FLOAT", 1.0f);
            reportMessage.Headers.SetInt("INT", 1);
            reportMessage.Headers.SetLong("LONG", 1);
            reportMessage.Headers.SetString("STRING", "hello");
            reportMessage.Headers.SetShort("SHORT", 2);

            publisher.Send(reportMessage);

            Console.WriteLine("Sent report: " + report);
        }
    }
}
