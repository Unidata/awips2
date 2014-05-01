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
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Integration.Tests.interop.TestCases
{
    /// <summary>
    /// Implements test case 3, basic pub/sub. Sends/received a specified number of messages to a specified route on the
    /// default topic exchange, using the specified number of receiver connections. Produces reports on the actual number of
    /// messages sent/received.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Supply the name of the test case that this implements.
    /// <tr><td> Accept/Reject invites based on test parameters.
    /// <tr><td> Adapt to assigned roles.
    /// <tr><td> Send required number of test messages using pub/sub.
    /// <tr><td> Generate test reports.
    /// </table>
    /// </summary>
    public class TestCase3BasicPubSub : InteropClientTestCase
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(TestCase3BasicPubSub));
    
        /// <summary> Holds the count of test messages received. </summary>
        private int messageCount;

        /// <summary> The role to be played by the test. </summary>
        private Roles role;

        /// <summary> The number of test messages to send. </summary>
        private int numMessages;

        /// <summary> The number of receiver connection to use. </summary>
        private int numReceivers;

        /// <summary> The routing key to send them to on the default direct exchange. </summary>
        private string sendDestination;

        /// <summary> The connections to send/receive the test messages on. </summary>
        private IConnection[] connection;

        /// <summary> The sessions to send/receive the test messages on. </summary>
        private IChannel[] channel;

        /// <summary> The producer to send the test messages with. </summary>
        IMessagePublisher publisher;

        /// <summary>
        /// Should provide the name of the test case that this class implements. The exact names are defined in the
        /// interop testing spec.
        /// </summary>
        ///
        /// <returns> The name of the test case that this implements. </returns>
        public String GetName()
        {
            log.Debug("public String GetName(): called");
    
            return "TC3_BasicPubSub";
        }
    
        /// <summary>
        /// Determines whether the test invite that matched this test case is acceptable.
        /// </summary>
        ///
        /// <param name="inviteMessage"> The invitation to accept or reject. </param>
        ///
        /// <returns> <tt>true</tt> to accept the invitation, <tt>false</tt> to reject it. </returns>
        public bool AcceptInvite(IMessage inviteMessage) 
        {
            log.Debug("public boolean AcceptInvite(IMessage inviteMessage = " + inviteMessage + "): called");
    
            // All invites are acceptable.
            return true;
        }
    
        /// <summary>
        /// Assigns the role to be played by this test case. The test parameters are fully specified in the
        /// assignment message. When this method return the test case will be ready to execute.
        /// </summary>
        ///
        /// <param name="role">              The role to be played; sender or receiver. </param>
        /// <param name="assignRoleMessage"> The role assingment message, contains the full test parameters. </param>
        public void AssignRole(Roles role, IMessage assignRoleMessage) 
        {
            log.Debug("public void assignRole(Roles role = " + role + ", IMessage assignRoleMessage = " + assignRoleMessage
                + "): called");
    
            // Reset the message count for a new test.
            messageCount = 0;
    
            // Take note of the role to be played.
            this.role = role;
    
            // Extract and retain the test parameters.
            numMessages = assignRoleMessage.Headers.GetInt("PUBSUB_NUM_MESSAGES");
            numReceivers = assignRoleMessage.Headers.GetInt("PUBSUB_NUM_RECEIVERS");
            string sendKey = assignRoleMessage.Headers.GetString("PUBSUB_KEY");
            sendDestination = sendKey;
    
            log.Debug("numMessages = " + numMessages);
            log.Debug("numReceivers = " + numReceivers);
            log.Debug("sendKey = " + sendKey);
            log.Debug("role = " + role);
    
            switch (role)
            {
            // Check if the sender role is being assigned, and set up a single message producer if so.
            case Roles.SENDER:
                // Create a new connection to pass the test messages on.
                connection = new IConnection[1];
                channel = new IChannel[1];
    
                connection[0] =
                    TestClient.CreateConnection(TestClient.brokerUrl, TestClient.virtualHost);
                channel[0] = connection[0].CreateChannel(false, AcknowledgeMode.AutoAcknowledge);
    
                // Extract and retain the test parameters.
                publisher = channel[0].CreatePublisherBuilder()
                    .WithExchangeName(ExchangeNameDefaults.TOPIC)
                    .WithRoutingKey(sendDestination)
                    .WithMandatory(false)
                    .WithImmediate(false)
                    .Create();
                break;
    
            // Otherwise the receiver role is being assigned, so set this up to listen for messages on the required number
            // of receiver connections.
            case Roles.RECEIVER:
                // Create the required number of receiver connections.
                connection = new IConnection[numReceivers];
                channel = new IChannel[numReceivers];
    
                for (int i = 0; i < numReceivers; i++)
                {
                    connection[i] =
                        TestClient.CreateConnection(TestClient.brokerUrl, TestClient.virtualHost);
                    channel[i] = connection[i].CreateChannel(false, AcknowledgeMode.AutoAcknowledge);

                    IMessageConsumer consumer = channel[i].CreateConsumerBuilder(sendDestination).Create();
                    consumer.OnMessage += new MessageReceivedDelegate(OnMessage);
                }
    
                break;
            }
    
            // Start all the connection dispatcher threads running.
            foreach (IConnection con in connection)
            {
                con.Start();
            }
        }
    
        /// <summary>
        /// Performs the test case actions.
        /// </summary>
        public void Start() 
        {
            log.Debug("public void Start(): called");
    
            // Check that the sender role is being performed.
            if (role == Roles.SENDER)
            {
                IMessage testMessage = channel[0].CreateTextMessage("test");
    
                for (int i = 0; i < numMessages; i++)
                {
                    publisher.Send(testMessage);
    
                    // Increment the message count.
                    messageCount++;
                }
            }
        }
    
        /// <summary>
        /// Gets a report on the actions performed by the test case in its assigned role.
        /// </summary>
        ///
        /// <param name="session"> The session to create the report message in. </param>
        ///
        /// <returns> The report message. </returns>
        public IMessage GetReport(IChannel channel) 
        {
            log.Debug("public IMessage getReport(IChannel channel): called");
    
            // Close the test connections.
            /*foreach (IConnection con in connection)
            {
                try
                {
                    con.Stop();
                }
                catch (AMQConnectionClosedException e)
                {
                    // The connection has already died due to an error. Log this as a warning.
                    log.Warn("Connection already closed.");
                }
            }*/
    
            // Generate a report message containing the count of the number of messages passed.
            IMessage report = channel.CreateMessage();
            //report.Headers.SetString("CONTROL_TYPE", "REPORT");
            report.Headers.SetInt("MESSAGE_COUNT", messageCount);
    
            return report;
        }
    
        /// <summary>
        /// Counts incoming test messages.
        /// </summary>
        ///
        /// <param name="message"> The incoming test message. </param>
        public void OnMessage(IMessage message)
        {
            log.Debug("public void onMessage(IMessage message = " + message + "): called");
    
            // Increment the message count.
            messageCount++;
        }
    }
}
