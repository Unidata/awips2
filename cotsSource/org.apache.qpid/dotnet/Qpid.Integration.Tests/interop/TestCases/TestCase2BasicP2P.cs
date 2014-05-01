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
    /// Implements test case 2, basic P2P. Sends/receives a specified number of messages to a specified route on the
    /// default direct exchange. Produces reports on the actual number of messages sent/received.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Supply the name of the test case that this implements.
    /// <tr><td> Accept/Reject invites based on test parameters.
    /// <tr><td> Adapt to assigned roles.
    /// <tr><td> Send required number of test messages.
    /// <tr><td> Generate test reports.
    /// </table>
    /// </summary>
    public class TestCase2BasicP2P : InteropClientTestCase
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(TestCase2BasicP2P));

        /// <summary> Holds the count of test messages received. </summary>
        private int messageCount;

        /// <summary> The role to be played by the test. </summary>
        private Roles role;

        /// <summary> The number of test messages to send. </summary>
        private int numMessages;

        /// <summary> The routing key to send them to on the default direct exchange. </summary>
        private string sendDestination;

        /// <summary> The connection to send the test messages on. </summary>
        private IConnection connection;

        /// <summary> The session to send the test messages on. </summary>
        private IChannel channel;

        /// <summary> The producer to send the test messages with. </summary>
        private IMessagePublisher publisher;

        /// <summary>
        /// Should provide the name of the test case that this class implements. The exact names are defined in the
        /// interop testing spec.
        /// </summary>
        ///
        /// <returns> The name of the test case that this implements. </returns>
        public String GetName()
        {
            log.Debug("public String GetName(): called");
    
            return "TC2_BasicP2P";
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
            log.Debug("public boolean AcceptInvite(Message inviteMessage = " + inviteMessage + "): called");

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
            log.Debug("public void AssignRole(Roles role = " + role + ", Message assignRoleMessage = " + assignRoleMessage
                + "): called");
    
            // Reset the message count for a new test.
            messageCount = 0;

            // Take note of the role to be played.
            this.role = role;

            // Create a new connection to pass the test messages on.
            connection =
                TestClient.CreateConnection(TestClient.brokerUrl, TestClient.virtualHost);
            channel = connection.CreateChannel(false, AcknowledgeMode.AutoAcknowledge);

            // Extract and retain the test parameters.
            numMessages = assignRoleMessage.Headers.GetInt("P2P_NUM_MESSAGES");
            string queueAndKeyName = assignRoleMessage.Headers.GetString("P2P_QUEUE_AND_KEY_NAME");
            channel.DeclareQueue(queueAndKeyName, false, true, true);
            channel.Bind(queueAndKeyName, ExchangeNameDefaults.DIRECT, queueAndKeyName);
            sendDestination = queueAndKeyName;

            log.Debug("numMessages = " + numMessages);
            log.Debug("sendDestination = " + sendDestination);
            log.Debug("role = " + role);

            switch (role)
            {
            // Check if the sender role is being assigned, and set up a message producer if so.
            case Roles.SENDER:
                publisher = channel.CreatePublisherBuilder()
                .WithExchangeName(ExchangeNameDefaults.DIRECT)
                .WithRoutingKey(sendDestination)
                .Create();
                break;

            // Otherwise the receiver role is being assigned, so set this up to listen for messages.
            case Roles.RECEIVER:
                IMessageConsumer consumer = channel.CreateConsumerBuilder(sendDestination).Create();
                consumer.OnMessage += new MessageReceivedDelegate(OnMessage);

                break;
            }

            connection.Start();
        }

        /// <summary> Performs the test case actions. </summary>
        public void Start()
        {
            log.Debug("public void start(): called");

            // Check that the sender role is being performed.
            if (role == Roles.SENDER)
            {
                IMessage testMessage = channel.CreateTextMessage("test");

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
            log.Debug("public Message GetReport(IChannel channel): called");

            // Close the test connection.
            //connection.Stop();

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
            log.Debug("public void OnMessage(IMessage message = " + message + "): called");

            // Increment the message count.
            messageCount++;
        }
    }
}
