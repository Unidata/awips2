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
using NUnit.Framework;
using Apache.Qpid.Messaging;
using Apache.Qpid.Client.Qms;

namespace Apache.Qpid.Integration.Tests.testcases
{
    /// <summary>
    /// DurableSubscriptionTest checks that durable subscriptions work, by sending messages that can be picked up by
    /// a subscription that is currently off-line, and checking that the subscriber gets all of its messages when it
    /// does come on-line.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> 
    /// </table>
    /// </summary>
    [TestFixture, Category("Integration")]
    public class DurableSubscriptionTest : BaseMessagingTestFixture
    {
        /// <summary>Used for debugging purposes.</summary>
        private static ILog log = LogManager.GetLogger(typeof(DurableSubscriptionTest));

        /// <summary>Defines the name of the test topic to use with the tests.</summary>
        public const string TEST_ROUTING_KEY = "durablesubtestkey";

        [SetUp]
        public override void Init()
        {
            base.Init();
        }

        [TearDown]
        public override void Shutdown()
        {
            base.Shutdown();
        }

        [Test]        
        public void TestDurableSubscriptionNoAck() 
        {
            TestDurableSubscription(AcknowledgeMode.NoAcknowledge);
        }

        [Test]
        public void TestDurableSubscriptionAutoAck()
        {
            TestDurableSubscription(AcknowledgeMode.AutoAcknowledge);
        }

        private void TestDurableSubscription(AcknowledgeMode ackMode)
        {
            // Create a topic with one producer and two consumers.
            SetUpEndPoint(0, true, false, TEST_ROUTING_KEY + testId, ackMode, false, ExchangeNameDefaults.TOPIC, true, false, null);
            SetUpEndPoint(1, false, true, TEST_ROUTING_KEY + testId, ackMode, false, ExchangeNameDefaults.TOPIC, true, false, null);
            SetUpEndPoint(2, false, true, TEST_ROUTING_KEY + testId, ackMode, false, ExchangeNameDefaults.TOPIC, true,
                          true, "TestSubscription" + testId);

            Thread.Sleep(500);

            // Send messages and receive on both consumers.
            testProducer[0].Send(testChannel[0].CreateTextMessage("A"));

            ConsumeNMessagesOnly(1, "A", testConsumer[1]);
            ConsumeNMessagesOnly(1, "A", testConsumer[2]);

            // Detach one consumer.
            CloseEndPoint(2);

            // Send message and receive on one consumer.
            testProducer[0].Send(testChannel[0].CreateTextMessage("B"));

            ConsumeNMessagesOnly(1, "B", testConsumer[1]);

            // Re-attach consumer, check that it gets the messages that it missed.
            SetUpEndPoint(2, false, true, TEST_ROUTING_KEY + testId, ackMode, false, ExchangeNameDefaults.TOPIC, true,
                          true, "TestSubscription" + testId);

            ConsumeNMessagesOnly(1, "B", testConsumer[2]);
			
            // Clean up any open consumers at the end of the test.
            CloseEndPoint(2);
            CloseEndPoint(1);
            CloseEndPoint(0);
        }

        /// <summary> Check that an uncommitted receive can be re-received, on re-consume from the same durable subscription. </summary>
        [Test]
        public void TestUncommittedReceiveCanBeRereceivedNewConnection() 
        {
            SetUpEndPoint(0, true, false, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, true, ExchangeNameDefaults.TOPIC,
                          true, false, null);
            SetUpEndPoint(1, false, true, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, true, ExchangeNameDefaults.TOPIC, 
                          true, true, "foo"+testId);

            // Send messages.
            testProducer[0].Send(testChannel[0].CreateTextMessage("C"));
            testChannel[0].Commit();

            // Try to receive messages, but don't commit them.
            ConsumeNMessagesOnly(1, "C", testConsumer[1]);

            // Close end-point 1 without committing the message, then re-open the subscription to consume again.
            CloseEndPoint(1);
            SetUpEndPoint(1, false, true, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, true, ExchangeNameDefaults.TOPIC, 
                          true, true, "foo"+testId);

            // Check that the message was released from the rolled back end-point an can be received on the alternative one instead.
            ConsumeNMessagesOnly(1, "C", testConsumer[1]);
			testChannel[1].Commit();
            CloseEndPoint(1);
            CloseEndPoint(0);
        }

        /// <summary> Check that a rolled back receive can be re-received, on re-consume from the same durable subscription. </summary>
        [Test]
        public void TestRolledBackReceiveCanBeRereceivedNewConnection() 
        {
            SetUpEndPoint(0, true, false, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, true, ExchangeNameDefaults.TOPIC, 
                          true, false, null);
            SetUpEndPoint(1, false, true, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, true, ExchangeNameDefaults.TOPIC, 
                          true, true, "foo"+testId);

            // Send messages.
            testProducer[0].Send(testChannel[0].CreateTextMessage("D"));
            testChannel[0].Commit();
            
            // Try to receive messages, but roll them back.
            ConsumeNMessagesOnly(1, "D", testConsumer[1]);
            testChannel[1].Rollback();

            // Close end-point 1 without committing the message, then re-open the subscription to consume again.
            CloseEndPoint(1);
            SetUpEndPoint(1, false, true, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, true, ExchangeNameDefaults.TOPIC, 
                          true, true, "foo"+testId);

            // Check that the message was released from the rolled back end-point an can be received on the alternative one instead.
            ConsumeNMessagesOnly(1, "D", testConsumer[1]);
			testChannel[1].Commit();
            CloseEndPoint(1);
            CloseEndPoint(0);
        }
    }
}
