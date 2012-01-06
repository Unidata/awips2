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
using Apache.Qpid.Client;

namespace Apache.Qpid.Integration.Tests.testcases
{
    /// <summary>
    /// CommitRollbackTest
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Check that an uncommitted send cannot be received.
    /// <tr><td> Check that a committed send can be received.
    /// <tr><td> Check that a rolled back send cannot be received.
    /// <tr><td> Check that an uncommitted receive can be re-received.
    /// <tr><td> Check that a committed receive cannot be re-received.
    /// <tr><td> Check that a rolled back receive can be re-received.
    /// </table>
    /// </summary>
    [TestFixture, Category("Integration")]
    public class CommitRollbackTest : BaseMessagingTestFixture
    {
        /// <summary>Used for debugging purposes.</summary>
        private static ILog log = LogManager.GetLogger(typeof(CommitRollbackTest));

        /// <summary>Defines the name of the test topic to use with the tests.</summary>
        public const string TEST_ROUTING_KEY = "commitrollbacktestkey";
        
        /// <summary>Used to count test messages received so far.</summary>
        private int messageReceivedCount;

        /// <summary>Used to hold the expected number of messages to receive.</summary>
        private int expectedMessageCount;

        /// <summary>Monitor used to signal succesfull receipt of all test messages.</summary>
        AutoResetEvent finishedEvent;
        
        /// <summary>Flag used to indicate that all messages really were received, and that the test did not just time out. </summary>
        private bool allReceived;
        
        [SetUp]
        public override void Init()
        {
            base.Init();

            // Create one producer and one consumer, p2p, tx, consumer with queue bound to producers routing key.
            SetUpEndPoint(0, true, false, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, true, ExchangeNameDefaults.DIRECT, 
                          true, false, null);
            SetUpEndPoint(1, true, true, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, true, ExchangeNameDefaults.DIRECT, 
                          true, false, null);
            
            // Clear counts
            messageReceivedCount = 0;
            expectedMessageCount = 0;
            finishedEvent = new AutoResetEvent(false);
            allReceived = false;
        }

        [TearDown]
        public override void Shutdown()
        {
            try
            {
                // Clean up after the test.
                CloseEndPoint(0);
                CloseEndPoint(1);
            } 
            finally 
            {
                base.Shutdown();
            }
        }

        /// <summary> Check that an uncommitted send cannot be received. </summary>
        [Test]
        public void TestUncommittedSendNotReceived() 
        {
            // Send messages.
            testProducer[0].Send(testChannel[0].CreateTextMessage("A"));
          
            // Try to receive messages.
            ConsumeNMessagesOnly(0, "A", testConsumer[1]);
            testChannel[1].Commit();
        }
        
        /// <summary> Check that a committed send can be received. </summary>
        [Test]
        public void TestCommittedSendReceived() 
        {
            // Send messages.
            testProducer[0].Send(testChannel[0].CreateTextMessage("B"));
            testChannel[0].Commit();

            // Try to receive messages.
            ConsumeNMessagesOnly(1, "B", testConsumer[1]);
            testChannel[1].Commit();
        }

        /// <summary> Check that a rolled back send cannot be received. </summary>
        [Test]
        public void TestRolledBackSendNotReceived()
        {
            // Send messages.
            testProducer[0].Send(testChannel[0].CreateTextMessage("B"));
            testChannel[0].Rollback();

            // Try to receive messages.
            ConsumeNMessagesOnly(0, "B", testConsumer[1]);
            testChannel[1].Commit();
        }

        /// <summary> Check that an uncommitted receive can be re-received. </summary>
        [Test]
        public void TestUncommittedReceiveCanBeRereceived() 
        {
            // Create a third end-point as an alternative delivery route for the message.
            SetUpEndPoint(2, false, true, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, true, ExchangeNameDefaults.DIRECT, 
                          true, false, null);

            // Send messages.
            testProducer[0].Send(testChannel[0].CreateTextMessage("C"));
            testChannel[0].Commit();

            // Try to receive messages.
            ConsumeNMessagesOnly(1, "C", testConsumer[1]);

            // Close end-point 1 without committing the message, then re-open to consume again.
            CloseEndPoint(1);

            // Check that the message was released from the rolled back end-point an can be received on the alternative one instead.
            ConsumeNMessagesOnly(1, "C", testConsumer[2]);

            CloseEndPoint(2);
        }

        /// <summary> Check that a committed receive cannot be re-received. </summary>
        [Test]
        public void TestCommittedReceiveNotRereceived() 
        {
            // Send messages.
            testProducer[0].Send(testChannel[0].CreateTextMessage("D"));
            testChannel[0].Commit();

            // Try to receive messages.
            ConsumeNMessagesOnly(1, "D", testConsumer[1]);
            testChannel[1].Commit();

            // Try to receive messages.
            ConsumeNMessagesOnly(0, "D", testConsumer[1]);
        }

        /// <summary> Check that a rolled back receive can be re-received. </summary>
        [Test]
        public void TestRolledBackReceiveCanBeRereceived() 
        {
            // Send messages.
            testProducer[0].Send(testChannel[0].CreateTextMessage("E"));
            testChannel[0].Commit();
            
            // Try to receive messages.
            ConsumeNMessagesOnly(1, "E", testConsumer[1]);

            testChannel[1].Rollback();

            // Try to receive messages.
            ConsumeNMessagesOnly(1, "E", testConsumer[1]);
            
        }
        
        [Test]
        public void TestReceiveAndSendRollback()
        {
        	// Send messages
        	testProducer[0].Send(testChannel[0].CreateTextMessage("F"));
            testChannel[0].Commit();
            
            // Try to receive messages.
            ConsumeNMessagesOnly(1, "F", testConsumer[1]);
			testProducer[1].Send(testChannel[1].CreateTextMessage("G"));
            testChannel[1].Rollback();

            // Try to receive messages.
            ConsumeNMessagesOnly(1, "F", testConsumer[1]);
            
        }
        
        [Test]
        public void TestReceivePrePublished()
        {
        	// Send messages
        	for (int i = 0; i < 10; ++i)
            {
	        	testProducer[0].Send(testChannel[0].CreateTextMessage("G"+i));
	        	testChannel[0].Commit();
        	}
        	
        	for (int i = 0; i < 10; ++i)
            {
        		ConsumeNMessages(1, "G"+i, testConsumer[1]);
        	}
        	testChannel[1].Commit();
        }
        
        [Test]
        public void TestReceivePrePublishedOnMessageHandler()
        {
        	testConsumer[1].OnMessage += new MessageReceivedDelegate(OnMessage);
        	// Send messages
        	for (int i = 0; i < 10; ++i)
            {
	        	testProducer[0].Send(testChannel[0].CreateTextMessage("G"+i));
	        	testChannel[0].Commit();
        	}
        	expectedMessageCount = 10;
        		
            finishedEvent.WaitOne(new TimeSpan(0, 0, 0, 30), false);

            // Check that all messages really were received.
            Assert.IsTrue(allReceived, "All messages were not received, only got: " + messageReceivedCount + " but wanted " + expectedMessageCount);
        
        	testChannel[1].Commit();
        }
        
        /// <summary> Atomically increments the message count on every message, and signals once all messages in the test are received. </summary>
        public void OnMessage(IMessage m)
        {
            int newCount = Interlocked.Increment(ref messageReceivedCount);

            if (newCount >= expectedMessageCount)
            {
                allReceived = true;
                finishedEvent.Set();
            }
        } 
        
    }
}
