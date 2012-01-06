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
using System.IO;
using System.Reflection;
using System.Threading;
using NUnit.Framework;
using Apache.Qpid.Client.Qms;
using Apache.Qpid.Client;
using Apache.Qpid.Messaging;
using log4net;

namespace Apache.Qpid.Integration.Tests.testcases
{
	/// <summary>
	/// Runs through the range of ack modes for each test case, sending and recieving a large number of messages
	/// </summary>
	[TestFixture, Category("Integration")]
	public class SustainedTest : BaseMessagingTestFixture
	{
		/// <summary>The number of test messages to send.</summary>
        private const int MESSAGE_COUNT = 50;//00;
        
        /// <summary>Base name for the routing key used for this test (made unique by adding in test id).</summary>
        private const string TEST_ROUTING_KEY = "MessageOrderTest";
        
        /// <summary>
        /// The logger
        /// </summary>
        private static ILog _logger = LogManager.GetLogger(typeof(SustainedTest));
        
        [Test]
        public void MessageOrderTestAutoAck()
        {
        	MessageOrderTest(AcknowledgeMode.AutoAcknowledge);
        }
        
        [Test]
        public void MessageOrderTestNoAck()
        {
        	MessageOrderTest(AcknowledgeMode.NoAcknowledge);
        }
        
        public void MessageOrderTest(AcknowledgeMode consumerMode)
        {
        	
        	// Consumer
        	SetUpEndPoint(1, false, true, TEST_ROUTING_KEY, consumerMode, false, ExchangeNameDefaults.DIRECT,
                              true, false, null);
        	
        	
        	Console.WriteLine("Starting producer thread");
        	Thread prodThread = new Thread(new ThreadStart(SendMessages));
        	prodThread.Start();
        	
        	Thread.Sleep(2000);
        	Console.WriteLine("Starting consuming");
        	for (int i = 0; i < MESSAGE_COUNT; i++)
        	{
        		if ((i % 10) == 0)
        		{
        			Console.WriteLine("Consuming message "+i);
        		}
        		ConsumeNMessages(1, "Msg"+i, testConsumer[1]);
        	}
        	prodThread.Join();
        	CloseEndPoint(0);
        	CloseEndPoint(1);
        }
        
        private static void SendMessages()
        {
        	AMQConnection conn = new AMQConnection(QpidConnectionInfo.FromUrl(BaseMessagingTestFixture.connectionUri));
        	conn.Start();
        	IChannel channel = conn.CreateChannel(false, AcknowledgeMode.AutoAcknowledge);
        	IMessagePublisher producer = channel.CreatePublisherBuilder().
        		WithExchangeName(ExchangeNameDefaults.DIRECT).
        		WithRoutingKey(TEST_ROUTING_KEY).
        		Create();
        	
        	for (int i = 0; i < MESSAGE_COUNT ; i++)
        	{
        		if ((i % 10) == 0) 
        		{
        			Console.WriteLine("Sending message "+i);
        		}
        		producer.Send(channel.CreateTextMessage("Msg" + i));
        	}
        }
	}
}
