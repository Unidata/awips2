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
    /// MandatoryMessageTest checks that messages sent with the 'mandatory' flag, must either be routed to a valid
    /// queue or returned to the sender when no route is available.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Check default exchange returns unroutable mandatory messages.
    /// <tr><td> Check direct exchange returns unroutable mandatory messages.
    /// <tr><td> Check headers exchange returns unroutable mandatory messages.
    /// <tr><td> Check topic exchange returns unroutable mandatory messages.
    /// </table>
    /// </summary>
    [TestFixture, Category("Integration")]
    public class MandatoryMessageTest : BaseMessagingTestFixture
    {
        /// <summary>Used for debugging purposes.</summary>
        private static ILog log = LogManager.GetLogger(typeof(MandatoryMessageTest));

        /// <summary>Defines the maximum time in milliseconds, to wait for redelivery to occurr.</summary>
        public const int TIMEOUT = 1000;

        /// <summary>Defines the name of the routing key to use with the tests.</summary>
        public const string TEST_ROUTING_KEY = "unboundkey";

        /// <summary>Condition used to coordinate receipt of redelivery exception to the sending thread.</summary>
        private ManualResetEvent errorEvent;

        /// <summary>Holds the last received error condition, for examination by the tests sending thread.</summary>
        private Exception lastErrorException;

        /// <summary> Holds the test connection. </summary>
        protected IConnection _connection;

        /// <summary> Holds the test channel. </summary>
        protected IChannel _channel;
        
        [SetUp]
        public override void Init()
        {
            base.Init();

            errorEvent = new ManualResetEvent(false);
            lastErrorException = null;
        }

        [TearDown]
        public override void Shutdown()
        {
            base.Shutdown();
        }
        
        /// <summary>
        /// Handles all exception conditions on the connection. The error event is notified and the exception recorded as the last seen.
        /// </summary>
        ///
        /// <param name="e">The asynchronous exception on the connection.</param>
        public void OnException(Exception e)
        {
            lastErrorException = e;
            errorEvent.Set();
        }
        
        [Test]
        public void SendUndeliverableMessageOnDirectExchange()
        {
            SendOne(ExchangeNameDefaults.DIRECT);
        }
        
        [Test]
        public void SendUndeliverableMessageOnTopicExchange()
        {
            SendOne(ExchangeNameDefaults.TOPIC);
        }
        
        [Test]
        public void SendUndeliverableMessageOnHeadersExchange()
        {
            SendOne(ExchangeNameDefaults.HEADERS);
        }
        
        /// <summary>
        /// Sends a single message to the specified exchange with the routing key 'unboundkey', marked as mandatory.
        /// A check is performed to assert that a redelivery error is returned from the broker for the message.
        /// </summary>
        ///
        /// <param name="exchangeName">The name of the exchange to send to.</param>
        private void SendOne(string exchangeName)
        {
            log.Debug("private void SendOne(string exchangeName = " + exchangeName + "): called");

            // Send a test message to a unbound key on the specified exchange.
            SetUpEndPoint(0, false, false, TEST_ROUTING_KEY + testId, AcknowledgeMode.AutoAcknowledge, false, exchangeName,
                          true, false, null);            
            testProducer[0] = testChannel[0].CreatePublisherBuilder()
                .WithRoutingKey(TEST_ROUTING_KEY + testId)
                .WithMandatory(true)
                .WithExchangeName(exchangeName)
                .Create();

            // Set up the exception listener on the connection.
            testConnection[0].ExceptionListener = new ExceptionListenerDelegate(OnException);

            // Send message that should fail.
            testProducer[0].Send(testChannel[0].CreateTextMessage("Test Message"));
            
            // Wait for up to the timeout for a redelivery exception to be returned.
            errorEvent.WaitOne(TIMEOUT, true);

            // Asserts that a redelivery exception was returned, and is of the correct type.
            Type expectedException = typeof(AMQUndeliveredException);
            Exception ex = lastErrorException;

            Assert.IsNotNull(ex, "No exception was thrown by the test. Expected " + expectedException);
            Assert.IsInstanceOfType(expectedException, ex.InnerException);

            CloseEndPoint(0);
        }
    }
}
