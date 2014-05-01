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
using Apache.Qpid.Framing;
using Apache.Qpid.Messaging;
using Apache.Qpid.Client.Qms;
using Apache.Qpid.Client;

namespace Apache.Qpid.Integration.Tests.testcases
{
    /// <summary>
    /// Sets up a producer/consumer pair to send test messages through a header exchange. The header exchange matching pattern is tested to
    /// verify that it correctly matches or filters out messages based on their headers.
    /// 
    /// Check that a message matching all fields of a headers exchange is passed by the exchange.
    /// Check that a message containing values for empty fields of a headers exchange is passed by the exchange.
    /// Check that a message matching only some fields of a headers exhcnage is not passed by the exchange.
    /// Check that a message with additional fields to the correct matching fields of a headers exchange is passed by the exchange.
    /// </summary>
    ///
    /// <todo>Remove the HeadersMatchingProducer class and rename this to HeaderExchangeTest. The producer and consumer are implemented
    ///       in a single test class to make running this as part of an automated test suite possible.</todo>
    /// 
    /// <todo>Consider not using a delegate to callback the OnMessage method. Easier to just call receive on the consumer but using the 
    ///       callback does demonstrate how to do so.</todo>
    [TestFixture, Category("Integration")]
    public class HeadersExchangeTest : BaseMessagingTestFixture
    {
        private static ILog _logger = LogManager.GetLogger(typeof(HeadersExchangeTest));

        /// <summary> Holds the default test timeout for broker communications before tests give up. </summary>
        private static readonly int TIMEOUT = 2000;

        /// <summary> Holds the name of the headers exchange to create to send test messages on. </summary>
        private string _exchangeName = "ServiceQ1";

        /// <summary> Used to preserve the most recent exception in case test cases need to examine it. </summary>
        private Exception _lastException = null;

        /// <summary> Used to preserve the most recent message from the test consumer. </summary>
        private IMessage _lastMessage = null;

        /// <summary> The test consumer to get messages from the broker with. </summary>
        private IMessageConsumer _consumer;

        private IMessagePublisher _publisher;

        private AutoResetEvent _evt = new AutoResetEvent(false);

        private MessageReceivedDelegate _msgRecDelegate;
        private ExceptionListenerDelegate _exceptionDelegate;

        /// <summary> Holds the test connection. </summary>
        protected IConnection _connection;

        /// <summary> Holds the test channel. </summary>
        protected IChannel _channel;

        [SetUp]
        public override void Init()
        {          
            // Ensure that the base init method is called. It establishes a connection with the broker.
            base.Init();   

            connectionInfo = QpidConnectionInfo.FromUrl(connectionUri);         
            _connection = new AMQConnection(connectionInfo);
            _channel = _connection.CreateChannel(false, AcknowledgeMode.AutoAcknowledge, 500, 300);

            _logger.Info("Starting...");
            _logger.Info("Exchange name is '" + _exchangeName + "'...");

            // Register this to listen for exceptions on the test connection.
            _exceptionDelegate = new ExceptionListenerDelegate(OnException);
            _connection.ExceptionListener += _exceptionDelegate;

            // Declare a new headers exchange with the name of the test service.
            _channel.DeclareExchange(_exchangeName, ExchangeClassConstants.HEADERS);

            // Create a non-durable, temporary (aka auto-delete), exclusive queue.
            string queueName = _channel.GenerateUniqueName();
            _channel.DeclareQueue(queueName, false, true, true);

            // Bind the queue to the new headers exchange, setting up some header patterns for the exchange to match.
            _channel.Bind(queueName, _exchangeName, null, CreatePatternAsFieldTable());

            // Create a test consumer to consume messages from the test exchange.
            _consumer = _channel.CreateConsumerBuilder(queueName)
                .WithPrefetchLow(100)
                .WithPrefetchHigh(500)
                .WithNoLocal(false) // make sure we get our own messages
                .Create();

            // Register this to listen for messages on the consumer.
            _msgRecDelegate = new MessageReceivedDelegate(OnMessage);
            _consumer.OnMessage += _msgRecDelegate;
            
            // Clear the most recent message and exception.
            _lastException = null;
            _lastMessage = null;

            _publisher = _channel.CreatePublisherBuilder()
                    .WithExchangeName(_exchangeName)
                    .WithMandatory(true)
                    .Create();

            _publisher.DeliveryMode = DeliveryMode.NonPersistent;

            // Start all channel
            _connection.Start();
        }

        /// <summary>
        /// Deregisters the on message delegate before closing the connection.
        /// </summary>
        [TearDown]
        public override void Shutdown()
        {
            _logger.Info("public void Shutdown(): called");

            //_consumer.OnMessage -= _msgRecDelegate;
            //_connection.ExceptionListener -= _exceptionDelegate;

            _connection.Stop();
            _connection.Close();
            _connection.Dispose();

            base.Shutdown();
        }

        /// <summary>
        /// Callback method that is passed any messages received on the test channel.
        /// </summary>
        /// 
        /// <param name="message">The received message.</param>
        public void OnMessage(IMessage message)
        {
            _logger.Debug(string.Format("message.Type = {0}", message.GetType()));
            _logger.Debug("Got message '" + message + "'");

            // Preserve the most recent exception so that test cases can examine it.
            _lastMessage = message;

            // Notify any waiting threads that a message has been received.
            _evt.Set();
        }

        /// <summary>Callback method to handle any exceptions raised by the test connection.</summary>
        /// 
        /// <param name="e">The connection exception.</param>
        public void OnException(Exception e)
        {
            // Preserve the most recent exception in case test cases need to examine it.
            _lastException = e;

            // Notify any waiting threads that an exception event has occurred.
            _evt.Set();
        }

        /// <summary>Check that a message matching all fields of a headers exchange is passed by the exchange.</summary>
        [Test]
        public void TestMatchAll()
        {
            IMessage msg = _channel.CreateTextMessage("matches match2=''");
            msg.Headers["match1"] = "foo";
            msg.Headers["match2"] = "";

            // Use the SendTestMessage helper method to verify that the message was sent and received.
            SendTestMessage(msg, true);
        }

        /// <summary>Check that a message containing values for empty fields of a headers exchange is passed by the exchange.</summary>
        [Test]
        public void TestMatchEmptyMatchesAnything()
        {
            // Send a test message that matches the headers exchange.
            IMessage msg = _channel.CreateTextMessage("matches match1='foo' and match2='bar'");
            msg.Headers["match1"] = "foo";
            msg.Headers["match2"] = "bar";

            // Use the SendTestMessage helper method to verify that the message was sent and received.
            SendTestMessage(msg, true);
        }

        /// <summary>Check that a message matching only some fields of a headers exchange is not passed by the exchange.</summary>
        [Test]
        public void TestMatchOneFails()
        {
            IMessage msg = _channel.CreateTextMessage("not match - only match1");
            msg.Headers["match1"] = "foo";

            // Use the SendTestMessage helper method to verify that the message was sent and not received.
            SendTestMessage(msg, false);
        }

        /// <summary>
        /// Check that a message with additional fields to the correct matching fields of a headers exchange is passed by 
        /// the exchange.
        /// </summary>
        [Test]
        public void TestMatchExtraFields()
        {
            IMessage msg = _channel.CreateTextMessage("matches - extra headers");
            msg.Headers["match1"] = "foo";
            msg.Headers["match2"] = "bar";
            msg.Headers["match3"] = "not required";

            // Use the SendTestMessage helper method to verify that the message was sent and received.
            SendTestMessage(msg, true);
        }

        /// <summary>
        /// Sends the specified message to the test publisher, and confirms that it was received by the test consumer or not
        /// depending on whether or not the message should be received by the consumer.
        /// 
        /// Any exceptions raised by the connection will cause an Assert failure exception to be raised.
        /// </summary>
        /// 
        /// <param name="msgSend">The message to send.</param>
        /// <param name="shouldPass">A flag to indicate whether or not the message should be received by the consumer.</param>
        private void SendTestMessage(IMessage msgSend, bool shouldPass)
        {
            _publisher.Send(msgSend);
            _evt.WaitOne(TIMEOUT, true);

            // Check that an exception other than not routable was raised in which case re-raise it as a test error.
            if (_lastException != null && !(_lastException.InnerException is AMQUndeliveredException))
            {
                Assert.Fail("Exception {0} was raised by the broker connection.", _lastException);
            }
            // Check that a message was returned if the test is expecting the message to pass.
            else if (shouldPass)
            {
                Assert.IsNotNull(_lastMessage, "Did not get a matching message from the headers exchange.");
            }
            // Check that a not routable exception was raised if the test is expecting the message to fail.
            else if (_lastException != null && _lastException.InnerException is AMQUndeliveredException)
            {
                Assert.IsNull(_lastMessage, "Message could not be routed so consumer should not have received it.");
            }
            // The broker did not respond within the test timeout so fail the test.
            else
            {
                Assert.Fail("The test timed out without a response from the broker.");
            }
        }

        /// <summary> Returns a field table containing patterns to match the test header exchange against. </summary>
        /// 
        /// <returns> A field table containing test patterns. </returns>
        private FieldTable CreatePatternAsFieldTable()
        {
            FieldTable matchTable = new FieldTable();

            matchTable["match1"] = "foo";
            matchTable["match2"] = "";
            matchTable["x-match"] = "all";

            return matchTable;
        }
    }
}
