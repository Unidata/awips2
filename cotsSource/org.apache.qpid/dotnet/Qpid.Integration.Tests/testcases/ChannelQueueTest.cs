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
using System.Net;
using System.Threading;
using log4net;
using Apache.Qpid.Client.Qms;
using Apache.Qpid.Client;
using Apache.Qpid.Messaging;
using NUnit.Framework;

namespace Apache.Qpid.Integration.Tests.testcases
{
    /// <summary>
    /// Test the queue methods
    /// </summary>
    [TestFixture, Category("Integration")]
    public class ChannelQueueTest
    {
        private static ILog _logger = LogManager.GetLogger(typeof(ChannelQueueTest));

        /// <summary> The default AMQ connection URL to use for tests. </summary>
        const string DEFAULT_URI = "amqp://guest:guest@default/test?brokerlist='tcp://localhost:5672'";
        const string _routingKey = "ServiceQ1";

        private ExceptionListenerDelegate _exceptionDelegate;
        private AutoResetEvent _evt = new AutoResetEvent(false);
        private Exception _lastException = null;

        private IMessageConsumer _consumer;
        private IMessagePublisher _publisher;
        private IChannel _channel;
        private IConnection _connection;

        private string _queueName;

        [SetUp]
        public virtual void Init()
        {
            _logger.Info("public virtual void Init(): called");

            // Create a connection to the broker.
            IConnectionInfo connectionInfo = QpidConnectionInfo.FromUrl(DEFAULT_URI);
            _connection = new AMQConnection(connectionInfo);
            _logger.Info("Starting...");

            // Register this to listen for exceptions on the test connection.
            _exceptionDelegate = new ExceptionListenerDelegate(OnException);
            _connection.ExceptionListener += _exceptionDelegate;

            // Establish a session on the broker.
            _channel = _connection.CreateChannel(false, AcknowledgeMode.AutoAcknowledge, 1);

            // Create a durable, non-temporary, non-exclusive queue.
            _queueName = _channel.GenerateUniqueName();
            _channel.DeclareQueue(_queueName, true, false, false);

            _channel.Bind(_queueName, ExchangeNameDefaults.TOPIC, _routingKey);

            // Clear the most recent message and exception.
            _lastException = null;
        }

        [TearDown]
        public virtual void ShutDown()
        {
            _logger.Info("public virtual void Shutdown(): called");

            if (_connection != null)
            {
                _logger.Info("Disposing connection.");
                _connection.Dispose();
                _logger.Info("Connection disposed.");
            }
        }
        
        [Test]
        public void DeleteUsedQueue()
        {
            // Create the consumer
            _consumer = _channel.CreateConsumerBuilder(_queueName)
                    .WithPrefetchLow(100)
                    .Create();
            _logger.Info("Consumer was created...");

            // delete the queue
            _channel.DeleteQueue(_queueName, false, true, true);
            _logger.InfoFormat("Queue {0} was delete", _queueName);

            Assert.IsNull(_lastException);
        }

        [Test]
        public void DeleteUnusedQueue()
        {
            // delete the queue
            _channel.DeleteQueue(_queueName, true, true, true);
            _logger.InfoFormat("Queue {0} was delete", _queueName);

            Assert.IsNull(_lastException);
        }

        [Test]
        public void DeleteNonEmptyQueue()
        {
            // Create the publisher
            _publisher = _channel.CreatePublisherBuilder()
                .WithExchangeName(ExchangeNameDefaults.TOPIC)
                .WithRoutingKey(_routingKey)
                .Create();
            _logger.Info("Publisher created...");
            SendTestMessage("DeleteNonEmptyQueue Message 1");

            try
            {
                _channel.DeleteQueue(_queueName, true, false, true);
            }
            catch (AMQException)
            {
                Assert.Fail("The test fails");
            }            
        }

        [Test]
        public void DeleteEmptyQueue()
        {
            // Create the publisher
            _publisher = _channel.CreatePublisherBuilder()
                .WithExchangeName(ExchangeNameDefaults.TOPIC)
                .WithRoutingKey(_routingKey)
                .Create();
            _logger.Info("Publisher created...");

            // delete an empty queue with ifEmpty = true
            _channel.DeleteQueue(_queueName, false, true, true);

            Assert.IsNull(_lastException);
        }

        [Test]
        public void DeleteQueueWithResponse()
        {
            // Create the publisher
            _publisher = _channel.CreatePublisherBuilder()
                .WithExchangeName(ExchangeNameDefaults.TOPIC)
                .WithRoutingKey(_routingKey)
                .Create();
            _logger.Info("Publisher created...");

            SendTestMessage("DeleteQueueWithResponse Message 1");
            SendTestMessage("DeleteQueueWithResponse Message 2");
            
            // delete the queue, the server must respond
            _channel.DeleteQueue(_queueName, false, false, false);
        }

        [Test]
        public void PurgeQueueWithResponse()
        {
            _publisher = _channel.CreatePublisherBuilder()
                .WithExchangeName(ExchangeNameDefaults.TOPIC)
                .WithRoutingKey(_routingKey)
                .Create();
            _logger.Info("Pubisher created");

            SendTestMessage("Message 1");
            SendTestMessage("Message 2");

            _channel.PurgeQueue(_queueName, false);
        }

        [Test]
        public void PurgeQueueWithOutResponse()
        {
            _publisher = _channel.CreatePublisherBuilder()
                .WithExchangeName(ExchangeNameDefaults.TOPIC)
                .WithRoutingKey(_routingKey)
                .Create();
            _logger.Info("Pubisher created");

            SendTestMessage("Message 1");
            SendTestMessage("Message 2");

            _channel.PurgeQueue(_queueName, true);
        }


        /// <summary>
        /// Callback method to handle any exceptions raised by the test connection.</summary>        /// 
        /// <param name="e">The connection exception.</param>
        public void OnException(Exception e)
        {
            // Preserve the most recent exception in case test cases need to examine it.
            _lastException = e;

            // Notify any waiting threads that an exception event has occurred.
            _evt.Set();
        }

        /// <summary>
        /// Sends the specified message to the test publisher, and confirms that it was received by the test consumer or not
        /// depending on whether or not the message should be received by the consumer.
        /// 
        /// Any exceptions raised by the connection will cause an Assert failure exception to be raised.
        /// </summary>
        /// 
        /// <param name="msgSend">The message to send.</param>
        private void SendTestMessage(string msg)
        {
            // create the IMessage object
            IMessage msgSend = _channel.CreateTextMessage(msg);

            // send the message
            _publisher.Send(msgSend);
            _logger.InfoFormat("The messages \"{0}\" was sent", msg);
        }

    }
}
