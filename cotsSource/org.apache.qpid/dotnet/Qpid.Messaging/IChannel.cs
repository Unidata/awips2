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

namespace Apache.Qpid.Messaging
{
    public delegate void MessageReceivedDelegate(IMessage msg);
    
    /// <summary>
    /// IChannel provides methods to access the commands in AMQP that operate at the channel level. This can be summarized as
    /// the ability to declare queues and exchanges, bind queues to exchanges, create messages of various types, declare transaction
    /// boundaries (commit and rollback), and to set up producers and consumers on the channel.
    ///
    /// <p/>You can create a channel by using the CreateChannel() method
    /// of the connection object.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Declare queues.
    /// <tr><td> Declare exchanges.
    /// <tr><td> Bind queues to exchanges.
    /// <tr><td> Create messages.
    /// <tr><td> Set up message consumers on the channel.
    /// <tr><td> Set up message producers on the channel.
    /// <tr><td> Commit the current transaction.
    /// <tr><td> Roll-back the current transaction.
    /// <tr><td> Close the channel.
    /// </table>
    /// </summary>
    public interface IChannel : IDisposable, ICloseable
    {
        /// <summary>
        /// Acknowledge mode for messages received.
        /// </summary>
        AcknowledgeMode AcknowledgeMode { get; }

        /// <summary>
        /// True if the channel should use transactions.
        /// </summary>
        bool Transacted { get; }

        /// <summary>
        /// Prefetch value to be used as the default for 
        /// consumers created on this channel.
        /// </summary>
        int DefaultPrefetch { get; }

        /// <summary>
        /// Prefetch low value to be used as the default for 
        /// consumers created on this channel.
        /// </summary>
        int DefaultPrefetchLow { get; }

        /// <summary>
        /// Prefetch high value to be used as the default for 
        /// consumers created on this channel.
        /// </summary>
        int DefaultPrefetchHigh { get; }

        /// <summary>
        /// Declare a new exchange.
        /// </summary>
        /// <param name="exchangeName">Name of the exchange</param>
        /// <param name="exchangeClass">Class of the exchange, from <see cref="ExchangeClassConstants"/></param>
        void DeclareExchange(string exchangeName, string exchangeClass);

        /// <summary>
        /// Declare a new exchange using the default exchange class.
        /// </summary>
        /// <param name="exchangeName">Name of the exchange</param>
        void DeleteExchange(string exchangeName);

        /// <summary>
        /// Declare a new queue with the specified set of arguments.
        /// </summary>
        /// <param name="queueName">Name of the queue</param>
        /// <param name="isDurable">True if the queue should be durable</param>
        /// <param name="isExclusive">True if the queue should be exclusive to this channel</param>
        /// <param name="isAutoDelete">True if the queue should be deleted when the channel closes</param>
        void DeclareQueue(string queueName, bool isDurable, bool isExclusive, bool isAutoDelete);

        /// <summary>
        /// Delete a queue with the specifies arguments.
        /// </summary>
        /// <param name="queueName">Name of the queue to delete</param>
        /// <param name="ifUnused">If true, the queue will not deleted if it has no consumers</param>
        /// <param name="ifEmpty">If true, the queue will not deleted if it has no messages</param>
        /// <param name="noWait">If true, the server will not respond to the method</param>
        void DeleteQueue(string queueName, bool ifUnused, bool ifEmpty, bool noWait);

        /// <summary>
        /// Generate a new Unique name to use for a queue.
        /// </summary>
        /// <returns>A unique name to this channel</returns>
        string GenerateUniqueName();

        /// <summary>
        /// Removes all messages from a queue.
        /// </summary>
        /// <param name="queueName">Name of the queue to delete</param>
        /// <param name="noWait">If true, the server will not respond to the method</param>
        void PurgeQueue(string queueName, bool noWait);
        
        /// <summary>
        /// Bind a queue to the specified exchange.
        /// </summary>
        /// <param name="queueName">Name of queue to bind</param>
        /// <param name="exchangeName">Name of exchange to bind to</param>
        /// <param name="routingKey">Routing key</param>
        void Bind(string queueName, string exchangeName, string routingKey);

        /// <summary>
        /// Bind a queue to the specified exchange.
        /// </summary>
        /// <param name="queueName">Name of queue to bind</param>
        /// <param name="exchangeName">Name of exchange to bind to</param>
        /// <param name="routingKey">Routing key</param>
        /// <param name="args">Table of arguments for the binding. Used to bind with a Headers Exchange</param>
        void Bind(string queueName, string exchangeName, string routingKey, IFieldTable args);

        /// <summary>
        /// Create a new empty message with no body.
        /// </summary>
        /// <returns>The new message</returns>
        IMessage CreateMessage();

        /// <summary>
        /// Create a new message of the specified MIME type.
        /// </summary>
        /// <param name="mimeType">The mime type to create</param>
        /// <returns>The new message</returns>
        IMessage CreateMessage(string mimeType);

        /// <summary>
        /// Creates a new message for bytes (application/octet-stream).
        /// </summary>
        /// <returns>The new message</returns>
        IBytesMessage CreateBytesMessage();

        /// <summary>
        /// Creates a new text message (text/plain) with empty content.
        /// </summary>
        /// <returns>The new message</returns>
        ITextMessage CreateTextMessage();

        /// <summary>
        /// Creates a new text message (text/plain) with a body.
        /// </summary>
        /// <param name="initialValue">Initial body of the message</param>
        /// <returns>The new message</returns>
        ITextMessage CreateTextMessage(string initialValue);

        #region Consuming
        
        /// <summary>
        /// Creates a new Consumer using the builder pattern.
        /// </summary>
        /// <param name="queueName">Name of queue to receive messages from</param>
        /// <returns>The builder object</returns>
        MessageConsumerBuilder CreateConsumerBuilder(string queueName);

        /// <summary>
        /// Creates a new consumer.
        /// </summary>
        /// <param name="queueName">Name of queue to receive messages from</param>
        /// <param name="prefetchLow">Low prefetch value</param>
        /// <param name="prefetchHigh">High prefetch value</param>
        /// <param name="noLocal">If true, messages sent on this channel will not be received by this consumer</param>
        /// <param name="exclusive">If true, the consumer opens the queue in exclusive mode</param>
        /// <returns>The new consumer</returns>
        IMessageConsumer CreateConsumer(string queueName,
                                        int prefetchLow, 
                                        int prefetchHigh,
                                        bool noLocal,
                                        bool exclusive);
        
        /// <summary>
        /// Unsubscribe from a queue.
        /// </summary>
        /// <param name="subscriptionName">Subscription name</param>
        void Unsubscribe(string subscriptionName);

        #endregion

        #region Publishing

        /// <summary>
        /// Create a new message publisher using the builder pattern.
        /// </summary>
        /// <returns>The builder object</returns>
        MessagePublisherBuilder CreatePublisherBuilder();
        
        /// <summary>
        /// Create a new message publisher.
        /// </summary>
        /// <param name="exchangeName">Name of exchange to publish to</param>
        /// <param name="routingKey">Routing key</param>
        /// <param name="deliveryMode">Default delivery mode</param>
        /// <param name="timeToLive">Default TTL time of messages</param>
        /// <param name="immediate">If true, sent immediately</param>
        /// <param name="mandatory">If true, the broker will return an error 
        /// (as a connection exception) if the message cannot be delivered</param>
        /// <param name="priority">Default message priority</param>
        /// <returns>The new message publisher</returns>
        IMessagePublisher CreatePublisher(string exchangeName,
                                        string routingKey,
                                        DeliveryMode deliveryMode,
                                        long timeToLive,
                                        bool immediate,
                                        bool mandatory,
                                        int priority);

        #endregion

        #region Transactions
        
        /// <summary>
        /// Recover after transaction failure.
        /// </summary>
        void Recover();

        /// <summary>
        /// Commit the transaction.
        /// </summary>
        void Commit();

        /// <summary>
        /// Rollback the transaction.
        /// </summary>
        void Rollback();

        #endregion
    }
}
