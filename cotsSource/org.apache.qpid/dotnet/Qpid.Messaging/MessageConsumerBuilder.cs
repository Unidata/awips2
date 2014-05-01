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
namespace Apache.Qpid.Messaging
{
    /// <summary>
    /// MessageConsumerBuilder provides a builder with a fluent interface to assist with creating message consumers on a channel.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Create message consumers from consume parameters. <td> <see cref="IChannel">
    /// </table>
    /// </summary>
    ///
    /// <remarks>It may be better to replace the Create method with a DeclareBindAndCreate method, that declares and binds the consumers queue,
    /// as well as creating the consumer. This is a common use case, so the method will generally be usefull. Need to consider situations where
    /// the declare and bind is not to be done, for example when resubscribing to a durable subscription. There may be others too.</remarks>
    public class MessageConsumerBuilder
    {
        private bool _noLocal = false;

        private bool _exclusive = false;

        //private bool _durable = false;
        //private string _subscriptionName = null;

        private IChannel _channel;

        private readonly string _queueName;

        private int _prefetchLow;

        private int _prefetchHigh;
        
        public MessageConsumerBuilder(IChannel channel, string queueName)
        {
            _channel = channel;
            _queueName = queueName;
            _prefetchHigh = _channel.DefaultPrefetchHigh;
            _prefetchLow = _channel.DefaultPrefetchLow;
        }
        
        public MessageConsumerBuilder WithPrefetchLow(int prefetchLow)
        {
            _prefetchLow = prefetchLow;
            return this;
        }
        
        public MessageConsumerBuilder WithPrefetchHigh(int prefetchHigh)
        {
            _prefetchHigh = prefetchHigh;
            return this;
        }
        
        public MessageConsumerBuilder WithNoLocal(bool noLocal)
        {
            _noLocal = noLocal;
            return this;
        }
        
        public MessageConsumerBuilder WithExclusive(bool exclusive)
        {
            _exclusive = exclusive;
            return this;
        }
        
        /*
        public MessageConsumerBuilder WithDurable(bool durable)
        {
            _durable = durable;
            return this;
        }
        */
        
        /*
        public MessageConsumerBuilder WithSubscriptionName(string subscriptionName)
        {
            _subscriptionName = subscriptionName;
            return this;
        }
        */
        
        public IMessageConsumer Create()
        {
            return _channel.CreateConsumer(_queueName, _prefetchLow, _prefetchHigh, _noLocal, _exclusive);
        }
    }
}
