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
    public class MessagePublisherBuilder
    {
        /// <summary>
        /// Default value for immediate flag is false, i.e. a consumer does not need to be attached to a queue
        /// </summary>
        const bool DEFAULT_IMMEDIATE = false;

        /// <summary>
        /// Default value for mandatory flag is true, i.e. server will not silently drop messages where no queue is
        /// connected to the exchange for the message
        /// </summary>
        const bool DEFAULT_MANDATORY = true;

        IChannel _channel;
        string _exchangeName = null;
        string _routingKey = null;
        DeliveryMode _deliveryMode = DeliveryMode.Persistent;
        long _timeToLive;
        bool _immediate = DEFAULT_IMMEDIATE;
        bool _mandatory = DEFAULT_MANDATORY;
        int _priority = 0;

        public MessagePublisherBuilder(IChannel channel)
        {
            _channel = channel;
        }

        public MessagePublisherBuilder WithRoutingKey(string routingKey)
        {
            _routingKey = routingKey;
            return this;
        }

        public MessagePublisherBuilder WithExchangeName(string exchangeName)
        {
            _exchangeName = exchangeName;
            return this;
        }

        public MessagePublisherBuilder WithDeliveryMode(DeliveryMode deliveryMode)
        {
            _deliveryMode = deliveryMode;
            return this;
        }

        public MessagePublisherBuilder WithTimeToLive(long timeToLive)
        {
            _timeToLive = timeToLive;
            return this;
        }

        public MessagePublisherBuilder WithImmediate(bool immediate)
        {
            _immediate = immediate;
            return this;
        }

        public MessagePublisherBuilder WithMandatory(bool mandatory)
        {
            _mandatory = mandatory;
            return this;
        }

        public IMessagePublisher Create()
        {
            return _channel.CreatePublisher(_exchangeName, _routingKey, _deliveryMode, _timeToLive, _immediate, _mandatory, _priority);
        }
    }
}
