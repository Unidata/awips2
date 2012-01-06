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
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Message
{
    public class AMQMessage
    {
        protected IContentHeaderProperties _contentHeaderProperties;

        /// <summary>
        /// If the acknowledge mode is CLIENT_ACKNOWLEDGE the session is required
        /// </summary>
        protected AmqChannel _channel;

        private long _deliveryTag;

        public AMQMessage(IContentHeaderProperties properties, long deliveryTag)
        {
            _contentHeaderProperties = properties;
            _deliveryTag = deliveryTag;
        }

        public AMQMessage(IContentHeaderProperties properties)
            : this(properties, -1)
        {
        }

        public long DeliveryTag
        {
            get { return _deliveryTag; }
        }

        public AmqChannel Channel
        {
            get { return _channel; }
            set { _channel = value; }
        }
    }
}
