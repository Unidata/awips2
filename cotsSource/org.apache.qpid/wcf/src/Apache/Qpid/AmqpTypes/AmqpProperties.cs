/*
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
*/

namespace Apache.Qpid.AmqpTypes
{
    using System;
    using System.IO;
    using System.Collections.Generic;
    using System.Text;

    public class AmqpProperties
    {
        // AMQP 0-10 delivery properties
        private bool durable;
        private Nullable<TimeSpan> timeToLive;
        private string routingKey;

        // AMQP 0-10 message properties
        private string replyToExchange;
        private string replyToRoutingKey;
        private byte[] userId;
        private byte[] correlationId;
        private string contentType;

        // for application and vendor properties
        Dictionary<String, AmqpType> propertyMap;

        public AmqpProperties()
        {
        }

        // AMQP 0-10 "message.delivery-properties
        internal bool HasDeliveryProperties
        {
            get
            {
                return ((this.routingKey != null) || this.durable || this.timeToLive.HasValue);
            }
        }

        internal bool HasMappedProperties
        {
            get
            {
                if (this.propertyMap != null)
                {
                    if (this.propertyMap.Count > 0)
                    {
                        return true;
                    }
                }

                return false;
            }
        }

        // AMQP 0-10 "message.message-properties"
        internal bool HasMessageProperties
        {
            get
            {
                if ((this.replyToExchange != null) ||
                (this.replyToRoutingKey != null) ||
                (this.userId != null) ||
                (this.correlationId != null) ||
                (this.contentType != null))
                {
                    return true;
                }

                if (this.propertyMap == null)
                {
                    return false;
                }

                return (this.propertyMap.Count != 0);
            }
        }

        public Dictionary<String, AmqpType> PropertyMap
        {
            get
            {
                if (this.propertyMap == null)
                {
                    this.propertyMap = new Dictionary<string, AmqpType>();
                }
                return propertyMap;
            }
            set { this.propertyMap = value; }
        }

        internal bool Empty
        {
            get
            {
                if (this.HasDeliveryProperties || this.HasMessageProperties)
                {
                    return true;
                }
                return false;
            }
        }

        public string ContentType
        {
            get { return contentType; }
            // TODO: validate
            set { contentType = value; }
        }

        public byte[] CorrelationId
        {
            get { return correlationId; }
            set
            {
                if (value != null)
                {
                    if (value.Length > 65535)
                    {
                        throw new ArgumentException("CorrelationId too big");
                    }
                }
                correlationId = value;
            }
        }

        public byte[] UserId
        {
            get { return userId; }
            set
            {
                if (value != null)
                {
                    if (value.Length > 65535)
                    {
                        throw new ArgumentException("UserId too big");
                    }
                }
                userId = value;
            }
        }

        public TimeSpan? TimeToLive
        {
            get { return this.timeToLive; }
            set { this.timeToLive = value; }
        }

        public string RoutingKey
        {
            get { return this.routingKey; }
            set { this.routingKey = value; }
        }

        public string ReplyToExchange
        {
            get { return this.replyToExchange; }
        }

        public string ReplyToRoutingKey
        {
            get { return this.replyToRoutingKey; }
        }

        // this changes from 0-10 to 1.0
        public void SetReplyTo(string exchange, string routingKey)
        {
            if ((exchange == null && routingKey == null))
            {
                throw new ArgumentNullException("SetReplyTo");
            }

            this.replyToExchange = exchange;
            this.replyToRoutingKey = routingKey;
        }

        public bool Durable
        {
            get { return durable; }
            set { durable = value; }
        }

        public void Clear()
        {
            this.timeToLive = null;
            this.routingKey = null;
            this.replyToRoutingKey = null;
            this.replyToExchange = null;
            this.durable = false;
            this.contentType = null;
            this.userId = null;
            this.correlationId = null;
            this.propertyMap = null;
        }

        public AmqpProperties Clone()
        {
            // memberwise clone ok for string, byte[], and value types
            AmqpProperties clonedProps = (AmqpProperties)this.MemberwiseClone();

            // deeper copy for the dictionary
            if (this.propertyMap != null)
            {
                if (this.propertyMap.Count > 0)
                {
                    Dictionary<string, AmqpType> clonedDictionary = new Dictionary<string, AmqpType>(this.propertyMap.Count);
                    foreach (KeyValuePair<string, AmqpType> original in this.propertyMap)
                    {
                        clonedDictionary.Add(original.Key, original.Value.Clone());
                    }

                    clonedProps.propertyMap = clonedDictionary;
                }
                else
                {
                    clonedProps.propertyMap = null;
                }
            }
            return clonedProps;
        }

        // adds/replaces from the other AmqpProperty object.
        // just inserts references, i.e. provides shallow copy semantics (see Clone for deep copy)
        public void MergeFrom(AmqpProperties other)
        {
            if (other.timeToLive.HasValue)
            {
                this.timeToLive = other.timeToLive;
            }

            if ((other.replyToRoutingKey != null) || (other.replyToExchange != null))
            {
                this.replyToExchange = other.replyToExchange;
                this.replyToRoutingKey = other.replyToRoutingKey;
            }

            if (other.routingKey != null)
            {
                this.routingKey = other.routingKey;
            }

            if (other.durable)
            {
                this.durable = true;
            }

            if (other.contentType != null)
            {
                this.contentType = other.contentType;
            }

            if (other.correlationId != null)
            {
                this.correlationId = other.correlationId;
            }

            if (other.userId != null)
            {
                this.userId = other.userId;
            }

            if (other.propertyMap != null)
            {
                if (other.propertyMap.Count > 0)
                {
                    Dictionary<string, AmqpType> thisMap = this.PropertyMap;
                    foreach (KeyValuePair<string, AmqpType> kvp in other.propertyMap)
                    {
                        thisMap[kvp.Key] = kvp.Value;
                    }
                }
            }
        }
    }
}
