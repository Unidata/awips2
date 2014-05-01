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
    public interface IMessage
    {
       /// <summary>
       /// The MIME Content Type
       /// </summary>
       string ContentType { get; set;}
       /// <summary>
       /// The MIME Content Encoding
       /// </summary>
       string ContentEncoding { get; set; }
       /// <summary>
       /// The application correlation identifier
       /// </summary>
       string CorrelationId { get; set; }
       /// <summary>
       /// The application correlation identifier, as an array of bytes
       /// </summary>
       byte[] CorrelationIdAsBytes { get; set; }
       /// <summary>
       /// Non-persistent (1) or persistent (2)
       /// </summary>
       DeliveryMode DeliveryMode { get; set; }
       /// <summary>
       /// Message expiration specification
       /// </summary>
       long Expiration { get; set; }
       /// <summary>
       /// The application message identifier
       /// </summary>
       string MessageId { get; set; }
       /// <summary>
       /// The message priority, 0 to 9
       /// </summary>
       byte Priority { get; set; }
       /// <summary>
       /// True if the message has been redelivered
       /// </summary>
        bool Redelivered { get; set; }
        /// <summary>
        /// Exchange name of the reply-to address
        /// </summary>
        string ReplyToExchangeName { get; set; }
        /// <summary>
        /// Routing key of the reply-to address
        /// </summary>
        string ReplyToRoutingKey { get; set; }
        /// <summary>
        /// The message timestamp
        /// </summary>
        long Timestamp { get; set; }
        /// <summary>
        /// The message type name
        /// </summary>
        string Type { get; set; }
        /// <summary>
        /// Message headers
        /// </summary>
        IHeaders Headers { get; }
        /// <summary>
        /// The creating user id
        /// </summary>
        string UserId { get; set; }
        /// <summary>
        /// The creating application id
        /// </summary>
        string AppId { get; set; }
        /// <summary>
        /// Intra-cluster routing identifier
        /// </summary>
        string ClusterId { get; set; }
        
        void Acknowledge();
        void ClearBody();
    }
}
