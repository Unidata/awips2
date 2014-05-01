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
using System.Collections;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Message
{
    public interface IMessageFactory
    {
        /// <summary>
        /// Create a message
        /// </summary>
        /// <param name="deliverTag">Delivery Tag</param>
        /// <param name="messageNbr">Message Sequence Number</param>
        /// <param name="redelivered">True if this is a redelivered message</param>
        /// <param name="contentHeader">Content headers</param>
        /// <param name="bodies">Message bodies</param>
        /// <returns>The new message</returns>
        /// <exception cref="QpidMessagingException">if the message cannot be created</exception>
        AbstractQmsMessage CreateMessage(long deliverTag, bool redelivered,
                                         ContentHeaderBody contentHeader,
                                         IList bodies);

        /// <summary>
        /// Creates the message.
        /// </summary>
        /// <param name="mimeType">Mime type to associate the new message with</param>
        /// <returns>The new message</returns>
        /// <exception cref="QpidMessagingException">if the message cannot be created</exception>
        AbstractQmsMessage CreateMessage(string mimeType);
    }
}


