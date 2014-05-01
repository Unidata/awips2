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
using System.Collections.Generic;
using org.apache.qpid.transport;

namespace org.apache.qpid.client
{
    public interface IClientSession : ISession
    {
        void AttachMessageListener(IMessageListener listener, string Destination);
        Dictionary<String, IMessageListener> MessageListeners { get; }
        void MessageTransfer(String destination, string routingkey, IMessage message);
        void MessageTransfer(String destination, IMessage message);
        void QueueDeclare(String queue);
        void QueueDeclare(String queue, params Option[] options);
        void ExchangeBind(String queue, String exchange, String bindingKey);
        void MessageSubscribe(String queue);
    }
}
