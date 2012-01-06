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
using System.IO;
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.client
{
    /// <summary> Implements a Qpid Sesion.</summary>
    public class ClientSession : Session, IClientSession
    {
        public static short TRANSFER_ACQUIRE_MODE_NO_ACQUIRE = 1;
        public static short TRANSFER_ACQUIRE_MODE_PRE_ACQUIRE = 0;
        public static short TRANSFER_CONFIRM_MODE_REQUIRED = 0;
        public static short TRANSFER_CONFIRM_MODE_NOT_REQUIRED = 1;
        public static short MESSAGE_FLOW_MODE_CREDIT = 0;
        public static short MESSAGE_FLOW_MODE_WINDOW = 1;
        public static short MESSAGE_FLOW_UNIT_MESSAGE = 0;
        public static short MESSAGE_FLOW_UNIT_BYTE = 1;
        public static long MESSAGE_FLOW_MAX_BYTES = 0xFFFFFFFF;
        public static short MESSAGE_REJECT_CODE_GENERIC = 0;
        public static short MESSAGE_REJECT_CODE_IMMEDIATE_DELIVERY_FAILED = 1;
        public static short MESSAGE_ACQUIRE_ANY_AVAILABLE_MESSAGE = 0;
        public static short MESSAGE_ACQUIRE_MESSAGES_IF_ALL_ARE_AVAILABLE = 1;

        private readonly Dictionary<String, IMessageListener> _listeners = new Dictionary<String, IMessageListener>();

        public ClientSession(byte[] name) : base(name)
        {
        }

        public void AttachMessageListener(IMessageListener listener, string Destination)
        {
            _listeners.Add(Destination, listener);
        }

        public Dictionary<String, IMessageListener> MessageListeners
        {
            get { return _listeners; }
        }

        public void MessageTransfer(String destination, string routingkey, IMessage message)
        {           
            message.DeliveryProperties.SetRoutingKey(routingkey);
            MessageTransfer(destination, message);
        }

        public void MessageTransfer(String destination, IMessage message)
        {           
            byte[] body = new byte[message.Body.Position];
            message.Body.Seek(0, SeekOrigin.Begin);
            message.Body.Read(body, 0, body.Length);
            message.MessageProperties.SetMessageId(UUID.RandomUuid());
            MessageTransfer(destination,
                            MessageAcceptMode.NONE,
                            MessageAcquireMode.PRE_ACQUIRED,
                            message.Header,
                            body);
        }

        public void QueueDeclare(String queue)
        {
            QueueDeclare(queue, null, null);
        }

        public void QueueDeclare(String queue, params Option[] options) 
        {
            QueueDeclare(queue, null, null, options);
        }

        public void ExchangeBind(String queue, String exchange, String bindingKey)
        {
            ExchangeBind(queue, exchange, bindingKey, null);
        }

         public void MessageSubscribe(String queue)
         {
             MessageSubscribe(queue, queue, MessageAcceptMode.EXPLICIT, MessageAcquireMode.PRE_ACQUIRED, null, 0, null);
             // issue credits     
             MessageSetFlowMode(queue, MessageFlowMode.WINDOW);
             MessageFlow(queue, MessageCreditUnit.BYTE, MESSAGE_FLOW_MAX_BYTES);
             MessageFlow(queue, MessageCreditUnit.MESSAGE, 10000);                                
         }
             
    }
}
