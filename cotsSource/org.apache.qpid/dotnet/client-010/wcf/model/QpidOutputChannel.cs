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
using System.IO;
using System.ServiceModel;
using System.ServiceModel.Channels;
using org.apache.qpid.client;
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.wcf.model
{
    internal sealed class QpidOutputChannel : QpidOutputChannelBase
    {
        private readonly MessageEncoder _encoder;
        private readonly ClientSession _session;
        private readonly string _queueName;

        public QpidOutputChannel(BindingContext context, ClientSession session, EndpointAddress address)
            : base(context, address)
        {
            var encoderElement = context.Binding.Elements.Find<MessageEncodingBindingElement>();
            if (encoderElement != null)
            {
                _encoder = encoderElement.CreateMessageEncoderFactory().Encoder;
            }
            _queueName = address.Uri.ToString();
            _session = session;            
        }

        public override void Send(System.ServiceModel.Channels.Message message, TimeSpan timeout)
        {
            if (message.State != MessageState.Closed)
            {
                byte[] body;
                using (var str = new MemoryStream())
                {
                    _encoder.WriteMessage(message, str);
                    body = str.ToArray();
                }
                _session.messageTransfer("amq.direct", MessageAcceptMode.NONE, MessageAcquireMode.PRE_ACQUIRED,
                                         new Header(new DeliveryProperties().setRoutingKey(_queueName),
                                                    new transport.MessageProperties().setMessageId(UUID.randomUUID())),
                                         body);
            }
        }

        public override void Close(TimeSpan timeout)
        {
            if (State == CommunicationState.Closed || State == CommunicationState.Closing)
                return; // Ignore the call, we're already closing.
            OnClosing();
            OnClosed();
        }

        public override void Open(TimeSpan timeout)
        {
            if (State != CommunicationState.Created && State != CommunicationState.Closed)
                throw new InvalidOperationException(string.Format("Cannot open the channel from the {0} state.", State));
            OnOpening();
            var qr = (QueueQueryResult) _session.queueQuery(_queueName).Result;
            if (qr.getQueue() == null)
            {
                // create the queue 
                _session.queueDeclare(_queueName, null, null);
            }
            OnOpened();
        }
    }
}
