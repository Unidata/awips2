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
using System.ServiceModel;
using System.ServiceModel.Channels;
using System.Text;
using System.Threading;
using org.apache.qpid.client;
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.wcf.model
{
    internal sealed class QpidInputChannel : QpidInputChannelBase
    {
        private static readonly Logger _log = Logger.get(typeof (QpidInputChannel));

        private readonly QpidTransportBindingElement _bindingElement;
        private readonly MessageEncoder _encoder;
        private readonly ClientSession _session;
        private readonly string _queueName;
        private BlockingQueue _queue;
        private bool _closed = false;

        public QpidInputChannel(BindingContext context, ClientSession session, EndpointAddress address)
            : base(context, address)
        {
            _bindingElement = context.Binding.Elements.Find<QpidTransportBindingElement>();
            var encoderElem = context.BindingParameters.Find<MessageEncodingBindingElement>();
            if (encoderElem != null)
            {
                _encoder = encoderElem.CreateMessageEncoderFactory().Encoder;
            }
            _session = session;
            _queueName = address.Uri.ToString();
            _queue = new BlockingQueue();
        }


        public override System.ServiceModel.Channels.Message Receive(TimeSpan timeout)
        {
            _session.messageFlow("myDest", MessageCreditUnit.MESSAGE, 1);
            _session.sync();
            IMessage m = _queue.Dequeue();
            System.ServiceModel.Channels.Message result = null;
            if (m != null)
            {              
                var reader = new BinaryReader(m.Body, Encoding.UTF8);
                var body = new byte[m.Body.Length - m.Body.Position];
                reader.Read(body, 0, body.Length);
                try
                {
                    result = _encoder.ReadMessage(new MemoryStream(body),
                                                  (int) _bindingElement.MaxReceivedMessageSize);
                }
                catch(Exception e)
                {
                    Console.WriteLine(e.StackTrace);
                }
                result.Headers.To = LocalAddress.Uri;

                var ack = new RangeSet();
                // ack this message 
                ack.add(m.Id);
                _session.messageAccept(ack);
                _session.sync();
            }
            else
            {
                if(! _closed )
                {
                   return Receive(timeout); 
                }
            }
            return result;
        }

        public override bool TryReceive(TimeSpan timeout, out System.ServiceModel.Channels.Message message)
        {            
            message = Receive(timeout);
            return message != null;
        }

        public override bool WaitForMessage(TimeSpan timeout)
        {
            throw new NotImplementedException();
        }

        public override void Close(TimeSpan timeout)
        {
            _closed = true;
            _queue = null;
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
            // bind the queue 
            _session.exchangeBind(_queueName, "amq.direct", _queueName, null);
            var myListener = new WCFListener(_queue);
            _session.attachMessageListener(myListener, "myDest");
            _session.messageSubscribe(_queueName, "myDest", MessageAcceptMode.EXPLICIT, MessageAcquireMode.PRE_ACQUIRED,
                                      null,
                                      0, null);
            // issue credits     
            _session.messageSetFlowMode("myDest", MessageFlowMode.WINDOW);
            _session.messageFlow("myDest", MessageCreditUnit.BYTE, ClientSession.MESSAGE_FLOW_MAX_BYTES);
            _session.sync();

            OnOpened();
        }
    }

    internal class WCFListener : IMessageListener
    {
        private static readonly Logger _log = Logger.get(typeof (WCFListener));
        private readonly BlockingQueue _q;

        public WCFListener(BlockingQueue q)
        {
            _q = q;
        }

        public void messageTransfer(IMessage m)
        {
            _log.debug("message received by listener");
            _q.Enqueue(m);
        }
    }

    internal class BlockingQueue
    {
        private int _count;
        private readonly Queue<IMessage> _queue = new Queue<IMessage>();

        public IMessage Dequeue(TimeSpan timeout)
        {
            lock (_queue)
            {
                DateTime start = DateTime.Now;
                long elapsed = 0;
                while (_count <= 0 && elapsed < timeout.Milliseconds)
                {
                    Monitor.Wait(_queue, new TimeSpan(timeout.Milliseconds - elapsed));
                    elapsed = DateTime.Now.Subtract(start).Milliseconds;                 
                }
                if (_count > 0)
                {
                    _count--;
                    return _queue.Dequeue();
                }
                return null;
            }
        }

        public IMessage Dequeue()
        {
            lock (_queue)
            {               
                while (_count <= 0)
                {
                    Monitor.Wait(_queue);                   
                }
                if (_count > 0)
                {
                    _count--;
                    return _queue.Dequeue();
                }
                return null;
            }
        }

        public void Enqueue(IMessage data)
        {
            if (data != null)
            {
                lock (_queue)
                {
                    _queue.Enqueue(data);
                    _count++;
                    Monitor.Pulse(_queue);
                }
            }
        }
    }
}

