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
using System.Text;
using System.Threading;
using NUnit.Framework;
using org.apache.qpid.client;
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;

namespace test.interop
{
    public class Message : TestCase
    {
        private static readonly Logger _log = Logger.Get(typeof (Message));

        [Test]
        public void sendAndPurge()
        {
            _log.Debug("Running: ExchangeBind");
            IClientSession ssn = Client.CreateSession(0);
            ssn.QueueDelete("queue1");
            QueueQueryResult result = (QueueQueryResult) ssn.QueueQuery("queue1").Result;
            Assert.IsNull(result.GetQueue());
            ssn.QueueDeclare("queue1", null, null);
            ssn.ExchangeBind("queue1", "amq.direct", "queue1", null);

            for (int i = 0; i < 10; i++)
            {
                ssn.MessageTransfer("amq.direct", MessageAcceptMode.NONE, MessageAcquireMode.PRE_ACQUIRED,
                                    new Header(new DeliveryProperties().SetRoutingKey("queue1"),
                                               new MessageProperties().SetMessageId(UUID.RandomUuid())),
                                    Encoding.UTF8.GetBytes("test: " + i));
            }
            ssn.Sync();
            result = (QueueQueryResult) ssn.QueueQuery("queue1").Result;
            Assert.IsTrue(result.GetMessageCount() == 10);
            ssn.QueuePurge("queue1");
            ssn.Sync();
            result = (QueueQueryResult) ssn.QueueQuery("queue1").Result;
            Assert.IsTrue(result.GetMessageCount() == 0);
        }

        [Test]
        public void sendAndReceiveSmallMessages()
        {
            _log.Debug("Running: sendAndReceiveSmallMessages");
            byte[] smallMessage = Encoding.UTF8.GetBytes("test");
            sendAndReceive(smallMessage, 100);
        }

        [Test]
        public void sendAndReceiveLargeMessages()
        {
            _log.Debug("Running: sendAndReceiveSmallMessages");
            byte[] largeMessage = new byte[100 * 1024];
            Random random = new Random();
            random.NextBytes(largeMessage);
            sendAndReceive(largeMessage, 10);
        }

        [Test]
        public void sendAndReceiveVeryLargeMessages()
        {
            _log.Debug("Running: sendAndReceiveSmallMessages");
            byte[] verylargeMessage = new byte[1000 * 1024];
            Random random = new Random();
            random.NextBytes(verylargeMessage);
            sendAndReceive(verylargeMessage, 2);
        }

        private void sendAndReceive(byte[] messageBody, int count)
        {           
            IClientSession ssn = Client.CreateSession(0);
            ssn.Sync();
            ssn.QueueDeclare("queue1", null, null);
            ssn.QueueDelete("queue1");           
            QueueQueryResult result = (QueueQueryResult) ssn.QueueQuery("queue1").Result;            
            Assert.IsNull(result.GetQueue());
            ssn.QueueDeclare("queue1", null, null);
            ssn.ExchangeBind("queue1", "amq.direct", "queue1", null);
            Object myLock = new Object();
            MyListener myListener = new MyListener(myLock, count);
            ssn.AttachMessageListener(myListener, "myDest");

            ssn.MessageSubscribe("queue1", "myDest", MessageAcceptMode.EXPLICIT, MessageAcquireMode.PRE_ACQUIRED, null,
                                 0, null);


            // issue credits     
            ssn.MessageSetFlowMode("myDest", MessageFlowMode.WINDOW);
            ssn.MessageFlow("myDest", MessageCreditUnit.BYTE, ClientSession.MESSAGE_FLOW_MAX_BYTES);
            ssn.MessageFlow("myDest", MessageCreditUnit.MESSAGE, 10000);
            ssn.Sync();

            for (int i = 0; i < count; i++)
            {
                ssn.MessageTransfer("amq.direct", MessageAcceptMode.NONE, MessageAcquireMode.PRE_ACQUIRED,
                                    new Header(new DeliveryProperties().SetRoutingKey("queue1"),
                                               new MessageProperties().SetMessageId(UUID.RandomUuid())),
                                    messageBody);
            }
            ssn.Sync();
           
            lock (myLock)
            {
                if (myListener.Count != 0)
                {
                    Monitor.Wait(myLock, 10000000);
                }
            }
            Assert.IsTrue(myListener.Count == 0);
            ssn.MessageAccept(myListener.UnAck);
            ssn.Sync();
            // the queue should be empty 
            result = (QueueQueryResult)ssn.QueueQuery("queue1").Result;
            Assert.IsTrue(result.GetMessageCount() == 0);
            ssn.Close();        
        }



        private class MyListener : IMessageListener
        {
            private static readonly Logger _log = Logger.Get(typeof (MyListener));
            private readonly Object _wl;
            private int _count;
            private RangeSet _rs = new RangeSet();

            public MyListener(Object wl, int count)
            {
                _wl = wl;
                _count = count;
            }

            public void MessageTransfer(IMessage     m)
            {
                byte[] body = new byte[m.Body.Length - m.Body.Position];                               
                _log.Debug("Got a message of size: " + body.Length + " count = " + _count);
                _rs.Add(m.Id);
                lock (_wl)
                {
                    _count--;
                    if (_count == 0)
                    {
                        Monitor.PulseAll(_wl);
                    }
                }
            }

            public int Count
            {
                get { return _count; }
            }

            public RangeSet UnAck
            {
                get { return _rs; }
            }
        }
    }
}
