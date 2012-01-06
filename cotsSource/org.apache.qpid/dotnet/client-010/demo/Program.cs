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
using System.Configuration;
using System.IO;
using System.Text;
using System.Threading;
using log4net.Config;
using org.apache.qpid.client;
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;

namespace WindowsClient
{
    class Program
    {
        static void Main(string[] args)
        {
             XmlConfigurator.Configure(new FileInfo("..\\..\\log.xml"));
            // DOMConfigurator.Configure()            

            string host = ConfigurationManager.AppSettings["Host"];
            int port = int.Parse(ConfigurationManager.AppSettings["Port"]);
            string virtualhost = ConfigurationManager.AppSettings["VirtualHost"];
            string username = ConfigurationManager.AppSettings["Username"];
            string password = ConfigurationManager.AppSettings["Password"];

            Client client = new Client();
            Console.WriteLine("Client created");
            client.Connect(host, port, virtualhost, username, password);
            Console.WriteLine("Connection established");

            IClientSession ssn = client.CreateSession(50000);
            Console.WriteLine("Session created");
            ssn.QueueDeclare("queue1", null, null);
            ssn.ExchangeBind("queue1", "amq.direct", "queue1", null);


            Object wl = new Object();
            ssn.AttachMessageListener(new MyListener(ssn, wl), "myDest");

            ssn.MessageSubscribe("queue1", "myDest", MessageAcceptMode.EXPLICIT, MessageAcquireMode.PRE_ACQUIRED, null,
                                 0, null);
            DateTime start = DateTime.Now;

            // issue credits     
            ssn.MessageSetFlowMode("myDest", MessageFlowMode.WINDOW);
            ssn.MessageFlow("myDest", MessageCreditUnit.BYTE, ClientSession.MESSAGE_FLOW_MAX_BYTES);
            ssn.MessageFlow("myDest", MessageCreditUnit.MESSAGE, 10000);
            ssn.Sync();


            for (int i = 0; i < 10000; i ++)
            {            
            ssn.MessageTransfer("amq.direct", MessageAcceptMode.NONE, MessageAcquireMode.PRE_ACQUIRED,
                                new Header(new DeliveryProperties().SetRoutingKey("queue1"),
                                           new MessageProperties().SetMessageId(UUID.RandomUuid())),
                                Encoding.UTF8.GetBytes("test: " + i));
            }

            lock(wl)
            {
                Monitor.Wait(wl);
            }
            DateTime now = DateTime.Now;
            Console.WriteLine("Start time " + start + " now: " + now);

            Console.WriteLine("Done time: " +  (now - start));
            lock (wl)
            {
                Monitor.Wait(wl, 30000);
            }
            client.Close();
        }
    }

    class MyListener : IMessageListener
    {
        private readonly Object _wl;
        private IClientSession _session;
        private int _count;

        public MyListener(IClientSession session, object wl)
        {
            _wl = wl;
            _session = session;
            _count = 0;
        }

        public void MessageTransfer(IMessage m)
        {
            BinaryReader reader = new BinaryReader(m.Body, Encoding.UTF8);
            byte[] body = new byte[m.Body.Length - m.Body.Position];
            reader.Read(body, 0, body.Length);
            ASCIIEncoding enc = new ASCIIEncoding();
        //   Console.WriteLine("Got a message: " + enc.GetString(body) + " count = " + _count);           
            _count++;
            if (_count == 10000)
            {
                lock (_wl)
                {
                    Monitor.PulseAll(_wl);
                }
            }
        }
    }
}
