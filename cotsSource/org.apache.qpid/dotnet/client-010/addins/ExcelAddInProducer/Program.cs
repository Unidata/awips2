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
using System.Text;
using System.Threading;
using org.apache.qpid.client;

namespace ExcelAddInProducer
{
    class Program
    {
        static void Main(string[] args)
        {
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
            IMessage message = new Message();
            message.ApplicationHeaders.Add("price", 0);
            for (int i = 0; i < 100; i++)
            {
                message.ClearData();
                message.AppendData( Encoding.UTF8.GetBytes("test: " + i));
                message.ApplicationHeaders["price"] =  i;
                ssn.MessageTransfer("amq.direct", "queue1", message);               
                Thread.Sleep(1000);
            }

            client.Close();
        }
    }
}
