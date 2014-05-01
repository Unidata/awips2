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

using System;
using System.Configuration;
using System.IO;
using System.Text;
using System.Threading;
using org.apache.qpid.client;
using org.apache.qpid.transport;

namespace org.apache.qpid.example.requestresponse
{
    /// <summary>
    ///  This program is one of two programs that illustrate the
    ///  request/response pattern.
    ///
    ///  Client (this program):
    ///    Make requests of a service, print the response.
    ///
    ///  Server:
    ///    Accept requests, set the letters to uppercase in each message, and
    ///    return it as a response.
    ///
    /// </summary>
    internal class RequestResponseClient
    {
        private static void Main(string[] args)
        {
            string host = ConfigurationManager.AppSettings["Host"];
            int port = int.Parse(ConfigurationManager.AppSettings["Port"]);
            string virtualhost = ConfigurationManager.AppSettings["VirtualHost"];
            string username = ConfigurationManager.AppSettings["Username"];
            string password = ConfigurationManager.AppSettings["Password"];

            Client connection = new Client();
            try
            {
                connection.Connect(host, port, virtualhost, username, password);
                IClientSession session = connection.CreateSession(50000);
                IMessage request = new Message();

                //--------- Main body of program --------------------------------------------
                // Create a response queue so the server can send us responses
                // to our requests. Use the client's session ID as the name
                // of the response queue.
                string response_queue = "client" + session.GetName();
                // Use the name of the response queue as the routing key
                session.QueueDeclare(response_queue);
                session.ExchangeBind(response_queue, "amq.direct", response_queue);

                // Each client sends the name of their own response queue so
                // the service knows where to route messages.
                request.DeliveryProperties.SetRoutingKey("request");
                request.MessageProperties.SetReplyTo(new ReplyTo("amq.direct", response_queue));

                lock (session)
                {
                    // Create a listener for the response queue and listen for response messages.
                    Console.WriteLine("Activating response queue listener for: " + response_queue);
                    IMessageListener listener = new ClientMessageListener(session);
                    session.AttachMessageListener(listener, response_queue);
                    session.MessageSubscribe(response_queue);

                    // Now send some requests ...
                    string[] strs = {
                                        "Twas brillig, and the slithy toves",
                                        "Did gire and gymble in the wabe.",
                                        "All mimsy were the borogroves,",
                                        "And the mome raths outgrabe.",
                                        "That's all, folks!"
                                    };
                    foreach (string s in strs)
                    {
                        request.ClearData();
                        request.AppendData(Encoding.UTF8.GetBytes(s));
                        session.MessageTransfer("amq.direct", request);
                    }
                    Console.WriteLine("Waiting for all responses to arrive ...");
                    Monitor.Wait(session);
                }
                //---------------------------------------------------------------------------

                connection.Close();
            }
            catch (Exception e)
            {
                Console.WriteLine("Error: \n" + e.StackTrace);
            }
        }
    }

    public class ClientMessageListener : IMessageListener
    {
        private readonly IClientSession _session;
        private readonly RangeSet _range = new RangeSet();
        private int _counter;
        public ClientMessageListener(IClientSession session)
        {
            _session = session;
        }

        public void MessageTransfer(IMessage m)
        {
            _counter++;
            BinaryReader reader = new BinaryReader(m.Body, Encoding.UTF8);
            byte[] body = new byte[m.Body.Length - m.Body.Position];
            reader.Read(body, 0, body.Length);
            ASCIIEncoding enc = new ASCIIEncoding();
            string message = enc.GetString(body);
            Console.WriteLine("Response: " + message);
            // Add this message to the list of message to be acknowledged 
            _range.Add(m.Id);
            if (_counter == 4)
            {
                Console.WriteLine("Shutting down listener for " + m.DeliveryProperties.GetRoutingKey());              
                // Acknowledge all the received messages 
                _session.MessageAccept(_range);
                lock (_session)
                {
                    Monitor.Pulse(_session);
                }
            }
        }
    }
}
