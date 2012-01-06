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
    ///  Client:
    ///    Make requests of a service, print the response.
    ///
    ///  Server (this program):
    ///    Accept requests, set the letters to uppercase in each message, and
    ///    return it as a response.
    ///
    /// </summary>
    class Server
    {
        static void Main(string[] args)
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

                //--------- Main body of program --------------------------------------------
                // Create a request queue for clients to use when making
                // requests.
                const string request_queue = "request";
                // Use the name of the request queue as the routing key
                session.QueueDeclare(request_queue);
                session.ExchangeBind(request_queue, "amq.direct", request_queue);

                lock (session)
                {
                    // Create a listener and subscribe it to the request_queue      
                    IMessageListener listener = new MessageListener(session);
                    session.AttachMessageListener(listener, request_queue);
                    session.MessageSubscribe(request_queue);
                    // Receive messages until all messages are received
                    Console.WriteLine("Waiting for requests");
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

    public class MessageListener : IMessageListener
    {
        private readonly IClientSession _session;
        private readonly RangeSet _range = new RangeSet();
        public MessageListener(IClientSession session)
        {
            _session = session;
        }

        public void MessageTransfer(IMessage request)
        {
            IMessage response = new Message();

            // Get routing key for response from the request's replyTo property
            string routingKey;
            if( request.MessageProperties.HasReplyTo() )
            {
                routingKey = request.MessageProperties.GetReplyTo().GetRoutingKey();
            }
            else
            {
                Console.WriteLine("Error: \n No routing key for request " + request);
                return;
            }

            BinaryReader reader = new BinaryReader(request.Body, Encoding.UTF8);
            byte[] body = new byte[request.Body.Length - request.Body.Position];
            reader.Read(body, 0, body.Length);
            ASCIIEncoding enc = new ASCIIEncoding();
            string message = enc.GetString(body);
            Console.WriteLine("Request: " + message);
            
            // Transform message content to upper case
            string responseBody = message.ToUpper();

            // Send it back to the user
            response.ClearData();
            response.AppendData(Encoding.UTF8.GetBytes(responseBody));
            _session.MessageTransfer("amq.direct", routingKey, response);

            // Add this message to the list of message to be acknowledged 
            _range.Add(request.Id);
            if (message.Equals("That's all, folks!"))
            {
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
