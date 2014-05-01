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

namespace org.apache.qpid.wcf.demo.helloClient
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Title = "Hello Service Client";
            Console.ForegroundColor = ConsoleColor.White;
            Console.WriteLine("Hello Service Client");
            Console.ForegroundColor = ConsoleColor.Gray;
            Console.WriteLine();            
            // create a client using the configuration file App.config
            var client = new HelloClient("HelloService");
            Console.WriteLine("Client Saying Hello to Qpid");
            client.Hello("Qpid");
            Console.WriteLine("Client Saying Hello to AMQP");
            client.Hello("AMQP");
            // closing the client service 
            client.ChannelFactory.Close();         
            Console.WriteLine();
            Console.Write("Press Enter to Exit...");
            Console.ReadLine();
        }
    }
}
