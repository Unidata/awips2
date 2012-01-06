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


namespace Apache.Qpid.Samples.Channel.WCFToWCFPubSub
{
    using System;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using Apache.Qpid.Channel;    

    class Topic_Producer
    {
        static void Main(string[] args)
        {
            try
            {
                // Create binding for the service endpoint.
                CustomBinding amqpBinding = new CustomBinding();
                amqpBinding.Elements.Add(new BinaryMessageEncodingBindingElement());
                amqpBinding.Elements.Add(new AmqpTransportBindingElement());

                // Create endpoint address.
                Uri amqpClientUri = new Uri("amqp:amq.topic?routingkey=usa.news");                
                EndpointAddress endpointAddress = new EndpointAddress(amqpClientUri);

                // Create a client with given client endpoint configuration.                                
                ChannelFactory<IHelloService> channelFactory = new ChannelFactory<IHelloService>(amqpBinding, endpointAddress);
                IHelloService clientProxy = channelFactory.CreateChannel();

                Console.WriteLine();

                string name = "name";
                for (int i = 0; i < 5; i++)
                {                    
                    Console.WriteLine("Sending message: " + name + (i + 1));
                    clientProxy.SayHello(name + (i+1));
                }

                Console.WriteLine();
                Console.WriteLine("Press <ENTER> to terminate client.");
                Console.ReadLine();

                channelFactory.Close();
            }
            catch (Exception e)
            {
                Console.WriteLine("Exception: {0}", e);
            }
        }
    }
}
