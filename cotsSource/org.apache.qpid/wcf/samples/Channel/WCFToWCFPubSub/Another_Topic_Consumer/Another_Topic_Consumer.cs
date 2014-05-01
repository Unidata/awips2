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
    using System.ServiceModel.Description;
    using Apache.Qpid.Channel;    

    class Another_Topic_Consumer
    {
        static void Main(string[] args)
        {
            // Create binding for the service endpoint.
            CustomBinding amqpBinding = new CustomBinding();
            amqpBinding.Elements.Add(new BinaryMessageEncodingBindingElement());
            amqpBinding.Elements.Add(new AmqpTransportBindingElement());

            // Create ServiceHost.
            ServiceHost serviceHost = new ServiceHost(typeof(HelloService), new Uri[] { new Uri("http://localhost:8080/HelloService2") });

            // Add behavior for our MEX endpoint.
            ServiceMetadataBehavior mexBehavior = new ServiceMetadataBehavior();
            mexBehavior.HttpGetEnabled = true;
            serviceHost.Description.Behaviors.Add(mexBehavior);

            // Add MEX endpoint.
            serviceHost.AddServiceEndpoint(typeof(IMetadataExchange), new BasicHttpBinding(), "MEX");

            // Add AMQP endpoint.
            Uri amqpUri = new Uri("amqp:news");
            serviceHost.AddServiceEndpoint(typeof(IHelloService), amqpBinding, amqpUri.ToString());

            serviceHost.Open();

            Console.WriteLine();
            Console.WriteLine("The consumer is now listening on the queue \"news\".");
            Console.WriteLine("Press <ENTER> to terminate service.");
            Console.WriteLine();
            Console.ReadLine();

            if (serviceHost.State != CommunicationState.Faulted)
            {
                serviceHost.Close();
            }            
        }
    }
}
