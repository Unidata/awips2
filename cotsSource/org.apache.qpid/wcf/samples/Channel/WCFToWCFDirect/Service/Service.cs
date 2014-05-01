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


namespace Apache.Qpid.Samples.Channel.WCFToWCFDirect
{
    using System;
    using System.ServiceModel;
    using System.ServiceModel.Description;
    using Apache.Qpid.Channel;

    // Define a service contract. 
    [ServiceContract]
    public interface IHelloService
    {
        [OperationContract(IsOneWay = true, Action="*")]
        void SayHello(string name);
    }

    // Service class which implements the service contract.
    [ServiceBehavior(AddressFilterMode = AddressFilterMode.Any)]
    public class HelloService : IHelloService
    {
        [OperationBehavior]
        public void SayHello(string name)
        {
            Console.WriteLine("Hello " + name);
        }
    }

    class Service
    {
        static void Main(string[] args)
        {
            // Create binding for the service endpoint.
            AmqpBinding amqpBinding = new AmqpBinding();

            // Create ServiceHost.
            ServiceHost serviceHost = new ServiceHost(typeof(HelloService), new Uri[] { new Uri("http://localhost:8080/HelloService") });

            // Add behavior for our MEX endpoint.
            ServiceMetadataBehavior mexBehavior = new ServiceMetadataBehavior();
            mexBehavior.HttpGetEnabled = true;
            serviceHost.Description.Behaviors.Add(mexBehavior);

            // Add MEX endpoint.
            serviceHost.AddServiceEndpoint(typeof(IMetadataExchange), new BasicHttpBinding(), "MEX");

            // Add AMQP endpoint.
            Uri amqpUri = new Uri("amqp:message_queue");
            serviceHost.AddServiceEndpoint(typeof(IHelloService), amqpBinding, amqpUri.ToString());

            serviceHost.Open();

            Console.WriteLine();
            Console.WriteLine("The service is ready.");
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
