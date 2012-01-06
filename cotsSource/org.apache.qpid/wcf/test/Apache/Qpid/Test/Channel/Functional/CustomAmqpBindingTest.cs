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

namespace Apache.Qpid.Test.Channel.Functional
{
    using System;
    using System.Collections.Generic;
    using System.Reflection;
    using System.ServiceModel;
    using System.Threading;
    using NUnit.Framework;

    [TestFixture]
    public class CustomAmqpBindingTest
    {
        private MessageClient client;

        [SetUp]
        public void Setup()
        {
            // Create client
            this.client = new MessageClient();
            this.client.NumberOfMessages = 3;
            this.client.NumberOfIterations = 3;

            // Setup and start service            
            MessageService.EndpointAddress = "amqp:message_queue";
            MessageService.ContractTypes = new List<Type>();
            MessageService.ContractTypes.Add(typeof(IInteropService));
            MessageService.CompletionHandle = new EventWaitHandle(false, EventResetMode.AutoReset);
            MessageService.IntendedInvocationCount = this.client.NumberOfIterations * this.client.NumberOfMessages * MessageService.ContractTypes.Count;
            MessageService.StartService(Util.GetCustomBinding());
        }

        [Test]
        public void Run()
        {
            // Create the WCF AMQP channel and send messages
            MethodInfo runClientMethod = this.client.GetType().GetMethod("RunInteropClient");
            EndpointAddress address = new EndpointAddress("amqp:amq.direct?routingkey=routing_key");
            foreach (Type contractType in MessageService.ContractTypes)
            {
                MethodInfo runClientT = runClientMethod.MakeGenericMethod(contractType);
                runClientT.Invoke(this.client, new object[] { address });
            }

            // Allow the WCF service to process all the messages before validation
            MessageService.CompletionHandle.WaitOne(TimeSpan.FromSeconds(10.0), false);
            
            // Validation
            int expectedMethodCallCount = this.client.NumberOfIterations * this.client.NumberOfMessages * MessageService.ContractTypes.Count;
            Assert.AreEqual(expectedMethodCallCount, MessageService.TotalMethodCallCount);
        }

        [TearDown]
        public void Cleanup()
        {
            MessageService.StopService();
        }
    }
}
