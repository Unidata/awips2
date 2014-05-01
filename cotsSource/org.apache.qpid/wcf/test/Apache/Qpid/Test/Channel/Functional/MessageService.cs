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
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.Threading;

    public class MessageService : IQueuedDatagramService1, IQueuedDatagramService2, IQueuedDatagramService3, IInteropService
    {
        private static Dictionary<string, int> methodCallCount = new Dictionary<string, int>();
        private static ServiceHost serviceHost;

        public static EventWaitHandle CompletionHandle
        {
            get;
            set;
        }

        public static int IntendedInvocationCount
        {
            get;
            set;
        }

        public static int TotalMethodCallCount
        {
            get;
            set;
        }

        // The test must set these paramters                  
        public static List<Type> ContractTypes
        {
            get;
            set;
        }

        public static string EndpointAddress
        {
            get;
            set;
        }

        public static void DisplayCounts()
        {
            Console.WriteLine("Method calls:");
            foreach (string key in methodCallCount.Keys)
            {
                Console.WriteLine("  {0}: {1}", key, methodCallCount[key]);
            }

            Console.WriteLine("Total: {0}", TotalMethodCallCount);
        }

        public static void StartService(Binding amqpBinding)
        {
            MessageService.methodCallCount.Clear();
            MessageService.TotalMethodCallCount = 0;

            serviceHost = new ServiceHost(typeof(MessageService));

            foreach (Type contractType in ContractTypes)
            {
                serviceHost.AddServiceEndpoint(contractType, amqpBinding, EndpointAddress);
            }

            serviceHost.Open();
        }

        public static void StopService()
        {
            if (serviceHost.State != CommunicationState.Faulted)
            {
                serviceHost.Close();
            }
        }

        public void UpdateCounts(string method)
        {
            lock (methodCallCount)
            {
                if (!methodCallCount.ContainsKey(method))
                {
                    methodCallCount[method] = 0;
                }

                ++methodCallCount[method];
                ++TotalMethodCallCount;
                if (TotalMethodCallCount >= IntendedInvocationCount && CompletionHandle != null)
                {
                    CompletionHandle.Set();
                }
            }
        }

        [OperationBehavior(TransactionScopeRequired = true, TransactionAutoComplete = true)]
        void IQueuedDatagramService1.Hello(string message)
        {
            this.UpdateCounts("IQueuedDatagramService1.Hello");
        }

        [OperationBehavior(TransactionScopeRequired = true, TransactionAutoComplete = true)]
        void IQueuedDatagramService1.Goodbye()
        {
            this.UpdateCounts("IQueuedDatagramService1.Goodbye");
        }

        [OperationBehavior(TransactionScopeRequired = true, TransactionAutoComplete = true)]
        void IQueuedDatagramService2.Hello(string message)
        {
            this.UpdateCounts("IQueuedDatagramService2.Hello");
        }

        [OperationBehavior(TransactionScopeRequired = true, TransactionAutoComplete = true)]
        void IQueuedDatagramService2.Goodbye()
        {
            this.UpdateCounts("IQueuedDatagramService2.Goodbye");
        }

        [OperationBehavior(TransactionScopeRequired = true, TransactionAutoComplete = true)]
        void IQueuedDatagramService3.Hello(string message)
        {
            this.UpdateCounts("IQueuedDatagramService3.Hello");
        }

        [OperationBehavior(TransactionScopeRequired = true, TransactionAutoComplete = true)]
        void IQueuedDatagramService3.Goodbye()
        {
            this.UpdateCounts("IQueuedDatagramService3.Goodbye");
        }

        [OperationBehavior(TransactionScopeRequired = true, TransactionAutoComplete = true)]
        void IInteropService.Hello(Message message)
        {
            this.UpdateCounts("IInteropService.Hello");
        }
    }
}
