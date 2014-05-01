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
    using System.Reflection;
    using System.ServiceModel;
    using System.ServiceModel.Channels;

    public class MessageClient
    {
        public int NumberOfMessages
        {
            get;
            set;
        }

        public int NumberOfIterations
        {
            get;
            set;
        }

        public void RunClient<TServiceContract>(EndpointAddress address)
        {
            Binding amqpBinding = Util.GetBinding();
            Type proxyType = typeof(TServiceContract);
            MethodInfo helloMethod = proxyType.GetMethod("Hello");
            MethodInfo goodbyeMethod = proxyType.GetMethod("Goodbye");

            string[] messages = new string[this.NumberOfMessages];
            for (int i = 0; i < this.NumberOfMessages; ++i)
            {
                messages[i] = "Message " + i;
            }

            for (int i = 0; i < this.NumberOfIterations; ++i)
            {
                this.CreateChannelAndSendMessages<TServiceContract>(address, amqpBinding, helloMethod, goodbyeMethod, messages);
            }
        }

        public void RunInteropClient<TServiceContract>(EndpointAddress address)
        {
            Binding amqpBinding = Util.GetBinding();
            Type proxyType = typeof(TServiceContract);
            MethodInfo helloMethod = proxyType.GetMethod("Hello");

            Message[] messages = new Message[this.NumberOfMessages];

            for (int i = 0; i < this.NumberOfIterations; ++i)
            {
                this.CreateInteropChannelAndSendMessages<TServiceContract>(address, amqpBinding, helloMethod, this.NumberOfMessages);
            }
        }

        private void CreateChannelAndSendMessages<TServiceContract>(EndpointAddress address, Binding amqpBinding, MethodInfo helloMethod, MethodInfo goodbyeMethod, object[] messages)
        {
            ChannelFactory<TServiceContract> channelFactory = new ChannelFactory<TServiceContract>(amqpBinding, address);
            TServiceContract proxy = channelFactory.CreateChannel();

            foreach (object message in messages)
            {
                helloMethod.Invoke(proxy, new object[] { message });
            }

            goodbyeMethod.Invoke(proxy, new object[0]);
            channelFactory.Close();
        }

        private void CreateInteropChannelAndSendMessages<TServiceContract>(EndpointAddress address, Binding amqpBinding, MethodInfo helloMethod, int messageCount)
        {
            ChannelFactory<TServiceContract> channelFactory = new ChannelFactory<TServiceContract>(amqpBinding, address);
            TServiceContract proxy = channelFactory.CreateChannel();

            for (int i = 0; i < messageCount; i++)
            {
                helloMethod.Invoke(proxy, new object[] { Message.CreateMessage(MessageVersion.Soap12WSAddressing10, "*") });
            }

            channelFactory.Close();
        }
    }
}
