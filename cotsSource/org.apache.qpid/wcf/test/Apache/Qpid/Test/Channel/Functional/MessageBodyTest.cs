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
    using System.Runtime.Serialization;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using NUnit.Framework;

    [TestFixture]
    public class MessageBodyTest
    {
        private const string Queue = "amqp:amq.direct?routingkey=routing_key";

        [Test]
        public void DateVariation()
        {
            DateTime rightNow = DateTime.UtcNow;
            this.SendMessage(rightNow);
            this.ReceiveMessage<DateTime>(rightNow);
        }

        [Test]
        public void EmptyStringVariation()
        {
            const string TestString = "";
            this.SendMessage(TestString);
            this.ReceiveMessage<string>(TestString);
        }

        [Test]
        public void IntPrimitiveVariation()
        {
            const int TheAnswer = 42;
            this.SendMessage(TheAnswer);
            this.ReceiveMessage<int>(TheAnswer);
        }

        [Test]
        public void MultipleIntVariation()
        {
            const int NumberOfMessages = 20;
            int[] listOfNumbers = new int[NumberOfMessages];

            for (int i = 0; i < NumberOfMessages; i++)
            {
                this.SendMessage(i);
                listOfNumbers[i] = i;
            }

            Assert.True(listOfNumbers[NumberOfMessages - 1] != 0, "Not all messages were sent.");

            for (int j = 0; j < NumberOfMessages; j++)
            {
                int receivedNumber = this.ReceiveMessage<int>();
                Assert.True(listOfNumbers[j].Equals(receivedNumber), "Received {0} - this number is unknown or has been received more than once.", receivedNumber);
            }
        }

        [Test]
        public void StringVariation()
        {
            const string TestString = "The darkest of dim, dreary days dost draw deathly deeds. どーも";
            this.SendMessage(TestString);
            this.ReceiveMessage<string>(TestString);
        }
        
        private void SendMessage(object objectToSend)
        {
            IChannelFactory<IOutputChannel> channelFactory =
                    Util.GetBinding().BuildChannelFactory<IOutputChannel>();
            channelFactory.Open();
            IOutputChannel proxy = channelFactory.CreateChannel(new EndpointAddress(Queue));
            proxy.Open();
            Message toSend = Message.CreateMessage(MessageVersion.Default, string.Empty, objectToSend);
            proxy.Send(toSend);
            toSend.Close();
            channelFactory.Close();
        }

        private TObjectType ReceiveMessage<TObjectType>()
        {
            Uri endpoint = new Uri("amqp:message_queue");
            IChannelListener<IInputChannel> listener = Util.GetBinding().BuildChannelListener<IInputChannel>(endpoint, new BindingParameterCollection());
            listener.Open();
            IInputChannel service = listener.AcceptChannel(TimeSpan.FromSeconds(10));
            service.Open();
            Message receivedMessage = service.Receive(TimeSpan.FromSeconds(10));
            Assert.NotNull(receivedMessage, "Message was not received");
            try
            {
                TObjectType receivedObject = receivedMessage.GetBody<TObjectType>();
                return receivedObject;
            }
            catch (SerializationException)
            {
                Assert.Fail("Deserialized object not of correct type");
            }
            finally
            {
                receivedMessage.Close();
                service.Close();
                listener.Close();
            }

            return default(TObjectType);
        }

        private TObjectType ReceiveMessage<TObjectType>(TObjectType objectToMatch)
        {
            TObjectType receivedObject = this.ReceiveMessage<TObjectType>();
            Assert.True(objectToMatch.Equals(receivedObject), "Original and deserialized objects do not match");
            return receivedObject;
        }
    }
}
