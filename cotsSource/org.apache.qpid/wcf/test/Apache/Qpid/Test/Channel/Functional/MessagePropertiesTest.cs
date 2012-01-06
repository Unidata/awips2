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
    using System.Runtime.Serialization;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using Apache.Qpid.AmqpTypes;
    using NUnit.Framework;

    [TestFixture]
    public class MessagePropertiesTest
    {
        private const string RoutingKey = "routing_key";
        private const string SendToUri = "amqp:amq.direct?routingkey=" + RoutingKey;

        [Test]
        public void DefaultAmqpProperties()
        {
            const string TestString = "Test Message";
            AmqpProperties messageProperties = new AmqpProperties();

            this.SendMessage(TestString, messageProperties);
            this.ReceiveMessage<string>(TestString, messageProperties);
        }

        [Test]
        public void NonDefaultAmqpProperties()
        {
            const string TestString = "Test Message";
            AmqpProperties messageProperties = this.CreateMessageProperties();

            this.SendMessage(TestString, messageProperties);
            this.ReceiveMessage<string>(TestString, messageProperties);
        }

        private AmqpProperties CreateMessageProperties()
        {
            Dictionary<string, string> messageProperties = Util.GetProperties("..\\..\\MessageProperties.txt");

            AmqpProperties amqpProperties = new AmqpProperties();
            amqpProperties.ContentType = (string)messageProperties["ContentType"];
            amqpProperties.Durable = Convert.ToBoolean((string)messageProperties["Durable"]);
            amqpProperties.RoutingKey = (string)messageProperties["RoutingKey"];
            amqpProperties.TimeToLive = TimeSpan.Parse((string)messageProperties["TimeToLive"]);

            return amqpProperties;
        }

        private void SendMessage(object objectToSend, AmqpProperties propertiesToSend)
        {
            ChannelFactory<IOutputChannel> channelFactory =
                    new ChannelFactory<IOutputChannel>(Util.GetBinding(), SendToUri);
            IOutputChannel proxy = channelFactory.CreateChannel();
            proxy.Open();
            
            Message toSend = Message.CreateMessage(MessageVersion.Default, string.Empty, objectToSend);
            toSend.Properties["AmqpProperties"] = propertiesToSend;
            proxy.Send(toSend);

            toSend.Close();
            proxy.Close();
            channelFactory.Close();
        }

        private void ReceiveMessage<TObjectType>(TObjectType objectToMatch, AmqpProperties expectedProperties)
        {
            Uri receiveFromUri = new Uri("amqp:message_queue");
            IChannelListener<IInputChannel> listener = Util.GetBinding().BuildChannelListener<IInputChannel>(receiveFromUri, new BindingParameterCollection());
            listener.Open();
            IInputChannel service = listener.AcceptChannel(TimeSpan.FromSeconds(10));
            service.Open();
            Message receivedMessage = service.Receive(TimeSpan.FromSeconds(10));            
            try
            {
                TObjectType receivedObject = receivedMessage.GetBody<TObjectType>();
                Assert.True(receivedObject.Equals(objectToMatch), "Original and deserialized objects do not match");
                
                AmqpProperties receivedProperties = (AmqpProperties)receivedMessage.Properties["AmqpProperties"];
                PropertyInfo[] propInfo = typeof(AmqpProperties).GetProperties();

                for (int i = 0; i < propInfo.Length; i++)
                {
                    string propertyName = propInfo[i].Name;
                    if (propertyName.Equals("RoutingKey", StringComparison.InvariantCultureIgnoreCase))
                    {
                        Assert.AreEqual(RoutingKey, Convert.ToString(propInfo[i].GetValue(receivedProperties, null)));
                    }
                    else
                    {
                        Assert.AreEqual(Convert.ToString(propInfo[i].GetValue(expectedProperties, null)), Convert.ToString(propInfo[i].GetValue(receivedProperties, null)));
                    }
                }
            }
            catch (NullReferenceException)
            {
                Assert.Fail("Message not received");
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
        }
    }
}
