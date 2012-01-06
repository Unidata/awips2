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
using common.org.apache.qpid.transport.util;
using NUnit.Framework;
using org.apache.qpid.client;
using org.apache.qpid.transport.util;

namespace test.interop
{
    public class ApplicationHeaders:TestCase
    {
        private static readonly Logger _log = Logger.Get(typeof(ApplicationHeaders));

        [Test]
        public void setHeaders()
        {          
            _log.Debug("Running: setHeaders");
            IClientSession ssn = Client.CreateSession(0);
            ssn.QueueDeclare("queue1");
            ssn.ExchangeBind("queue1", "amq.direct", "queue1");
            ssn.Sync();
            CircularBuffer<IMessage> buff = new CircularBuffer<IMessage>(10); 
            SyncListener listener = new SyncListener(ssn, buff);
            ssn.AttachMessageListener(listener, "queue1");
            ssn.MessageSubscribe("queue1");

            IMessage message = new org.apache.qpid.client.Message();
            message.DeliveryProperties.SetRoutingKey("queue1");
            const long someLong = 14444444;
            message.ApplicationHeaders.Add("someLong", someLong);
            const int someInt = 14;
            message.ApplicationHeaders.Add("soneInt", someInt);
            const float someFloat = 14.001F;
            message.ApplicationHeaders.Add("soneFloat", someFloat);
            const double someDouble = 14.5555555;
            message.ApplicationHeaders.Add("someDouble", someDouble);
            const byte someByte = 2;
            message.ApplicationHeaders.Add("someByte", someByte);
            const string someString = "someString";
            message.ApplicationHeaders.Add("someString", someString);
            const char someChar = 'a';
            message.ApplicationHeaders.Add("someChar", someChar);
            const Boolean someBoolean = true;
            message.ApplicationHeaders.Add("someBoolean", someBoolean);

            // transfer the message 
            ssn.MessageTransfer("amq.direct", message); 

            // get the message and check the headers 
            IMessage messageBack = buff.Dequeue();
            Assert.IsTrue(((string) messageBack.ApplicationHeaders["someString"]).Equals(someString));
            Assert.IsTrue(((char)messageBack.ApplicationHeaders["someChar"]).Equals(someChar));
            Assert.IsTrue((long)messageBack.ApplicationHeaders["someLong"] == someLong);
            Assert.IsTrue((int)messageBack.ApplicationHeaders["soneInt"] == someInt);           
            Assert.IsTrue((double)messageBack.ApplicationHeaders["someDouble"] == someDouble);
            Assert.IsTrue((byte) messageBack.ApplicationHeaders["someByte"] == someByte);
            Assert.IsTrue((Boolean)messageBack.ApplicationHeaders["someBoolean"]);
            // c# has an conversion precision issue with decimal 
            Assert.IsTrue((float) messageBack.ApplicationHeaders["soneFloat"] <= someFloat);
            float b = (float) messageBack.ApplicationHeaders["soneFloat"];
            Assert.IsTrue(Convert.ToInt32(b) == Convert.ToInt32(someFloat));
        }
    }
}
