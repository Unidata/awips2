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
using System.Net;
using NUnit.Framework;
using Apache.Qpid.Client.Qms;

namespace Apache.Qpid.Client.Tests.BrokerDetails
{
    [TestFixture]
    public class BrokerDetailsTest
    {

        [Test]
        public void ValidateBrokerInfoEqualsMethod()
        {
            AmqBrokerInfo broker = new AmqBrokerInfo("amqp", "localhost", 5672, true);
            AmqBrokerInfo broker1 = new AmqBrokerInfo("Amqp", "localhost", 5672, true);

            Assert.IsTrue(broker.Equals(broker1),"The two AmqBrokerInfo objects are not equals");
            Console.WriteLine(string.Format("The object broker: {0} and broker1: {1} are equals", broker, broker1));
        }

        [Test]
        public void ValidateBrokerInfoWithDifferentSSL()
        {
            AmqBrokerInfo broker = new AmqBrokerInfo("amqp", "localhost", 5672, true);
            AmqBrokerInfo broker1 = new AmqBrokerInfo("amqp", "localhost", 5672, false);

            Assert.IsFalse(broker.Equals(broker1), "The two AmqBrokerInfo objects are equals");
            Console.WriteLine(string.Format("The object broker: {0} and broker1: {1} are not equals", broker, broker1));
        }

        [Test]
        public void ValidateBrokerInfoFromToString()
        { 
            String url = "tcp://localhost:5672?timeout='200',immediatedelivery='true'";

            AmqBrokerInfo broker = new AmqBrokerInfo(url);
            AmqBrokerInfo broker1 = new AmqBrokerInfo(broker.ToString());

            Assert.AreEqual(broker.GetOption("timeout"), broker1.GetOption("timeout"));
            Assert.AreEqual(broker.GetOption("immediatedelivery"), broker1.GetOption("immediatedelivery"));
        }

    }
}
