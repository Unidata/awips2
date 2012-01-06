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

using NUnit.Framework;
using org.apache.qpid.client;
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;

namespace test.interop
{
    public class Admin : TestCase
    {
        private static readonly Logger _log = Logger.Get(typeof(Admin));

        [Test]
        public void createSession()
        {
            _log.Debug("Running: CreateSession");
            IClientSession ssn = Client.CreateSession(0);
            ssn.Close();            
            // This test fails if an exception is thrown 
        }

        [Test]
        public void queueLifecycle()
        {
            _log.Debug("Running: queueLifecycle");
            IClientSession ssn = Client.CreateSession(0);
            ssn.QueueDeclare("queue1", null, null);
            ssn.Sync();
            ssn.QueueDelete("queue1");
            ssn.Sync();
            try
            {
                ssn.ExchangeBind("queue1", "amq.direct", "queue1", null);
                ssn.Sync();
            }
            catch (SessionException)
            {
              // as expected
            }           
            // This test fails if an exception is thrown 
        }

        [Test]
        public void exchangeCheck()
        {
            _log.Debug("Running: exchangeCheck");           
            IClientSession ssn = Client.CreateSession(0);            
            ExchangeQueryResult query = (ExchangeQueryResult) ssn.ExchangeQuery("amq.direct").Result;
            Assert.IsFalse(query.GetNotFound());
            Assert.IsTrue(query.GetDurable());
            query = (ExchangeQueryResult)ssn.ExchangeQuery("amq.topic").Result;
            Assert.IsFalse(query.GetNotFound());
            Assert.IsTrue(query.GetDurable());           
            query = (ExchangeQueryResult) ssn.ExchangeQuery("foo").Result;           
            Assert.IsTrue(query.GetNotFound());
        }

        [Test]
        public void exchangeBind()
        {
            _log.Debug("Running: ExchangeBind");       
            IClientSession ssn = Client.CreateSession(0);
            ssn.QueueDeclare("queue1", null, null);
            ssn.ExchangeBind("queue1", "amq.direct", "queue1", null);
            // This test fails if an exception is thrown 
        }


    }
}
