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
using System.Net.Sockets;
using NUnit.Framework;
using org.apache.qpid.client;
using test.Helpers;

namespace test
{
    [TestFixture]
    public class ConnectionTests
    {
        [SetUp]
        public void Setup()
        {

        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void should_raise_exception_in_calling_thread_on_authentification_failure()
        {
            var properties = ConfigHelpers.LoadConfig();

            var client = new Client();
            client.Connect(properties["Host"], Convert.ToInt16(properties["Port"]), properties["VirtualHost"],
                           properties["Username"], "some silly password to make sure the authentification fail");      
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void should_raise_exception_in_calling_thread_on_authentification_failure_with_clodedListener()
        {
            var properties = ConfigHelpers.LoadConfig();

            var client = new Client();
            client.ClosedListener = new FakeListener();
            client.Connect(properties["Host"], Convert.ToInt16(properties["Port"]), properties["VirtualHost"],
                           properties["Username"], "some silly password to make sure the authentification fail");
        }

        [Test]
        public void should_not_block_on_close()
        {
            var properties = ConfigHelpers.LoadConfig();

            var client = new Client();
            client.Connect(properties["Host"], Convert.ToInt16(properties["Port"]), properties["VirtualHost"],
                           properties["Username"], properties["Password"]);
            client.Close();
        }
    }

    public class FakeListener : IClosedListener
    {
        public void OnClosed(ErrorCode errorCode, string reason, Exception t)
        {
        }
    }
}
