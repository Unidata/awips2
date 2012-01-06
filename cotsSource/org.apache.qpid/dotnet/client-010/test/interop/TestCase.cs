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
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Xml;
using common.org.apache.qpid.transport.util;
using log4net.Config;
using NUnit.Framework;
using org.apache.qpid.client;
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;
using test.Helpers;

namespace test.interop
{
    [TestFixture]

    public class TestCase
    {       
        private readonly Dictionary<string,string> _properties = new Dictionary<string, string>();
        private  Client _client;

        [TestFixtureSetUp] 
        public void Init()
        {
            var properties = ConfigHelpers.LoadConfig();
            // create a client and connect to the broker
            _client = new Client();
            _client.Connect(properties["Host"], Convert.ToInt16(properties["Port"]), properties["VirtualHost"],
                           properties["Username"], properties["Password"]);           
   
        }

        [TestFixtureTearDown]
        public void Cleanup()
        {
            // Note : breaks the Resharper nunit test runner. It blocks on the Monitor.WaitAll() 
            // Certainly a problem with the threading context..
            //_client.Close();
        }

        public Client Client
        {
            get{ return _client;}
        }

        public Dictionary<string,string> Properties
        {
            get { return _properties; }
        }


        public class SyncListener : IMessageListener
        {
            private static readonly Logger _log = Logger.Get(typeof(SyncListener));
            private readonly CircularBuffer<IMessage> _buffer;
            private readonly RangeSet _range = new RangeSet();
            private readonly IClientSession _session;

            public SyncListener(IClientSession session, CircularBuffer<IMessage> buffer)
            {
                _buffer = buffer;
                _session = session;
            }

            public void MessageTransfer(IMessage m)
            {
                _range.Clear();
                _range.Add(m.Id);
                _session.MessageAccept(_range);
                _buffer.Enqueue(m);
            }
        }
    }
}
