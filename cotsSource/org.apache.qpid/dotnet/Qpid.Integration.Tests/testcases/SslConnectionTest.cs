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
using System.IO;
using System.Reflection;
using System.Security.Cryptography.X509Certificates;
using NUnit.Framework;
using Apache.Qpid.Client.Qms;
using Apache.Qpid.Client;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Integration.Tests.testcases
{
    /// <summary>
    /// Test SSL/TLS connections to the broker
    /// </summary>
    [TestFixture, Category("Integration")]
    public class SslConnectionTest
    {
        /// <summary>
        /// Make a test TLS connection to the broker
        /// without using client-certificates
        /// </summary>
        //[Test]
        public void DoSslConnection()
        {
            // because for tests we don't usually trust the server certificate
            // we need here to tell the client to ignore certificate validation errors
            SslOptions sslConfig = new SslOptions(null, true);
            
            MakeBrokerConnection(sslConfig);
        }
        
        private static void MakeBrokerConnection(SslOptions options)
        {
            IConnectionInfo connectionInfo = new QpidConnectionInfo();
            connectionInfo.VirtualHost = "test";
            connectionInfo.AddBrokerInfo(new AmqBrokerInfo("amqp", "localhost", 8672, options));
            
            using ( IConnection connection = new AMQConnection(connectionInfo) )
            {
                Console.WriteLine("connection = " + connection);
            }
        }
    }
}
