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

namespace Apache.Qpid.Client.Tests.url
{
    [TestFixture]
    public class connectionUrlTests
    {
        [Test]
        public void FailoverURL()
        {
            //String url = "amqp://ritchiem:bob@/temp?brokerlist='tcp://localhost:5672;tcp://fancyserver:3000/',failover='roundrobin'";
            String url = "amqp://ritchiem:bob@default/temp?brokerlist='tcp://localhost:5672;tcp://fancyserver:3000/',failover='roundrobin'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);

            Assert.AreEqual("roundrobin", connectionurl.FailoverMethod);
            Assert.IsTrue(connectionurl.Username.Equals("ritchiem"));
            Assert.IsTrue(connectionurl.Password.Equals("bob"));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/temp"));

            Assert.IsTrue(connectionurl.BrokerCount == 2);

            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("tcp"));
            Assert.IsTrue(service.Host.Equals("localhost"));
            Assert.IsTrue(service.Port == 5672);

            service = connectionurl.GetBrokerInfo(1);

            Assert.IsTrue(service.Transport.Equals("tcp"));
            Assert.IsTrue(service.Host.Equals("fancyserver"));
            Assert.IsTrue(service.Port == 3000);

        }

        [Test]
        public void SingleTransportUsernamePasswordURL()
        {
            String url = "amqp://ritchiem:bob@default/temp?brokerlist='tcp://localhost:5672'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);

            Assert.IsTrue(connectionurl.FailoverMethod == null);
            Assert.IsTrue(connectionurl.Username.Equals("ritchiem"));
            Assert.IsTrue(connectionurl.Password.Equals("bob"));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/temp"));

            Assert.IsTrue(connectionurl.BrokerCount == 1);

            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("tcp"));
            Assert.IsTrue(service.Host.Equals("localhost"));
            Assert.IsTrue(service.Port == 5672);
        }

        [Test]
        public void SingleTransportUsernameBlankPasswordURL()
        {
            String url = "amqp://ritchiem:@default/temp?brokerlist='tcp://localhost:5672'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);

            Assert.IsTrue(connectionurl.FailoverMethod == null);
            Assert.IsTrue(connectionurl.Username.Equals("ritchiem"));
            Assert.IsTrue(connectionurl.Password.Equals(""));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/temp"));

            Assert.IsTrue(connectionurl.BrokerCount == 1);

            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("tcp"));
            Assert.IsTrue(service.Host.Equals("localhost"));
            Assert.IsTrue(service.Port == 5672);
        }

        [Test]
        public void FailedURLNullPassword()
        {
            String url = "amqp://ritchiem@default/temp?brokerlist='tcp://localhost:5672'";

            try
            {
                QpidConnectionInfo.FromUrl(url);
                Assert.Fail("URL has null password");
            }
            catch (UrlSyntaxException e)
            {
                Assert.AreEqual("Null password in user information not allowed.", e.Message);
                Assert.IsTrue(e.GetIndex() == 7);
            }
        }

        [Test]
        public void SingleTransportURL()
        {
            String url = "amqp://guest:guest@default/test?brokerlist='tcp://localhost:5672'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);


            Assert.IsTrue(connectionurl.FailoverMethod == null);
            Assert.IsTrue(connectionurl.Username.Equals("guest"));
            Assert.IsTrue(connectionurl.Password.Equals("guest"));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/test"));


            Assert.IsTrue(connectionurl.BrokerCount == 1);


            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("tcp"));
            Assert.IsTrue(service.Host.Equals("localhost"));
            Assert.IsTrue(service.Port == 5672);
        }

        [Test]
        public void SingleTransportWithClientURLURL()
        {
            String url = "amqp://guest:guest@clientname/temp?brokerlist='tcp://localhost:5672'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);


            Assert.IsTrue(connectionurl.FailoverMethod == null);
            Assert.IsTrue(connectionurl.Username.Equals("guest"));
            Assert.IsTrue(connectionurl.Password.Equals("guest"));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/temp"));
            Assert.IsTrue(connectionurl.ClientName.Equals("clientname"));


            Assert.IsTrue(connectionurl.BrokerCount == 1);


            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("tcp"));
            Assert.IsTrue(service.Host.Equals("localhost"));
            Assert.IsTrue(service.Port == 5672);
        }

        [Test]
        public void SingleTransport1OptionURL()
        {
            String url = "amqp://guest:guest@default/temp?brokerlist='tcp://localhost:5672',routingkey='jim'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);

            Assert.IsTrue(connectionurl.FailoverMethod == null);
            Assert.IsTrue(connectionurl.Username.Equals("guest"));
            Assert.IsTrue(connectionurl.Password.Equals("guest"));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/temp"));


            Assert.IsTrue(connectionurl.BrokerCount == 1);

            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("tcp"));

            Assert.IsTrue(service.Host.Equals("localhost"));
            Assert.IsTrue(service.Port == 5672);
            Assert.IsTrue(connectionurl.GetOption("routingkey").Equals("jim"));
        }

        [Test]
        public void SingleTransportDefaultedBroker()
        {
            String url = "amqp://guest:guest@default/temp?brokerlist='localhost:'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);

            Assert.IsTrue(connectionurl.FailoverMethod == null);
            Assert.IsTrue(connectionurl.Username.Equals("guest"));
            Assert.IsTrue(connectionurl.Password.Equals("guest"));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/temp"));


            Assert.IsTrue(connectionurl.BrokerCount == 1);

            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("tcp"));

            Assert.IsTrue(service.Host.Equals("localhost"));
            Assert.IsTrue(service.Port == 5672);
        }

        [Test]
        public void SingleTransportMultiOptionURL()
        {
            String url = "amqp://guest:guest@default/temp?brokerlist='tcp://localhost:5672',routingkey='jim',timeout='200',immediatedelivery='true'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);

            Assert.IsTrue(connectionurl.FailoverMethod == null);
            Assert.IsTrue(connectionurl.Username.Equals("guest"));
            Assert.IsTrue(connectionurl.Password.Equals("guest"));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/temp"));

            Assert.IsTrue(connectionurl.BrokerCount == 1);

            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("tcp"));

            Assert.IsTrue(service.Host.Equals("localhost"));
            Assert.IsTrue(service.Port == 5672);

            Assert.IsTrue(connectionurl.GetOption("routingkey").Equals("jim"));
            Assert.IsTrue(connectionurl.GetOption("timeout").Equals("200"));
            Assert.IsTrue(connectionurl.GetOption("immediatedelivery").Equals("true"));
        }

        [Test]
        public void SinglevmURL()
        {
            String url = "amqp://guest:guest@default/messages?brokerlist='vm://default:2'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);

            Assert.IsTrue(connectionurl.FailoverMethod == null);
            Assert.IsTrue(connectionurl.Username.Equals("guest"));
            Assert.IsTrue(connectionurl.Password.Equals("guest"));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/messages"));

            Assert.IsTrue(connectionurl.BrokerCount == 1);

            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("vm"));
            Assert.AreEqual("localhost", service.Host);
            Assert.AreEqual(2, service.Port);
        }

        [Test]
        public void FailoverVMURL()
        {
            String url = "amqp://ritchiem:bob@default/temp?brokerlist='vm://default:2;vm://default:3',failover='roundrobin'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);

            Assert.IsTrue(connectionurl.FailoverMethod.Equals("roundrobin"));
            Assert.IsTrue(connectionurl.Username.Equals("ritchiem"));
            Assert.IsTrue(connectionurl.Password.Equals("bob"));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/temp"));

            Assert.AreEqual(2, connectionurl.BrokerCount);

            IBrokerInfo service = connectionurl.GetBrokerInfo(0);

            Assert.IsTrue(service.Transport.Equals("vm"));
            Assert.AreEqual("localhost", service.Host);
            Assert.IsTrue(service.Port == 2);

            service = connectionurl.GetBrokerInfo(1);
            Assert.IsTrue(service.Transport.Equals("vm"));
            Assert.AreEqual("localhost", service.Host);
            Assert.IsTrue(service.Port == 3);
        }

        [Test]
        public void NoVirtualHostURL()
        {
            String url = "amqp://user@default?brokerlist='tcp://localhost:5672'";

            try
            {
                QpidConnectionInfo.FromUrl(url);
                Assert.Fail("URL has no virtual host should not parse");
            }
            catch (UrlSyntaxException)
            {
                // This should occur.
            }
        }

        [Test]
        public void NoClientID()
        {
            String url = "amqp://user:@default/test?brokerlist='tcp://localhost:5672'";

            IConnectionInfo connectionurl = QpidConnectionInfo.FromUrl(url);

            Assert.IsTrue(connectionurl.Username.Equals("user"));
            Assert.IsTrue(connectionurl.Password.Equals(""));
            Assert.IsTrue(connectionurl.VirtualHost.Equals("/test"));
            Assert.IsTrue(connectionurl.ClientName.StartsWith(Dns.GetHostName()));

            Assert.IsTrue(connectionurl.BrokerCount == 1);
        }

        [Test]
        public void WrongOptionSeparatorInOptions()
        {
            String url = "amqp://guest:guest@default/test?brokerlist='tcp://localhost:5672;tcp://localhost:5673'+failover='roundrobin'";
            try
            {
                QpidConnectionInfo.FromUrl(url);
                Assert.Fail("URL Should not parse");
            }
            catch (UrlSyntaxException urise)
            {
                Assert.IsTrue(urise.Message.Equals("Unterminated option. Possible illegal option separator:'+'"));
            }

        }

        [Test]
        public void NoUserDetailsProvidedWithClientID()

        {
            String url = "amqp://clientID/test?brokerlist='tcp://localhost:5672;tcp://localhost:5673'";
            try
            {
                QpidConnectionInfo.FromUrl(url);
                Assert.Fail("URL Should not parse");
            }
            catch (UrlSyntaxException urise)
            {
                Assert.IsTrue(urise.Message.StartsWith("User information not found on url"));
            }

        }

        [Test]
        public void NoUserDetailsProvidedNOClientID()

        {
            String url = "amqp:///test@default?brokerlist='tcp://localhost:5672;tcp://localhost:5673'";
            try
            {
                QpidConnectionInfo.FromUrl(url);
                Assert.Fail("URL Should not parse");
            }
            catch (UrlSyntaxException urise)
            {

                Assert.IsTrue(urise.Message.StartsWith("User information not found on url"));
            }

        }

        [Test]
        public void CheckVirtualHostFormat()
        {
            String url = "amqp://guest:guest@default/t.-_+!=:?brokerlist='tcp://localhost:5672'";

            IConnectionInfo connection = QpidConnectionInfo.FromUrl(url);
            Assert.IsTrue(connection.VirtualHost.Equals("/t.-_+!=:"));
        }

        [Test]
        public void CheckDefaultPort()
        {
            String url = "amqp://guest:guest@default/test=:?brokerlist='tcp://localhost'";

            IConnectionInfo connection = QpidConnectionInfo.FromUrl(url);

            IBrokerInfo broker = connection.GetBrokerInfo(0);
            Assert.IsTrue(broker.Port == BrokerInfoConstants.DEFAULT_PORT);

        }

        [Test]
        public void CheckMissingFinalQuote()
        {
            String url = "amqp://guest:guest@id/test" + "?brokerlist='tcp://localhost:5672";

            try
            {
                QpidConnectionInfo.FromUrl(url);
            }
            catch (UrlSyntaxException e)
            {
//                Assert.AreEqual("Unterminated option at index 32: brokerlist='tcp://localhost:5672",
//                    e.Message);
                Assert.AreEqual("Unterminated option", e.Message);
            }
        }

        [Test]
        public void ValidateQpidConnectionInfoFromToString()
        {
            String url = "amqp://ritchiem:bob@default/temp?brokerlist='tcp://localhost:5672;tcp://fancyserver:3000/',failover='roundrobin'";

            IConnectionInfo connectionInfo = QpidConnectionInfo.FromUrl(url);
            IConnectionInfo connectionInfo1 = QpidConnectionInfo.FromUrl(connectionInfo.ToString());

            Console.WriteLine(connectionInfo.ToString());
            Console.WriteLine(connectionInfo1.ToString());

            Assert.AreEqual(connectionInfo.Username, connectionInfo1.Username);
            Assert.AreEqual(connectionInfo.Password, connectionInfo1.Password);
            Assert.AreEqual(connectionInfo.VirtualHost, connectionInfo1.VirtualHost);

            Assert.IsTrue((connectionInfo1.GetAllBrokerInfos().Count == 2));
            Assert.IsTrue(connectionInfo.GetBrokerInfo(0).Equals(connectionInfo1.GetBrokerInfo(0)));
            Assert.IsTrue(connectionInfo.GetBrokerInfo(1).Equals(connectionInfo1.GetBrokerInfo(1)));

        }

        [Test]
        public void EnsureVirtualHostStartsWithSlash()
        {
           IConnectionInfo connection = new QpidConnectionInfo();
           connection.VirtualHost = "test";
           Assert.AreEqual("/test", connection.VirtualHost);

           connection.VirtualHost = "/mytest";
           Assert.AreEqual("/mytest", connection.VirtualHost);

           connection.VirtualHost = "";
           Assert.AreEqual("/", connection.VirtualHost);

           connection.VirtualHost = null;
           Assert.AreEqual("/", connection.VirtualHost);
        }
    }
}
