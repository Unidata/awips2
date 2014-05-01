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

using System.Configuration;
using System.ServiceModel;
using System.ServiceModel.Channels;

namespace org.apache.qpid.wcf.model
{
    public sealed class QpidBinding : Binding
    {
        private string _host;
        private int _port;
        private string _username;
        private string _password;
        private string _virtuaHost;
        private readonly CompositeDuplexBindingElement _compositeDuplex;
        private readonly MessageEncodingBindingElement _encoding;
        private bool _oneWayOnly;
        private readonly ReliableSessionBindingElement _session;
        private readonly TransactionFlowBindingElement _transactionFlow;
        private bool _transactionsEnabled;
        private readonly QpidTransportBindingElement _transport;



        public QpidBinding() : this("localhost", 5672, "guest", "guest", "test")
        {
        }


        public QpidBinding(string host, int port ) : this (host, port, "guest", "guest", "test")
        {
        }

        public QpidBinding(string host, int port, string username, string password, string virtualhost)
        {
            Host = host;
            PortNumber = port;
            UserName = username;
            Password = password;
            VirtualHost = virtualhost;
            _transport = new QpidTransportBindingElement();
            _transport.Host = host;
            _transport.PortNumber = port;
            _transport.Password = password;
            _transport.UserName = username;
            _transport.VirtualHost = virtualhost;
            _encoding = new TextMessageEncodingBindingElement();
            _session = new ReliableSessionBindingElement();
            _compositeDuplex = new CompositeDuplexBindingElement();
            _transactionFlow = new TransactionFlowBindingElement();
        }       

        public override BindingElementCollection CreateBindingElements()
        {
            var elements = new BindingElementCollection();

            if (_transactionsEnabled)
            {
                elements.Add(_transactionFlow);
            }
            if (!OneWayOnly)
            {
                elements.Add(_session);
                elements.Add(_compositeDuplex);
            }
            elements.Add(_encoding);
            elements.Add(_transport);

            return elements;
        }

       
        
        /// <summary>
        /// Gets the scheme used by the binding, soap.amqp
        /// </summary>
        public override string Scheme
        {
            get { return "soap.amqp"; }
        }

        /// <summary>
        /// Specifies the broker host 
        /// </summary>
        [ConfigurationProperty("host")]
        public string Host
        {
            get { return _host; }
            set { _host = value; }
        }

        /// <summary>
        /// Specifies the broker port 
        /// </summary>
        public int PortNumber
        {
            get { return _port; }
            set { _port = value; }
        }

        /// <summary>
        /// Specifies the username
        /// </summary>
        public string UserName
        {
            get { return _username; }
            set { _username = value; }
        }

        /// <summary>
        /// Specifies the password
        /// </summary>
        public string Password
        {
            get { return _password; }
            set { _password = value; }
        }

        /// <summary>
        /// Specifies the virtualhost
        /// </summary>
        public string VirtualHost
        {
            get { return _virtuaHost; }
            set { _virtuaHost = value; }
        }


        /// <summary>
        /// Gets the AMQP _transport binding element
        /// </summary>
        public QpidTransportBindingElement Transport
        {
            get { return _transport; }
        }

        /// <summary>
        /// Gets the reliable _session parameters for this binding instance
        /// </summary>
        public ReliableSession ReliableSession
        {
            get { return new ReliableSession(_session); }
        }

        /// <summary>
        /// Determines whether or not the TransactionFlowBindingElement will 
        /// be added to the channel stack
        /// </summary>
        public bool TransactionFlow
        {
            get { return _transactionsEnabled; }
            set { _transactionsEnabled = value; }
        }

        /// <summary>
        /// Specifies whether or not the CompositeDuplex and ReliableSession
        /// binding elements are added to the channel stack.
        /// </summary>
        public bool OneWayOnly
        {
            get { return _oneWayOnly; }
            set { _oneWayOnly = value; }
        }
    }
}