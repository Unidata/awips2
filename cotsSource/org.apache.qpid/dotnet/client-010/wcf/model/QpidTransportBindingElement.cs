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
using System.Configuration;
using System.ServiceModel.Channels;
using org.apache.qpid.client;

namespace org.apache.qpid.wcf.model
{
    public sealed class QpidTransportBindingElement : TransportBindingElement
    {
        private Client _connection;
        private string _host;
        private int _port;
        private string _username;
        private string _password;
        private string _virtuaHost;

        /// <summary>
        /// Creates a new instance of the QpidTransportBindingElement Class 
        /// </summary>
        public QpidTransportBindingElement()
        {
            _host = "localhost";
            _port = 5672;
            _username = "guest";
            _password = "guest";
            _virtuaHost = "test";
        }

        private QpidTransportBindingElement(QpidTransportBindingElement other)
            : this()
        {
            Connection = other.Connection;
            Host = other.Host;
            PortNumber = other.PortNumber;
            UserName = other.UserName;
            Password = other.Password;
        }


        public override IChannelFactory<TChannel> BuildChannelFactory<TChannel>(BindingContext context)
        {
            if (Host == null)
                throw new InvalidOperationException("No broker was specified.");
            return (IChannelFactory<TChannel>) new QpidChannelFactory(context);
        }

        public override IChannelListener<TChannel> BuildChannelListener<TChannel>(BindingContext context)
        {
            if (Host == null)
                throw new InvalidOperationException("No broker was specified.");

            return (IChannelListener<TChannel>) ((object) new QpidChannelListener(context));
        }

        public override bool CanBuildChannelFactory<TChannel>(BindingContext context)
        {
            return typeof (TChannel) == typeof (IOutputChannel);
        }

        public override bool CanBuildChannelListener<TChannel>(BindingContext context)
        {
            return typeof (TChannel) == typeof (IInputChannel);
        }

        public override BindingElement Clone()
        {
            return new QpidTransportBindingElement(this);
        }

        public override T GetProperty<T>(BindingContext context)
        {
            return context.GetInnerProperty<T>();
        }

        /// <summary>
        /// Gets the scheme used by the binding, this is 0.10 as default for now.
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
        /// Specifies the connection 
        /// </summary>
        public Client Connection
        {
            get { return _connection; }
            set { _connection = value; }
        }


        internal ClientSession Open(long timeout)
        {
            if (Connection == null)
            {
                Connection = new Client();
            }            
            Connection.connect(Host, PortNumber, VirtualHost, UserName, Password);           
            return Connection.createSession(timeout);
        }

        internal void Close()
        {
            if (Connection != null)
            {
                try
                {
                    Connection.close();
                }
                catch (Exception e)
                {
                   // todo log it 
                }
            }
        }
    }
}
