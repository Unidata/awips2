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
using System.Reflection;
using System.ServiceModel.Channels;
using System.ServiceModel.Configuration;
using System.Configuration;

namespace org.apache.qpid.wcf.model
{

   /// <remarks>
    /// This configuration element should be imported into the client
    /// and server configuration files to provide declarative configuration 
    /// of a AMQP bound service.
    /// </remarks>
    public sealed class QpidBindingConfigurationElement : StandardBindingElement
    {
        /// <summary>
        /// Creates a new instance of the QpidBindingConfigurationElement
        /// Class initialized with values from the specified configuration.
        /// </summary>
        /// <param name="configurationName"></param>
        public QpidBindingConfigurationElement(string configurationName)
            : base(configurationName)
        {
        }

        /// <summary>
        /// Creates a new instance of the RabbitMQBindingConfigurationElement Class.
        /// </summary>
        public QpidBindingConfigurationElement()
            : this(null)
        {
        }


        protected override void InitializeFrom(Binding binding)
        {
            base.InitializeFrom(binding);
            QpidBinding qpidbinding = binding as QpidBinding;
            if (qpidbinding != null)
            {
                Host = qpidbinding.Host;
                OneWayOnly = qpidbinding.OneWayOnly;
                TransactionFlowEnabled = qpidbinding.TransactionFlow;
                VirtualHost = qpidbinding.VirtualHost;
                PortNumber = qpidbinding.PortNumber;
                UserName = qpidbinding.UserName;
                Password = qpidbinding.Password;                
            }
        }

        protected override void OnApplyConfiguration(Binding binding)
        {
            if (binding == null)
                throw new ArgumentNullException("binding");

            var qpidbinding = binding as QpidBinding;
            if (qpidbinding == null)
            {
                throw new ArgumentException(
                    string.Format("Invalid type for binding. Expected {0}, Passed: {1}",
                        typeof(QpidBinding).AssemblyQualifiedName,
                        binding.GetType().AssemblyQualifiedName));
            }

            qpidbinding.Host = Host;
            qpidbinding.OneWayOnly = OneWayOnly;
            qpidbinding.TransactionFlow = TransactionFlowEnabled;
            qpidbinding.Password = Password;
            qpidbinding.UserName = UserName;
            qpidbinding.VirtualHost = VirtualHost;
            qpidbinding.PortNumber = PortNumber;
        }
       

        /// <summary>
        /// Specifies the host that the binding should connect to.
        /// </summary>
        [ConfigurationProperty("host", DefaultValue = "localhost")]
        public string Host
        {
            get { return ((string) base["host"]); }
            set { base["host"] = value; }            
        }

        /// <summary>
        /// Specifies the broker port number that the binding should connect to.
        /// </summary>
        [ConfigurationProperty("port", DefaultValue = "5672")]
        public int PortNumber
        {
            get { return (Convert.ToInt16(base["port"])); }
            set { base["port"] = value; }
        }


        /// <summary>
        /// Specifies whether or not the CompositeDuplex and ReliableSession
        /// binding elements are added to the channel stack.
        /// </summary>
        [ConfigurationProperty("oneWay", DefaultValue = false)]
        public bool OneWayOnly
        {
            get { return ((bool)base["oneWay"]); }
            set { base["oneWay"] = value; }
        }

        /// <summary>
        /// Password to use when authenticating with the broker
        /// </summary>
        [ConfigurationProperty("password", DefaultValue = "guest")]
        public string Password
        {
            get { return ((string)base["password"]); }
            set { base["password"] = value; }
        }

        /// <summary>
        /// Specifies whether or not WS-AtomicTransactions are supported by the binding
        /// </summary>
        [ConfigurationProperty("transactionFlow", DefaultValue = false)]
        public bool TransactionFlowEnabled
        {
            get { return ((bool)base["transactionFlow"]); }
            set { base["transactionFlow"] = value; }
        }

        /// <summary>
        /// The username  to use when authenticating with the broker
        /// </summary>
        [ConfigurationProperty("username", DefaultValue = "guest")]
        public string UserName
        {
            get { return ((string)base["username"]); }
            set { base["username"] = value; }
        }

       


        /// <summary>
        /// The virtual host to access.
        /// </summary>
        [ConfigurationProperty("virtualHost", DefaultValue = "test")]
        public string VirtualHost
        {
            get { return ((string)base["virtualHost"]); }
            set { base["virtualHost"] = value; }
        }

        ///<summary>The security realm to use when calling IModel.AccessRequest</summary>
        [ConfigurationProperty("realm", DefaultValue = "plain")]
        public string Realm
        {
            get { return ((string)base["realm"]); }
            set { base["realm"] = value; }
        }

        protected override Type BindingElementType
        {
            get { return typeof(QpidBinding); }
        }

        protected override ConfigurationPropertyCollection Properties
        {
            get
            {
                ConfigurationPropertyCollection configProperties = base.Properties;
                foreach (PropertyInfo prop in this.GetType().GetProperties(BindingFlags.DeclaredOnly
                                                                           | BindingFlags.Public
                                                                           | BindingFlags.Instance))
                {
                    foreach (ConfigurationPropertyAttribute attr in prop.GetCustomAttributes(typeof(ConfigurationPropertyAttribute), false))
                    {
                        configProperties.Add(
                            new ConfigurationProperty(attr.Name, prop.PropertyType, attr.DefaultValue));
                    }
                }

                return configProperties;
            }
        }
    }
}
