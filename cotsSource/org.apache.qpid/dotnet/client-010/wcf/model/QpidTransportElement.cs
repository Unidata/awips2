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
using System.Reflection;
using System.ServiceModel.Channels;
using System.ServiceModel.Configuration;

namespace org.apache.qpid.wcf.model
{
    public sealed class QpidTransportElement : TransportElement
    {

        public override void ApplyConfiguration(BindingElement bindingElement)
        {
            base.ApplyConfiguration(bindingElement);
            if (bindingElement == null)
                throw new ArgumentNullException("bindingElement");

            var bindind = bindingElement as QpidTransportBindingElement;
            if (bindind == null)
            {
                throw new ArgumentException(
                    string.Format("Invalid type for binding. Expected {0}, Passed: {1}",
                                  typeof(QpidTransportBindingElement).AssemblyQualifiedName,
                                  bindingElement.GetType().AssemblyQualifiedName));
            }

            bindind.Host = Host;
            bindind.Password = Password;
            bindind.UserName = UserName;
            bindind.VirtualHost = VirtualHost;
            bindind.PortNumber = PortNumber;
        }

        public override void CopyFrom(ServiceModelExtensionElement from)
        {
            base.CopyFrom(from);
            var element = from as QpidTransportElement;
            if (element != null)
            {
                Host = element.Host;
                PortNumber = element.PortNumber;
                Password = element.Password;
                UserName = element.UserName;
                VirtualHost = element.VirtualHost;
            }
        }

        protected override BindingElement CreateBindingElement()
        {
            TransportBindingElement element = CreateDefaultBindingElement();
            ApplyConfiguration(element);
            return element;
        }

        protected override TransportBindingElement CreateDefaultBindingElement()
        {
            return new QpidTransportBindingElement();
        }

        protected override void InitializeFrom(BindingElement bindingElement)
        {
            base.InitializeFrom(bindingElement);

            if (bindingElement == null)
                throw new ArgumentNullException("bindingElement");

            var binding = bindingElement as QpidTransportBindingElement;
            if (binding == null)
            {
                throw new ArgumentException(
                    string.Format("Invalid type for binding. Expected {0}, Passed: {1}",
                                  typeof(QpidTransportBindingElement).AssemblyQualifiedName,
                                  bindingElement.GetType().AssemblyQualifiedName));
            }

            Host = binding.Host;
            PortNumber = binding.PortNumber;
            Password = binding.Password;
            UserName = binding.UserName;
            VirtualHost = binding.VirtualHost;
        }

        public override Type BindingElementType
        {
            get { return typeof(QpidTransportElement); }
        }

       

        /// <summary>
        /// Specifies the broker host name that the binding should connect to.
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
        /// Password to use when authenticating with the broker
        /// </summary>
        [ConfigurationProperty("password", DefaultValue = "guest")]
        public string Password
        {
            get { return ((string)base["password"]); }
            set { base["password"] = value; }
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


        protected override ConfigurationPropertyCollection Properties
        {
            get
            {
                ConfigurationPropertyCollection configProperties = base.Properties;
                foreach (PropertyInfo prop in GetType().GetProperties(BindingFlags.DeclaredOnly
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