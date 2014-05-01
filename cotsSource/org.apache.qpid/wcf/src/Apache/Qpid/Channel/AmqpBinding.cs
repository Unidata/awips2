/*
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
*/

namespace Apache.Qpid.Channel
{
    using System;
    using System.Collections.Generic;
    using System.Collections.ObjectModel;
    using System.Configuration;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.ServiceModel.Configuration;

    using Apache.Qpid.AmqpTypes;

    public class AmqpBinding : Binding
    {
        protected AmqpTransportBindingElement transport;
        protected MessageEncodingBindingElement encoding;

        public AmqpBinding()
        {
            transport = new AmqpTransportBindingElement();
            encoding = new BinaryMessageEncodingBindingElement();
        }

        protected AmqpBinding(MessageEncodingBindingElement encoding)
        {
            this.encoding = encoding;
            transport = new AmqpTransportBindingElement();
        }

        public AmqpBinding(string configurationName)
            : this()
        {
            ApplyConfiguration(configurationName);
        }

        public string BrokerHost
        {
            get { return transport.BrokerHost; }
            set { transport.BrokerHost = value; }
        }

        public int BrokerPort
        {
            get { return transport.BrokerPort; }
            set { transport.BrokerPort = value; }
        }

        public int PrefetchLimit
        {
            get { return transport.PrefetchLimit; }
            set { transport.PrefetchLimit = value; }
        }

        public bool Shared
        {
            get { return transport.Shared; }
            set { transport.Shared = value; }
        }

        public TransferMode TransferMode
        {
            get { return transport.TransferMode; }
            set { transport.TransferMode = value; }
        }

        public AmqpProperties DefaultMessageProperties
        {
            get { return transport.DefaultMessageProperties; }
            set { transport.DefaultMessageProperties = value; }
        }

        public override string Scheme
        {
            get { return AmqpConstants.Scheme; }
        }

        public override BindingElementCollection CreateBindingElements()
        {
            BindingElementCollection bindingElements = new BindingElementCollection();

            bindingElements.Add(encoding);
            bindingElements.Add(transport);

            return bindingElements.Clone();
        }

        private void ApplyConfiguration(string configurationName)
        {
            AmqpBindingCollectionElement section = (AmqpBindingCollectionElement)ConfigurationManager.GetSection(AmqpConstants.AmqpBindingSectionName);
            AmqpBindingConfigurationElement element = section.Bindings[configurationName];
            if (element == null)
            {
                throw new ConfigurationErrorsException(string.Format(System.Globalization.CultureInfo.CurrentCulture,
                    "There is no binding named {0} at {1}.", configurationName, section.BindingName));
            }
            else
            {
                element.ApplyConfiguration(this);
            }
        }
    }
}
