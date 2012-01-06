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

    public class AmqpBindingConfigurationElement : StandardBindingElement
    {
        // not regular config elements.  See PostDeserialize
        string brokerHost;
        int brokerPort;

        public AmqpBindingConfigurationElement(string configurationName)
            : base(configurationName)
        {
            brokerHost = AmqpDefaults.BrokerHost;
            brokerPort = AmqpDefaults.BrokerPort;
        }

        public AmqpBindingConfigurationElement()
            : this(null)
        {
        }

        protected override Type BindingElementType
        {
            get { return typeof(AmqpBinding); }
        }

        public string BrokerHost
        {
            get { return brokerHost; }
            set { brokerHost = value; }
        }

        public int BrokerPort
        {
            get { return brokerPort; }
            set { brokerPort = value; }
        }

        [ConfigurationProperty(AmqpConfigurationStrings.PrefetchLimit, DefaultValue = false)]
        public int PrefetchLimit
        {
            get { return (int)base[AmqpConfigurationStrings.PrefetchLimit]; }
            set { base[AmqpConfigurationStrings.PrefetchLimit] = value; }
        }

        [ConfigurationProperty(AmqpConfigurationStrings.Shared, DefaultValue = false)]
        public bool Shared
        {
            get { return (bool)base[AmqpConfigurationStrings.Shared]; }
            set { base[AmqpConfigurationStrings.Shared] = value; }
        }

        [ConfigurationProperty(AmqpConfigurationStrings.TransferMode, DefaultValue = AmqpDefaults.TransferMode)]
        public TransferMode TransferMode
        {
            get { return (TransferMode)base[AmqpConfigurationStrings.TransferMode]; }
            set { base[AmqpConfigurationStrings.TransferMode] = value; }
        }

        [ConfigurationProperty(AmqpConfigurationStrings.Brokers)]
        public BrokerCollection Brokers
        {
            get
            {
                return (BrokerCollection)base[AmqpConfigurationStrings.Brokers];
            }
            set
            {
                base[AmqpConfigurationStrings.Brokers] = value;
            }
        }

        protected override ConfigurationPropertyCollection Properties
        {
            get
            {
                ConfigurationPropertyCollection properties = base.Properties;
                properties.Add(new ConfigurationProperty(AmqpConfigurationStrings.PrefetchLimit,
                    typeof(int), 0, null, null, ConfigurationPropertyOptions.None));
                properties.Add(new ConfigurationProperty(AmqpConfigurationStrings.Shared,
                    typeof(bool), false, null, null, ConfigurationPropertyOptions.None));
                properties.Add(new ConfigurationProperty(AmqpConfigurationStrings.TransferMode,
                    typeof(TransferMode), AmqpDefaults.TransferMode, null, null, ConfigurationPropertyOptions.None));
                properties.Add(new ConfigurationProperty("brokers", typeof(BrokerCollection), null));
                return properties;
            }
        }

        protected override void InitializeFrom(Binding binding)
        {
            base.InitializeFrom(binding);
            AmqpBinding amqpBinding = (AmqpBinding)binding;
            this.BrokerHost = amqpBinding.BrokerHost;
            this.BrokerPort = amqpBinding.BrokerPort;
            this.TransferMode = amqpBinding.TransferMode;
            this.Shared = amqpBinding.Shared;
            this.PrefetchLimit = amqpBinding.PrefetchLimit;

            AmqpProperties props = amqpBinding.DefaultMessageProperties;
        }

        protected override void OnApplyConfiguration(Binding binding)
        {
            if (binding == null)
                throw new ArgumentNullException("binding");

            if (!(binding is AmqpBinding))
            {
                throw new ArgumentException(string.Format("Invalid type for configuring an AMQP binding. Expected type: {0}. Type passed in: {1}.",
                    typeof(AmqpBinding).AssemblyQualifiedName,
                    binding.GetType().AssemblyQualifiedName));
            }

            AmqpBinding amqpBinding = (AmqpBinding)binding;
            amqpBinding.BrokerHost = this.BrokerHost;
            amqpBinding.BrokerPort = this.BrokerPort;
            amqpBinding.TransferMode = this.TransferMode;
            amqpBinding.Shared = this.Shared;
            amqpBinding.PrefetchLimit = this.PrefetchLimit;
        }

        protected override void PostDeserialize()
        {
            base.PostDeserialize();

            BrokerCollection brokers = Brokers;
            if (brokers != null)
            {
                if (brokers.Count > 0)
                {
                    // just grab the first element until failover is supported
                    System.Collections.IEnumerator brokersEnum = brokers.GetEnumerator();
                    // move to first element
                    brokersEnum.MoveNext();
                    BrokerElement be = (BrokerElement)brokersEnum.Current;
                    this.BrokerHost = be.Host;
                    this.BrokerPort = be.Port;
                }
            }
        }
    }

    public class BrokerCollection : ConfigurationElementCollection
    {
        public BrokerCollection()
        {
            //this.AddElementName = "broker";
        }

        protected override ConfigurationElement CreateNewElement()
        {
            return new BrokerElement();
        }

        protected override void BaseAdd(ConfigurationElement element)
        {
            BrokerElement be = (BrokerElement)element;
            if (this.BaseGet((Object)be.Key) != null)
            {
                throw new ConfigurationErrorsException("duplicate broker definition at line " + element.ElementInformation.LineNumber);
            }
            base.BaseAdd(element);
        }

        protected override Object GetElementKey(ConfigurationElement element)
        {
            BrokerElement be = (BrokerElement) element;
            return be.Key;
        }

        protected override void PostDeserialize()
        {
            base.PostDeserialize();
            if (this.Count == 0)
            {
                throw new ArgumentException("Brokers collection requires at least one broker");
            }
            if (this.Count > 1)
            {
                Console.WriteLine("Warning: multiple brokers not supported, selecting first instance");
            }
            BrokerElement be = (BrokerElement)this.BaseGet(0);
        }

        protected override string ElementName
        {
            get
            {
                return "broker";
            }
        }

        public override ConfigurationElementCollectionType CollectionType
        {
            get
            {
                return ConfigurationElementCollectionType.BasicMap;
            }
        }
    }

    public class BrokerElement : ConfigurationElement
    {
        string key;

        public BrokerElement()
        {
            Properties.Add(new ConfigurationProperty(AmqpConfigurationStrings.BrokerHost, 
                typeof(string), AmqpDefaults.BrokerHost, null, null, ConfigurationPropertyOptions.None));
            Properties.Add(new ConfigurationProperty(AmqpConfigurationStrings.BrokerPort,
                typeof(int), AmqpDefaults.BrokerPort, null, null, ConfigurationPropertyOptions.None));

        }

        [ConfigurationProperty(AmqpConfigurationStrings.BrokerHost, DefaultValue = AmqpDefaults.BrokerHost)]
        public string Host
        {
            get { return (string)base[AmqpConfigurationStrings.BrokerHost]; }
            set { base[AmqpConfigurationStrings.BrokerHost] = value; }
        }

        [ConfigurationProperty(AmqpConfigurationStrings.BrokerPort, DefaultValue = AmqpDefaults.BrokerPort)]
        public int Port
        {
            get { return (int)base[AmqpConfigurationStrings.BrokerPort]; }
            set { base[AmqpConfigurationStrings.BrokerPort] = value; }
        }

        public string Key
        {
            get
            {
                if (this.key == null)
                {
                    this.key = this.Host + ':' + this.Port;
                }
                return this.key;
            }
        }

    }
}
