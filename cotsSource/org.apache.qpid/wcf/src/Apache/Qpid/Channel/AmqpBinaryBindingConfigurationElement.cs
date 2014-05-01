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

    public class AmqpBinaryBindingConfigurationElement : AmqpBindingConfigurationElement
    {
        public AmqpBinaryBindingConfigurationElement(string configurationName)
            : base(configurationName)
        {
        }

        public AmqpBinaryBindingConfigurationElement()
            : this(null)
        {
        }

        protected override Type BindingElementType
        {
            get { return typeof(AmqpBinaryBinding); }
        }

        protected override ConfigurationPropertyCollection Properties
        {
            get
            {
                ConfigurationPropertyCollection properties = base.Properties;

                return properties;
            }
        }

        protected override void InitializeFrom(Binding binding)
        {
            base.InitializeFrom(binding);
            AmqpBinaryBinding amqpBinding = (AmqpBinaryBinding)binding;
        }

        protected override void OnApplyConfiguration(Binding binding)
        {
            if (binding == null)
                throw new ArgumentNullException("binding");

            if (binding.GetType() != typeof(AmqpBinaryBinding))
            {
                throw new ArgumentException(string.Format("Invalid type for configuring an AMQP binding. Expected type: {0}. Type passed in: {1}.",
                    typeof(AmqpBinaryBinding).AssemblyQualifiedName,
                    binding.GetType().AssemblyQualifiedName));
            }

            base.OnApplyConfiguration(binding);
        }
    }
}
