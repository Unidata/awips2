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

    public class AmqpBinaryBinding : AmqpBinding
    {
        public AmqpBinaryBinding()
: base (new RawMessageEncodingBindingElement())
        {
        }

        public AmqpBinaryBinding(string configurationName)
            : this()
        {
            ApplyConfiguration(configurationName);
        }

        private void ApplyConfiguration(string configurationName)
        {
            AmqpBinaryBindingCollectionElement section = (AmqpBinaryBindingCollectionElement)ConfigurationManager.GetSection(AmqpConstants.AmqpBinaryBindingSectionName);
            AmqpBinaryBindingConfigurationElement element = section.Bindings[configurationName];
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
