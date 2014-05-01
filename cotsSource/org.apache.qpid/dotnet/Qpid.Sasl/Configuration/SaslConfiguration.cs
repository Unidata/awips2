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
using System.Collections;
using System.Configuration;
using System.Text;
using System.Xml;

namespace Apache.Qpid.Sasl.Configuration
{
   /// <summary>
   /// Represents an Sasl configuration section
   /// in the config file
   /// </summary>
   internal class SaslConfiguration
   {
      private IList _clientFactories;

      /// <summary>
      /// Set of configured client factores
      /// </summary>
      public IList ClientFactories
      {
         get { return _clientFactories; }
      }

      internal SaslConfiguration(IList clientFactoryTypes)
      {
         _clientFactories = new ArrayList();
         foreach ( Type type in clientFactoryTypes )
         {
            _clientFactories.Add(Activator.CreateInstance(type));
         }
      }

      /// <summary>
      /// Get the configuration for the library
      /// </summary>
      /// <returns>The configuration from app.config or a default configuration</returns>
      internal static SaslConfiguration GetConfiguration()
      {
         // 'obsolete' warning, but needed for .NET 1.1 compatibility
         SaslConfiguration config = (SaslConfiguration)
            ConfigurationSettings.GetConfig("qpid.sasl");
         if ( config == null )
         {
            // create default configuration
            IList clientFactories = GetDefaultClientFactories();
            config = new SaslConfiguration(clientFactories);
         }
         return config;
      }

      /// <summary>
      /// Create a list filled with the default client
      /// factories supported by the library
      /// </summary>
      /// <returns>The list of client factory types</returns>
      internal static IList GetDefaultClientFactories()
      {
         IList clientFactories = new ArrayList();
         clientFactories.Add(typeof(DefaultClientFactory));
         return clientFactories;
      }


   } // class SaslConfiguration

} // namespace Apache.Qpid.Sasl.Configuration


