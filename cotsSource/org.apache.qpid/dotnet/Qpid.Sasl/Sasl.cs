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

using Apache.Qpid.Sasl.Configuration;

namespace Apache.Qpid.Sasl
{
   /// <summary>
   /// Static class used to access the SASL functionality. 
   /// The core SASL mechanism is described in RFC 2222.
   /// </summary>
   /// <remarks>
   /// Only client side mechanisms are implemented.
   /// <para>
   /// New client side factories can be added programatically using the 
   /// RegisterClientFactory method, or through the application 
   /// configuration file, like this:
   /// </para>
   /// <example><![CDATA[
   /// <configuration>
   ///   <configSections>
   ///      <section name="qpid.sasl" type="Apache.Qpid.Sasl.Configuration.SaslConfigurationSectionHandler, Apache.Qpid.Sasl"/>
   ///   </configSections>
   ///
   ///   <qpid.sasl>
   ///      <clientFactories>
   ///         <add type="Apache.Qpid.Sasl.Tests.TestClientFactory, Apache.Qpid.Sasl.Tests"/>
   ///      </clientFactories>
   ///   </qpid.sasl>
   /// </configuration>
   /// ]]></example>
   /// </remarks>
   public sealed class Sasl
   {
      private static IList _clientFactories;
      

      static Sasl()
      {
         SaslConfiguration config = SaslConfiguration.GetConfiguration();
         _clientFactories = config.ClientFactories;
      }
      private Sasl()
      {
      }

      public static ISaslClient CreateClient(
         string[] mechanisms, string authorizationId,
         string protocol, string serverName,
         IDictionary props, ISaslCallbackHandler handler
         )
      {
         ISaslClientFactory factory = FindFactory(mechanisms, props);
         if ( factory == null )
            return null;

         return factory.CreateClient (
            mechanisms, authorizationId, 
            protocol, serverName, props, handler
            );
      }

      public static void RegisterClientFactory(ISaslClientFactory factory)
      {
         lock ( _clientFactories )
         {
            _clientFactories.Add(factory);
         }
      }

      private static ISaslClientFactory FindFactory(string[] mechanisms, IDictionary props)
      {
         lock ( _clientFactories )
         {
            foreach ( ISaslClientFactory factory in _clientFactories )
            {
               string[] mechs = factory.GetSupportedMechanisms(props);
               foreach ( string m1 in mechs )
               {
                  foreach (string m2 in mechanisms )
                  {
                     if ( m1 == m2 )
                        return factory;
                  }
               }
            }
            return null;
         }
      }
   } // class Sasl

} // namespace Apache.Qpid.Sasl.Mechanisms
