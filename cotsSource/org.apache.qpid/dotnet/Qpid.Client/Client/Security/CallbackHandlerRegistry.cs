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
using Apache.Qpid.Sasl;
using Apache.Qpid.Sasl.Mechanisms;

using Apache.Qpid.Client.Configuration;

namespace Apache.Qpid.Client.Security
{

   /// <summary>
   /// Helper class to map SASL mechanisms to our 
   /// internal ISaslCallbackHandler implementations.
   /// </summary>
   /// <remarks>
   /// The set of configured callback handlers and their order
   /// controls the selection of the SASL mechanism used for authentication.
   /// <para>
   /// You can either replace the default handler for CRAM-MD5 and PLAIN
   /// authentication (the two default options) using the application 
   /// configuration file. Configuration is done by especifying the SASL
   /// mechanism name (e.g PLAIN) and the type implementing the callback handler
   /// used to provide any data required by the mechanism like username and password.
   /// </para>
   /// <para>
   /// Callback handler types should implement the IAMQCallbackHandler interface.
   /// </para>
   /// <para>
   /// New callbacks or authentication mechanisms can be configured like this:
   /// </para>
   /// <example><![CDATA[
   /// <configuration>
   ///   <configSections>
   ///      <sectionGroup name="qpid.client">
   ///         <section name="authentication" type="Apache.Qpid.Client.Configuration.AuthenticationConfigurationSectionHandler, Apache.Qpid.Client"/>
   ///      </sectionGroup>
   ///   </configSections>
   ///   <qpid.client>
   ///      <authentication>
   ///         <add key="TEST" value="Apache.Qpid.Client.Tests.Security.TestCallbackHandler, Apache.Qpid.Client.Tests"/>
   ///      </authentication>
   ///   </qpid.client>
   /// </configuration>
   /// ]]></example>
   /// </remarks>
   public sealed class CallbackHandlerRegistry
   {
      private static CallbackHandlerRegistry _instance = 
         new CallbackHandlerRegistry();
      private OrderedHashTable _mechanism2HandlerMap;
      private string[] _mechanisms;

      public static CallbackHandlerRegistry Instance
      {
         get { return _instance; }
      }

      public string[] Mechanisms
      {
         get { return _mechanisms; }
      }

      private CallbackHandlerRegistry()
      {
         _mechanism2HandlerMap = (OrderedHashTable)
            ConfigurationSettings.GetConfig("qpid.client/authentication");

         // configure default options if not available
         if ( _mechanism2HandlerMap == null )
            _mechanism2HandlerMap = new OrderedHashTable();

         if ( !_mechanism2HandlerMap.Contains(ExternalSaslClient.Mechanism) )
            _mechanism2HandlerMap.Add(ExternalSaslClient.Mechanism, typeof(UsernamePasswordCallbackHandler));
         if ( !_mechanism2HandlerMap.Contains(CramMD5SaslClient.Mechanism) )
            _mechanism2HandlerMap.Add(CramMD5SaslClient.Mechanism, typeof(UsernamePasswordCallbackHandler));
         if ( !_mechanism2HandlerMap.Contains(CramMD5HexSaslClient.Mechanism) )
            _mechanism2HandlerMap.Add(CramMD5HexSaslClient.Mechanism, typeof(UsernamePasswordCallbackHandler));
         if ( !_mechanism2HandlerMap.Contains(PlainSaslClient.Mechanism) )
            _mechanism2HandlerMap.Add(PlainSaslClient.Mechanism, typeof(UsernamePasswordCallbackHandler));

         _mechanisms = new string[_mechanism2HandlerMap.Count];
         _mechanism2HandlerMap.OrderedKeys.CopyTo(_mechanisms, 0);
      }

      public bool IsSupportedMechanism(string mechanism)
      {
         return _mechanism2HandlerMap.Contains(mechanism);
      }

      public string ChooseMechanism(string mechanisms)
      {
         IList mechs = mechanisms.Split(' ');
         foreach ( string supportedMech in _mechanisms )
         {
            if ( mechs.Contains(supportedMech) )
               return supportedMech;
         }
         return null;
      }

      public Type GetCallbackHandler(string mechanism)
      {
         return (Type)_mechanism2HandlerMap[mechanism];
      }
   }
}
