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
using System.Collections.Specialized;
using System.Configuration;
using System.Text;

using Apache.Qpid.Client.Security;
using Apache.Qpid.Sasl.Mechanisms;

namespace Apache.Qpid.Client.Configuration
{
   public class AuthenticationConfigurationSectionHandler 
      : IConfigurationSectionHandler
   {

      public object Create(object parent, object configContext, System.Xml.XmlNode section)
      {
         NameValueSectionHandler handler = new NameValueSectionHandler();
         OrderedHashTable schemes = new OrderedHashTable();

         NameValueCollection options = (NameValueCollection)
            handler.Create(parent, configContext, section);

         if ( options != null )
         {
            foreach ( string key in options.Keys )
            {
               Type type = Type.GetType(options[key]);
               if ( type == null )
                  throw new ConfigurationException(string.Format("Type '{0}' not found", key));
               if ( !typeof(IAMQCallbackHandler).IsAssignableFrom(type) )
                  throw new ConfigurationException(string.Format("Type '{0}' does not implement IAMQCallbackHandler", key));

               schemes.Add(key, type);
            }
         }

         return schemes;
      }

   } // class AuthenticationConfigurationSectionHandler

   public class OrderedHashTable : Hashtable
   {
      private ArrayList _keys = new ArrayList();

      public IList OrderedKeys
      {
         get { return _keys; }
      }

      public override void Add(object key, object value)
      {
         base.Add(key, value);
         _keys.Add(key);
      }
      public override void Remove(object key)
      {
         base.Remove(key);
         _keys.Remove(key);
      }
   }
} // namespace Apache.Qpid.Client.Configuration
