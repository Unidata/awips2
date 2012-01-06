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
   /// Defines the configuration section to configure extra 
   /// Sasl client factories
   /// </summary>
   public class SaslConfigurationSectionHandler 
      : IConfigurationSectionHandler
   {
      public object Create(object parent, object configContext, XmlNode section)
      {
         IList clientFactories = SaslConfiguration.GetDefaultClientFactories();

         foreach ( XmlNode node in section.ChildNodes )
         {
            if ( node.LocalName == "clientFactories" )
            {
               ProcessFactories(node, clientFactories);
            }
         }

         SaslConfiguration config = new SaslConfiguration(clientFactories);
         return config;
      }

      
      private void ProcessFactories(XmlNode node, IList factories)
      {
         foreach ( XmlNode child in node.ChildNodes )
         {
            Type type;
            switch ( child.LocalName )
            {
            case "add":
               type = Type.GetType(child.Attributes["type"].Value);
               if ( !factories.Contains(type) )
                  factories.Add(type);
               break;
            case "remove":
               type = Type.GetType(child.Attributes["type"].Value);
               if ( factories.Contains(type) )
                  factories.Remove(type);
               break;
            case "clear":
               factories.Clear();
               break;
            default:
               // gives obsolete warning but needed for .NET 1.1 support
               throw new ConfigurationException(string.Format("Unknown element '{0}' in section '{0}'", child.LocalName, node.LocalName));
            }
         }
      }
   } // class SaslConfigurationSectionHandler

} // namespace Apache.Qpid.Sasl.Configuration


