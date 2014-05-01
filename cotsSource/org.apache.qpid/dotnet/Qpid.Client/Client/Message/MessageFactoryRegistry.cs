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
using Apache.Qpid.Framing;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Client.Message
{
   public class MessageFactoryRegistry
   {
      private readonly Hashtable _mimeToFactoryMap = new Hashtable();
      private IMessageFactory _defaultFactory;

      /// <summary>
      /// Default factory to use for unknown message types
      /// </summary>
      public IMessageFactory DefaultFactory
      {
         get { return _defaultFactory; }
         set { _defaultFactory = value; }
      }

      /// <summary>
      /// Register a new message factory for a MIME type
      /// </summary>
      /// <param name="mimeType">Mime type to register</param>
      /// <param name="mf"></param>
      public void RegisterFactory(string mimeType, IMessageFactory mf)
      {
         if ( mf == null )
            throw new ArgumentNullException("mf");
         if ( mimeType == null || mimeType.Length == 0 )
            throw new ArgumentNullException("mimeType");

         _mimeToFactoryMap[mimeType] = mf;
      }

      /// <summary>
      /// Remove a message factory
      /// </summary>
      /// <param name="mimeType">MIME type to unregister</param>
      public void DeregisterFactory(string mimeType)
      {
         _mimeToFactoryMap.Remove(mimeType);
      }

      /// <summary>
      /// Create a message. This looks up the MIME type from the content header and instantiates the appropriate
      /// concrete message type.
      /// </summary>
      /// <param name="messageNbr">the AMQ message id</param>
      /// <param name="redelivered">true if redelivered</param>
      /// <param name="contentHeader">the content header that was received</param>
      /// <param name="bodies">a list of ContentBody instances</param>
      /// <returns>the message.</returns>
      /// <exception cref="AMQException"/>
      /// <exception cref="QpidException"/>
      public AbstractQmsMessage CreateMessage(long messageNbr, bool redelivered,
                                              ContentHeaderBody contentHeader,
                                              IList bodies)
      {
         BasicContentHeaderProperties properties = (BasicContentHeaderProperties)contentHeader.Properties;

         if ( properties.ContentType == null )
         {
            properties.ContentType = "";
         }

         IMessageFactory mf = GetFactory(properties.ContentType);
         return mf.CreateMessage(messageNbr, redelivered, contentHeader, bodies);
      }

      /// <summary>
      /// Create a new message of the specified type
      /// </summary>
      /// <param name="mimeType">The Mime type</param>
      /// <returns>The new message</returns>
      public AbstractQmsMessage CreateMessage(string mimeType)
      {
         if ( mimeType == null || mimeType.Length == 0 )
            throw new ArgumentNullException("mimeType");

         IMessageFactory mf = GetFactory(mimeType);
         return mf.CreateMessage(mimeType);
      }

      /// <summary>
      /// Construct a new registry with the default message factories registered
      /// </summary>
      /// <returns>a message factory registry</returns>
      public static MessageFactoryRegistry NewDefaultRegistry()
      {
         MessageFactoryRegistry mf = new MessageFactoryRegistry();
         mf.RegisterFactory("text/plain", new QpidTextMessageFactory());
         mf.RegisterFactory("text/xml", new QpidTextMessageFactory());
         mf.RegisterFactory("application/octet-stream", new QpidBytesMessageFactory());

         mf.DefaultFactory = new QpidBytesMessageFactory();
         return mf;
      }

      private IMessageFactory GetFactory(string mimeType)
      {
         IMessageFactory mf = (IMessageFactory)_mimeToFactoryMap[mimeType];
         return mf != null ? mf : _defaultFactory;
      }
   }
}


