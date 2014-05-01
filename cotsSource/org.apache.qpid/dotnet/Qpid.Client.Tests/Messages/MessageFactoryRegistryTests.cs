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

using log4net;
using NUnit.Framework;
using Apache.Qpid.Messaging;
using Apache.Qpid.Client.Message;

namespace Apache.Qpid.Client.Tests.Messages
{
   /// <summary>
   /// Ensure a factory creates messages correctly
   /// </summary>
   [TestFixture]
   public class MessageFactoryRegistryTests
   {
      const string TEXT_PLAIN = "text/plain";
      const string TEXT_XML = "text/xml";
      const string OCTET_STREAM = "application/octet-stream";

      /// <summary>
      /// Check default registry can create text/plain messages
      /// </summary>
      [Test]
      public void CanCreateTextPlain()
      {
         MessageFactoryRegistry registry = 
            MessageFactoryRegistry.NewDefaultRegistry();

         IMessage message = registry.CreateMessage(TEXT_PLAIN);
         Assert.IsNotNull(message);
         Assert.AreEqual(TEXT_PLAIN, message.ContentType);
         Assert.IsInstanceOfType(typeof(QpidTextMessage), message);
      }
      /// <summary>
      /// Check default registry can create text/xml messages
      /// </summary>
      [Test]
      public void CanCreateTextXml()
      {
         MessageFactoryRegistry registry =
            MessageFactoryRegistry.NewDefaultRegistry();

         IMessage message = registry.CreateMessage(TEXT_XML);
         Assert.IsNotNull(message);
         Assert.AreEqual(TEXT_XML, message.ContentType);
         Assert.IsInstanceOfType(typeof(QpidTextMessage), message);
      }
      /// <summary>
      /// Check default registry can create application/octet-stream messages
      /// </summary>
      [Test]
      public void CanCreateBinary()
      {
         MessageFactoryRegistry registry =
            MessageFactoryRegistry.NewDefaultRegistry();

         IMessage message = registry.CreateMessage(OCTET_STREAM);
         Assert.IsNotNull(message);
         Assert.AreEqual(OCTET_STREAM, message.ContentType);
         Assert.IsInstanceOfType(typeof(QpidBytesMessage), message);
      }
      /// <summary>
      /// Check default registry can create messages for unknown types
      /// </summary>
      [Test]
      public void CanCreateUnknownType()
      {
         MessageFactoryRegistry registry =
            MessageFactoryRegistry.NewDefaultRegistry();

         const string OTHER = "application/unknown";
         IMessage message = registry.CreateMessage(OTHER);
         Assert.IsNotNull(message);
         Assert.AreEqual(OTHER, message.ContentType);
         Assert.IsInstanceOfType(typeof(QpidBytesMessage), message);
      }
      /// <summary>
      /// Check that text messages default to UTF-8 encoding
      /// </summary>
      [Test]
      public void TextMessagesDefaultToUTF8Encoding()
      {
         MessageFactoryRegistry registry =
            MessageFactoryRegistry.NewDefaultRegistry();

         IMessage message = registry.CreateMessage(TEXT_PLAIN);
         Assert.AreEqual("utf-8", message.ContentEncoding.ToLower());
      }

   }
} // namespace Apache.Qpid.Client.Tests.Messages


