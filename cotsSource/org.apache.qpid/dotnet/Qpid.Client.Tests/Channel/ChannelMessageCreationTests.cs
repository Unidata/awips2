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
using Apache.Qpid.Client;
using Apache.Qpid.Client.Message;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Client.Tests.Channel
{
   /// <summary>
   /// Test that channels can create messages correctly
   /// </summary>
   [TestFixture]
   public class ChannelMessageCreationTests
   {
      [Test]
      public void CanCreateTextMessage()
      {
         IChannel channel = AmqChannel.CreateDisconnectedChannel();
         ITextMessage msg = channel.CreateTextMessage();
         Assert.IsNotNull(msg);
      }
      [Test]
      public void CanCreateTextMessageWithContent()
      {
         IChannel channel = AmqChannel.CreateDisconnectedChannel();
         const string CONTENT = "1234567890";
         ITextMessage msg = channel.CreateTextMessage(CONTENT);
         Assert.IsNotNull(msg);
         Assert.AreEqual(CONTENT, msg.Text);
      }
      [Test]
      public void CanCreateBytesMessage()
      {
         IChannel channel = AmqChannel.CreateDisconnectedChannel();
         IBytesMessage msg = channel.CreateBytesMessage();
         Assert.IsNotNull(msg);
      }
      [Test]
      public void CanCreateMessage()
      {
         IChannel channel = AmqChannel.CreateDisconnectedChannel();
         IMessage msg = channel.CreateMessage();
         Assert.IsNotNull(msg);
      }
      [Test]
      public void CanCreateMessageFromMimeType()
      {
         IChannel channel = AmqChannel.CreateDisconnectedChannel();
         IMessage msg = channel.CreateMessage("text/xml");
         Assert.IsNotNull(msg);
         Assert.IsInstanceOfType(typeof(ITextMessage), msg);
      }
   }
} // namespace Apache.Qpid.Client.Tests.Channel


