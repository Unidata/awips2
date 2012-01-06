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
using System.Text;

using NUnit.Framework;
using Apache.Qpid.Sasl;
using Apache.Qpid.Sasl.Mechanisms;

namespace Apache.Qpid.Sasl.Tests.Mechanisms
{
   [TestFixture]
   public class AnonymousSaslClientTests : ISaslCallbackHandler
   {
      private const string AUTHID = "nobody@nowhere.com";
      
      [Test]
      public void ReturnsRightMechanismName()
      {
         ISaslClient client = new AnonymousSaslClient(AUTHID, new Hashtable(), this);

         Assert.AreEqual("ANONYMOUS", client.MechanismName);
      }

      [Test]
      public void HasInitialResponseReturnsTrue()
      {
         ISaslClient client = new AnonymousSaslClient(AUTHID, new Hashtable(), this);

         Assert.IsTrue(client.HasInitialResponse);
      }

      [Test]
      public void CanEvaluateChallenge()
      {
         Hashtable props = new Hashtable();
         ISaslClient client = new AnonymousSaslClient(AUTHID, props, this);

         Assert.IsFalse(client.IsComplete);
         byte[] response = client.EvaluateChallenge(new byte[0]);
         Assert.AreEqual(AUTHID, Encoding.UTF8.GetString(response));
         
         Assert.IsTrue(client.IsComplete);
      }

      void ISaslCallbackHandler.Handle(ISaslCallback[] callbacks)
      {
      }

   } // class AnonymousSaslClientTests

} // namespace Apache.Qpid.Sasl.Tests.Mechanisms
