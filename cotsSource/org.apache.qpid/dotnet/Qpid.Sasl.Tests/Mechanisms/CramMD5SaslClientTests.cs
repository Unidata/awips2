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
   public class CramMD5SaslClientTests : ISaslCallbackHandler
   {
      private const string USERNAME = "testuser";
      private const string PASSWORD = "tanstaaftanstaaf";
      private const string AUTHID = "test";
      
      [Test]
      public void ReturnsRightMechanismName()
      {
         ISaslClient client = new CramMD5SaslClient(AUTHID, new Hashtable(), this);

         Assert.AreEqual("CRAM-MD5", client.MechanismName);
      }

      [Test]
      public void HasInitialResponseReturnsFalse()
      {
         ISaslClient client = new CramMD5SaslClient(AUTHID, new Hashtable(), this);

         Assert.IsFalse(client.HasInitialResponse);
      }

      [Test]
      public void CanEvaluateChallenge()
      {
         Hashtable props = new Hashtable();

         ISaslClient client = new CramMD5SaslClient(AUTHID, props, this);

         Assert.IsFalse(client.IsComplete);

         byte[] challenge = 
            Encoding.UTF8.GetBytes("<1896.697170952@postoffice.reston.mci.net>");
         byte[] response = client.EvaluateChallenge(challenge);
         string[] parts = Encoding.UTF8.GetString(response).Split(' ');
         
         Assert.AreEqual(2, parts.Length);
         Assert.AreEqual(USERNAME, parts[0]);
         Assert.AreEqual("b913a602c7eda7a495b4e6e7334d3890", parts[1]);
         Assert.IsTrue(client.IsComplete);
      }

      void ISaslCallbackHandler.Handle(ISaslCallback[] callbacks)
      {
         foreach ( ISaslCallback cb in callbacks )
         {
            if ( cb is NameCallback )
            {
               ((NameCallback)cb).Text = USERNAME;
            } else if ( cb is PasswordCallback )
            {
               ((PasswordCallback)cb).Text = PASSWORD;
            }
         }
      }
   } // class CramMD5SaslClientTests

} // namespace Apache.Qpid.Sasl.Tests.Mechanisms
