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

namespace Apache.Qpid.Sasl.Tests
{
   [TestFixture]
   public class SaslTests : ISaslCallbackHandler
   {

      [Test]
      public void CanCreatePlain()
      {
         Hashtable props = new Hashtable();
         string[] mechanisms = new string[] { "PLAIN", "OTHER" };
         ISaslClient client = Sasl.CreateClient(mechanisms, "", "", "", props, this);
         
         Assert.IsNotNull(client);
         Assert.IsInstanceOfType(typeof(PlainSaslClient), client);
      }

      [Test]
      public void CanCreateCramMD5()
      {
         Hashtable props = new Hashtable();
         string[] mechanisms = new string[] { "CRAM-MD5", "OTHER" };
         ISaslClient client = Sasl.CreateClient(mechanisms, "", "", "", props, this);

         Assert.IsNotNull(client);
         Assert.IsInstanceOfType(typeof(CramMD5SaslClient), client);
      }

      [Test]
      public void CanCreateAnonymous()
      {
         Hashtable props = new Hashtable();
         string[] mechanisms = new string[] { "ANONYMOUS", "OTHER" };
         ISaslClient client = Sasl.CreateClient(mechanisms, "", "", "", props, this);

         Assert.IsNotNull(client);
         Assert.IsInstanceOfType(typeof(AnonymousSaslClient), client);
      }

      [Test]
      public void CanCreateDigest()
      {
         Hashtable props = new Hashtable();
         string[] mechanisms = new string[] { "DIGEST-MD5", "OTHER" };
         ISaslClient client = Sasl.CreateClient(mechanisms, "", "", "", props, this);

         Assert.IsNotNull(client);
         Assert.IsInstanceOfType(typeof(DigestSaslClient), client);
      }

      [Test]
      public void CanCreateExternal()
      {
         Hashtable props = new Hashtable();
         string[] mechanisms = new string[] { "EXTERNAL", "OTHER" };
         ISaslClient client = Sasl.CreateClient(mechanisms, "", "", "", props, this);

         Assert.IsNotNull(client);
         Assert.IsInstanceOfType(typeof(ExternalSaslClient), client);
      }

      [Test]
      public void ReturnsNullIfNoFactoryFound()
      {
         Hashtable props = new Hashtable();
         props.Add(SaslProperties.PolicyNoPlainText, true);
         string[] mechanisms = new string[] { "PLAIN", "OTHER" };
         ISaslClient client = Sasl.CreateClient(mechanisms, "", "", "", props, this);

         Assert.IsNull(client);
      }

      [Test]
      public void ParsesConfigurationSection()
      {
         // if the TEST mechanism is available, then we know
         // the configuration section worked!
         Hashtable props = new Hashtable();
         string[] mechanisms = new string[] { "TEST" };
         ISaslClient client = Sasl.CreateClient(mechanisms, "", "", "", props, this);

         Assert.IsNotNull(client);
         Assert.IsInstanceOfType(typeof(TestSaslClient), client);
      }

      [Test]
      public void ChoosesStrongerMechanism()
      {
         Hashtable props = new Hashtable();
         string[] mechanisms = new string[] { "PLAIN", "OTHER", "CRAM-MD5" };
         ISaslClient client = Sasl.CreateClient(mechanisms, "", "", "", props, this);

         Assert.IsNotNull(client);
         Assert.IsInstanceOfType(typeof(CramMD5SaslClient), client);
      }


      void ISaslCallbackHandler.Handle(ISaslCallback[] callbacks)
      {
      }

   } // class SaslTests

} // namespace Apache.Qpid.Sasl.Tests
