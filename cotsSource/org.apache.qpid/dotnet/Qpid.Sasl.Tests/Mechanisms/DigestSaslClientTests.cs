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
using System.Text;

using NUnit.Framework;
using Apache.Qpid.Sasl;
using Apache.Qpid.Sasl.Mechanisms;

namespace Apache.Qpid.Sasl.Tests.Mechanisms
{
   [TestFixture]
   public class DigestSaslClientTests : ISaslCallbackHandler
   {
      private const string USERNAME = "chris";
      private const string PASSWORD = "secret";
      private const string AUTHID = null;
      private const string PROTOCOL = "IMAP";
      private const string SERVERNAME = "elwood.innosoft.com";

      #region Digest Challenge Parsing Tests
      //
      // Digest Challenge Parsing Tests
      //

      [Test]
      public void CanParseSimpleString()
      {
         string challenge = "realm=\"elwood.innosoft.com\", algorithm=md5-sess";
         StringDictionary values = DigestChallenge.ParseParameters(challenge);
         Assert.AreEqual(2, values.Count);
         Assert.AreEqual("elwood.innosoft.com", values["realm"]);
         Assert.AreEqual("md5-sess", values["algorithm"]);
      }

      [Test]
      public void CanParseEscapedQuotes()
      {
         string challenge = "realm=\"elwood\\\".innosoft.com\", algorithm=md5-sess";
         StringDictionary values = DigestChallenge.ParseParameters(challenge);
         Assert.AreEqual(2, values.Count);
         Assert.AreEqual("elwood\\\".innosoft.com", values["realm"]);
         Assert.AreEqual("md5-sess", values["algorithm"]);
      }

      [Test]
      public void CanParseEmbeddedDelimiter()
      {
         string challenge = "realm=\"elwood,innosoft.com\", algorithm=md5-sess";
         StringDictionary values = DigestChallenge.ParseParameters(challenge);
         Assert.AreEqual(2, values.Count);
         Assert.AreEqual("elwood,innosoft.com", values["realm"]);
         Assert.AreEqual("md5-sess", values["algorithm"]);
      }

      [Test]
      public void CanParse1()
      {
         string challenge = "realm=\"elwood.innosoft.com\",nonce=\"OA6MG9tEQGm2hh\",qop=\"auth\",algorithm=md5-sess,charset=utf-8";
         DigestChallenge parsed = DigestChallenge.Parse(challenge);

         Assert.AreEqual("elwood.innosoft.com", parsed.Realm);
         Assert.AreEqual("OA6MG9tEQGm2hh", parsed.Nonce);
         Assert.Contains("auth", parsed.QopOptions);
         Assert.AreEqual("md5-sess", parsed.Algorithm);
         Assert.AreEqual("utf-8", parsed.Charset);
      }

      #endregion // Digest Challenge Parsing Tests


      #region Digest Response Tests
      //
      // Digest Response Tests
      //

      [Test]
      public void CanWriteResponse()
      {
         DigestResponse resp = new DigestResponse();
         resp.Username = "user";
         resp.Realm = "nowhere.com";
         resp.Nonce = "OA9BSXrbuRhWay";
         resp.Cnonce = "OA9BSuZWMSpW8m";
         resp.NonceCount = 16;
         resp.DigestUri = "acap/elwood.innosoft.com";
         resp.Response = "6084c6db3fede7352c551284490fd0fc";
         resp.Qop = "auth";
         resp.MaxBuffer = 65536;
         resp.Cipher = "3des";
         resp.Authzid = "user2";
         resp.AuthParam = "ap";
         resp.Charset = "utf-8";

         string expected = "username=\"user\",realm=\"nowhere.com\",nonce=\"OA9BSXrbuRhWay\",cnonce=\"OA9BSuZWMSpW8m\",nc=00000010,qop=auth,digest-uri=\"acap/elwood.innosoft.com\",response=\"6084c6db3fede7352c551284490fd0fc\",maxbuf=65536,charset=utf-8,cipher=3des,authzid=\"user2\",auth-param=\"ap\"";
         Assert.AreEqual(expected, resp.ToString());
      }

      [Test]
      public void CanWriteEscapedSecuence()
      {
         DigestResponse resp = new DigestResponse();
         resp.Username = "us\"er";

         string expected = "username=\"us\\\"er\",nc=00000000,maxbuf=0";
         Assert.AreEqual(expected, resp.ToString());
      }

      #endregion // Digest Response Tests


      #region Authentication Tests
      //
      // Authentication Tests
      //

      [Test]
      public void ReturnsRightMechanismName()
      {
         ISaslClient client = CreateClient();

         Assert.AreEqual("DIGEST-MD5", client.MechanismName);
      }

      [Test]
      public void HasInitialResponseReturnsFalse()
      {
         ISaslClient client = CreateClient();

         Assert.IsFalse(client.HasInitialResponse);
      }

      [Test]
      public void CanAuthenticate()
      {
         string challenge = "realm=\"elwood.innosoft.com\",nonce=\"OA6MG9tEQGm2hh\",qop=\"auth\",algorithm=md5-sess,charset=utf-8";
         DigestSaslClient client = CreateClient();
         client.Cnonce = "OA6MHXh6VqTrRk";

         byte[] bresp = client.EvaluateChallenge(Encoding.UTF8.GetBytes(challenge));
         string response = Encoding.UTF8.GetString(bresp);
         string expectedResp = "username=\"chris\",realm=\"elwood.innosoft.com\",nonce=\"OA6MG9tEQGm2hh\",cnonce=\"" +
            client.Cnonce + "\",nc=00000001,qop=auth,digest-uri=\"imap/elwood.innosoft.com\",response=\"d388dad90d4bbd760a152321f2143af7\",maxbuf=65536,charset=utf-8";

         Assert.AreEqual(expectedResp, response);
         Assert.IsFalse(client.IsComplete);

         string challenge2 = "rspauth=ea40f60335c427b5527b84dbabcdfffd";
         bresp = client.EvaluateChallenge(Encoding.UTF8.GetBytes(challenge2));
         // client responds with zero-length array
         Assert.AreEqual(0, bresp.Length);
         Assert.IsTrue(client.IsComplete);
      }

      [Test]
      [ExpectedException(typeof(ArgumentNullException))]
      public void ThrowsExceptionWhenChallengeIsMissing()
      {
         DigestSaslClient client = CreateClient();
         client.EvaluateChallenge(null);
      }


      [Test]
      [ExpectedException(typeof(SaslException))]
      public void ThrowsExceptionWhenNonceMissing()
      {
         string challenge = "realm=\"elwood.innosoft.com\"";
         DigestSaslClient client = CreateClient();

         client.EvaluateChallenge(Encoding.UTF8.GetBytes(challenge));
      }

      [Test]
      [ExpectedException(typeof(SaslException))]
      public void ThrowsExceptionWhenAlgorithmMissing()
      {
         string challenge = "realm=\"elwood.innosoft.com\",nonce=\"asdasadsad\"";
         DigestSaslClient client = CreateClient();

         client.EvaluateChallenge(Encoding.UTF8.GetBytes(challenge));
      }

      [Test]
      [ExpectedException(typeof(SaslException))]
      public void ThrowsExceptionWhenSecondChallengeInvalid()
      {
         string challenge = "realm=\"elwood.innosoft.com\",nonce=\"OA6MG9tEQGm2hh\",qop=\"auth\",algorithm=md5-sess,charset=utf-8";
         DigestSaslClient client = CreateClient();

         byte[] bresp = client.EvaluateChallenge(Encoding.UTF8.GetBytes(challenge));
         Encoding.UTF8.GetString(bresp);

         // repeat challenge 1, which is incorrect
         client.EvaluateChallenge(Encoding.UTF8.GetBytes(challenge));
      }

      private DigestSaslClient CreateClient()
      {
         return new DigestSaslClient(
            AUTHID, SERVERNAME, PROTOCOL, 
            new Hashtable(), this
            );
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
            } else if ( cb is RealmCallback )
            {
               ((RealmCallback)cb).Text = SERVERNAME;
            }
         }
      }

      #endregion // Authentication Tests


   } // class DigestSaslClientTests

} // namespace Apache.Qpid.Sasl.Tests.Mechanisms
