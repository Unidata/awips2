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
using System.Security.Cryptography;
using System.Text;

namespace Apache.Qpid.Sasl.Mechanisms
{
   /// <summary>
   /// Implements the CRAM-MD5 authentication mechanism as outlined
   /// in RFC 2195
   /// </summary>
   public class CramMD5SaslClient : SaslClient
   {
      public const string Mechanism = "CRAM-MD5";
      private const int MinPwdLen = 16;

      public CramMD5SaslClient(
         string authorizationId, 
         IDictionary properties, 
         ISaslCallbackHandler handler)
         : base(authorizationId, null, null, properties, handler)
      {
      }

      #region ISaslClient Implementation
      //
      // ISaslClient Implementation
      //

      public override string MechanismName
      {
         get { return Mechanism; }
      }

      public override bool HasInitialResponse
      {
         get { return false; }
      }

      public override byte[] EvaluateChallenge(byte[] challenge)
      {
         if ( challenge == null || challenge.Length == 0 )
            throw new ArgumentNullException("challenge");

         NameCallback nameCB = new NameCallback(AuthorizationId);
         PasswordCallback pwdCB = new PasswordCallback();
         ISaslCallback[] callbacks = { nameCB, pwdCB };
         Handler.Handle(callbacks);

         string username = nameCB.Text;
         string passwd = pwdCB.Text.PadRight(MinPwdLen, '\0');

         byte[] secret = Encoding.UTF8.GetBytes(passwd);

         //using ( HMAC hmac = new HMACMD5(secret) )
         using ( MD5HMAC hmac = new MD5HMAC(secret) )
         {
            byte[] value = hmac.ComputeHash(challenge);
            string encoded = ToHex(value);
            SetComplete();
            return Encoding.UTF8.GetBytes(username + " " + encoded);
         }

      }

      #endregion // ISaslClient Implementation

   } // class CramMD5SaslClient

} // namespace Apache.Qpid.Sasl.Mechanisms
