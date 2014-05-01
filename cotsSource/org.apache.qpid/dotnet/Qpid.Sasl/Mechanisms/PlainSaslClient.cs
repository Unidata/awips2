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

namespace Apache.Qpid.Sasl.Mechanisms
{

   /// <summary>
   /// Implements the PLAIN authentication mechanism
   /// as outlined in RFC 4616
   /// </summary>
   public class PlainSaslClient : SaslClient
   {
      public const string Mechanism = "PLAIN";

      public PlainSaslClient(
         string authid, IDictionary properties, 
         ISaslCallbackHandler handler)
         : base(authid, null, null, properties, handler)
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
         get { return true; }
      }

      public override byte[] EvaluateChallenge(byte[] challenge)
      {
         // ignore challenge

         NameCallback nameCB = new NameCallback();
         PasswordCallback pwdCB = new PasswordCallback();
         ISaslCallback[] callbacks = { nameCB, pwdCB };
         Handler.Handle(callbacks);

         string username = nameCB.Text;
         string authid = AuthorizationId;
         string passwd = pwdCB.Text;

         string response = 
            string.Format("{0}\0{1}\0{2}", authid, username, passwd);
         SetComplete();
         return Encoding.UTF8.GetBytes(response);
      }

      #endregion // ISaslClient Implementation

   } // class PlainSaslClient
} // namespace Apache.Qpid.Sasl.Mechanisms
