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
   public class TestClientFactory : ISaslClientFactory
   {
      public string[] GetSupportedMechanisms(IDictionary props)
      {
         return new string[] { TestSaslClient.Mechanism };
      }

      public ISaslClient CreateClient(string[] mechanisms, string authorizationId, string protocol, string serverName, IDictionary props, ISaslCallbackHandler handler)
      {
         foreach ( string mech in mechanisms )
         {
            if ( mech == TestSaslClient.Mechanism )
               return new TestSaslClient(props, handler);
         }
         return null;
      }

   } // class TestClientFactory

   internal class TestSaslClient : SaslClient
   {
      public const string Mechanism = "TEST";

      public override string MechanismName
      {
         get { return Mechanism; }
      }
      public override bool HasInitialResponse
      {
         get { return false; }
      }

      public TestSaslClient(IDictionary props, ISaslCallbackHandler handler)
         : base("", "", "", props, handler)
      {
      }

      public override byte[] EvaluateChallenge(byte[] challenge)
      {
         throw new NotImplementedException();
      }
   } // class TestSaslClient

} // namespace Apache.Qpid.Sasl.Tests
