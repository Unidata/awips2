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
using System.Globalization;
using System.Text;

namespace Apache.Qpid.Sasl
{
   public abstract class SaslClient : ISaslClient
   {
      private bool _isComplete;
      private IDictionary _properties;
      private string _authorizationId;
      private string _serverName;
      private string _protocol;
      private ISaslCallbackHandler _handler;

      protected string AuthorizationId
      {
         get { return _authorizationId; }
      }
      protected string ServerName
      {
         get { return _serverName; }
      }

      protected string Protocol
      {
         get { return _protocol; }
      }

      protected ISaslCallbackHandler Handler
      {
         get { return _handler; }
      }

      protected IDictionary Properties
      {
         get { return _properties; }
      }

      protected SaslClient(
         string authid, string serverName, 
         string protocol, IDictionary properties, 
         ISaslCallbackHandler handler)
      {
         if ( properties == null )
            throw new ArgumentNullException("properties");
         if ( handler == null )
            throw new ArgumentNullException("handler");

         _authorizationId = authid==null ? "" : authid;
         _serverName = serverName;
         _protocol = protocol;
         _properties = properties;
         _handler = handler;

         if ( _serverName == null || _serverName.Length == 0 )
         {
            _serverName = System.Net.Dns.GetHostName();
         }
      }




      #region ISaslClient Implementation
      //
      // ISaslClient Implementation
      //

      public abstract string MechanismName { get; }

      public abstract bool HasInitialResponse { get; }

      public bool IsComplete
      {
         get { return _isComplete; }
      }

      public abstract byte[] EvaluateChallenge(byte[] challenge);

      public virtual object GetNegotiatedProperty(string propName)
      {
         return null;
      }

      public virtual byte[] Unwrap(byte[] buffer, int offset, int length)
      {
         throw new NotImplementedException();
      }

      public virtual byte[] Wrap(byte[] buffer, int offset, int lenght)
      {
         throw new NotImplementedException();
      }

      #endregion // ISaslClient Implementation


      #region Helper Methods
      //
      // Helper Methods
      //

      protected void SetComplete()
      {
         _isComplete = true;
      }

      protected static string ToHex(byte[] buffer)
      {
         StringBuilder builder = new StringBuilder();
         foreach ( byte b in buffer )
         {
            builder.Append(b.ToString("x2", CultureInfo.InvariantCulture));
         }
         return builder.ToString();
      }

      #endregion // Helper Methods

   } // class SaslClient

} // namespace Apache.Qpid.Sasl.Mechanisms
