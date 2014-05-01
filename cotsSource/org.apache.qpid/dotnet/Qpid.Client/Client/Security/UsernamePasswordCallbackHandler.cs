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
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Sasl;

namespace Apache.Qpid.Client.Security
{
   internal class UsernamePasswordCallbackHandler : IAMQCallbackHandler
   {
      private AMQProtocolSession _session;

      public void Initialize(AMQProtocolSession session)
      {
         if ( session == null )
            throw new ArgumentNullException("session");

         _session = session;
      }

      public void Handle(ISaslCallback[] callbacks)
      {
         foreach ( ISaslCallback cb in callbacks )
         {
            if ( cb is NameCallback )
            {
               ((NameCallback)cb).Text = _session.Username;
            } else if ( cb is PasswordCallback )
            {
               ((PasswordCallback)cb).Text = _session.Password;
            }
         }
      }
   }
}
