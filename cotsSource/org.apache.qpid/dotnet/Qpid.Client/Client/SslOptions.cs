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
using System.Security.Cryptography.X509Certificates;

namespace Apache.Qpid.Client
{
   /// <summary>
   /// Configures SSL-related options to connect to an AMQP broker.
   /// </summary>
   /// <remarks>
   /// If the server certificate is not trusted by the client,
   /// connection will fail. However, you can set the 
   /// <see cref="IgnoreValidationErrors"/> property to true
   /// to ignore any certificate verification errors for debugging purposes.
   /// </remarks>
   public class SslOptions
   {
      private X509Certificate _clientCertificate;
      private bool _ignoreValidationErrors;

      /// <summary>
      /// Certificate to present to the broker to authenticate
      /// this client connection
      /// </summary>
      public X509Certificate ClientCertificate
      {
         get { return _clientCertificate; }
      }

      /// <summary>
      /// If true, the validity of the broker certificate 
      /// will not be verified on connection
      /// </summary>
      public bool IgnoreValidationErrors
      {
         get { return _ignoreValidationErrors; }
      }

      /// <summary>
      /// Initialize a new instance with default values 
      /// (No client certificate, don't ignore validation errors)
      /// </summary>
      public SslOptions()
      {
      }

      /// <summary>
      /// Initialize a new instance
      /// </summary>
      /// <param name="clientCertificate">
      /// Certificate to use to authenticate the client to the broker
      /// </param>
      /// <param name="ignoreValidationErrors">
      /// If true, ignore any validation errors when validating the server certificate
      /// </param>
      public SslOptions(X509Certificate clientCertificate, bool ignoreValidationErrors)
      {
         _clientCertificate = clientCertificate;
         _ignoreValidationErrors = ignoreValidationErrors;
      }
   }
}
