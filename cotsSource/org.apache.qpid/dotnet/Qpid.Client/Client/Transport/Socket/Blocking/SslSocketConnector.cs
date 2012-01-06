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
using System.IO;
using System.Net;
using log4net;
using Apache.Qpid.Client.Qms;
using Org.Mentalis.Security.Ssl;
using MCertificate = Org.Mentalis.Security.Certificates.Certificate;
using MCertificateChain = Org.Mentalis.Security.Certificates.CertificateChain;

namespace Apache.Qpid.Client.Transport.Socket.Blocking
{
   /// <summary>
   /// Implements a TLS v1.0 connection using the Mentalis.org library
   /// </summary>
   /// <remarks>
   /// It would've been easier to implement this at the StreamFilter
   /// level, but unfortunately the Mentalis library doesn't support
   /// a passthrough SSL stream class and is tied directly
   /// to socket-like classes.
   /// </remarks>
   class SslSocketConnector : ISocketConnector
   {
      private static ILog _logger = LogManager.GetLogger(typeof(SslSocketConnector));
      private MyTcpClient _tcpClient;

      public string LocalEndpoint
      {
         get { return _tcpClient.LocalEndpoint.ToString(); }
      }

      public Stream Connect(IBrokerInfo broker)
      {
         MCertificate cert = GetClientCert(broker);
         SecurityOptions options = new SecurityOptions(
            SecureProtocol.Tls1, cert, ConnectionEnd.Client
            );
         if ( broker.SslOptions != null 
            && broker.SslOptions.IgnoreValidationErrors )
         {
            _logger.Warn("Ignoring any certificate validation errors during SSL handshake...");
            options.VerificationType = CredentialVerification.None;
         }

         _tcpClient = new MyTcpClient(broker.Host, broker.Port, options);
         return _tcpClient.GetStream();
      }

      public void Dispose()
      {
         if ( _tcpClient != null )
         {
            _tcpClient.Close();
            _tcpClient = null;
         }
      }

      private static MCertificate GetClientCert(IBrokerInfo broker)
      {
         // if a client certificate is configured, 
         // use that to enable mutual authentication
         MCertificate cert = null;
         if ( broker.SslOptions != null 
            && broker.SslOptions.ClientCertificate != null )
         {
            cert = MCertificate.CreateFromX509Certificate(
               broker.SslOptions.ClientCertificate
               );
            _logger.DebugFormat("Using Client Certificate for SSL '{0}'", cert.ToString(true));
         }
         return cert;
      }

      class MyTcpClient : SecureTcpClient
      {
         public MyTcpClient(string host, int port, SecurityOptions options)
            : base(host, port, options)
         {
         }

         public EndPoint LocalEndpoint
         {
            get { return Client.LocalEndPoint; }
         }

      }

   }
}
