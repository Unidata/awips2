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
using System.Net.Sockets;
using Apache.Qpid.Client.Qms;

namespace Apache.Qpid.Client.Transport.Socket.Blocking
{
   /// <summary>
   /// Implements a TCP connection over regular sockets.
   /// </summary>
   class SocketConnector : ISocketConnector
   {
      private MyTcpClient _tcpClient;

      public string LocalEndpoint
      {
         get { return _tcpClient.LocalEndpoint.ToString(); }
      }

      public Stream Connect(IBrokerInfo broker)
      {
         _tcpClient = new MyTcpClient(broker.Host, broker.Port);
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

      class MyTcpClient : TcpClient
      {
         public MyTcpClient(string host, int port)
            : base(host, port)
         {
         }

         public EndPoint LocalEndpoint
         {
            get { return Client.LocalEndPoint; }
         }
      }

   }
}


