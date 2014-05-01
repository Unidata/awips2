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
using System.Net;
using System.Net.Sockets;
using log4net;
using Apache.Qpid.Buffer;
using Apache.Qpid.Client.Protocol;

namespace Apache.Qpid.Client.Transport.Socket.Blocking
{
    class BlockingSocketProcessor : IConnectionCloser
    {
        private static readonly ILog _log = LogManager.GetLogger(typeof(BlockingSocketProcessor));

        string _host;
        int _port;
        System.Net.Sockets.Socket _socket;
        private NetworkStream _networkStream;
        IByteChannel _byteChannel;
        IProtocolListener _protocolListener;

        public BlockingSocketProcessor(string host, int port, IProtocolListener protocolListener)
        {
            _host = host;
            _port = port;
            _protocolListener = protocolListener;
            _byteChannel = new ByteChannel(this);
        }

        /// <summary>
        ///  Synchronous blocking connect.
        /// </summary>
        public void Connect()
        {
            _socket = new System.Net.Sockets.Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);

	    /// For future note TCP Set NoDelay options may help, though with the blocking io not sure
	    /// The Don't linger may help with detecting disconnect but that hasn't been the case in testing.
	    /// _socket.SetSocketOption (SocketOptionLevel.Socket, SocketOptionName.NoDelay, 0);
	    /// _socket.SetSocketOption (SocketOptionLevel.Socket, SocketOptionName.DontLinger, 0);
	    
            IPHostEntry ipHostInfo = Dns.Resolve(_host); // Note: don't fix this warning. We do this for .NET 1.1 compatibility.
            IPAddress ipAddress = ipHostInfo.AddressList[0];

            IPEndPoint ipe = new IPEndPoint(ipAddress, _port);

            _socket.Connect(ipe);            
            _networkStream = new NetworkStream(_socket, true);
        }
        
        public string getLocalEndPoint()
        {
            return _socket.LocalEndPoint.ToString();
        }

        public void Write(ByteBuffer byteBuffer)
        {            
            try
            {
                _networkStream.Write(byteBuffer.array(), byteBuffer.position(), byteBuffer.limit()); // FIXME
            }
            catch (Exception e)
            {
                _log.Error("Write caused exception", e);
                _protocolListener.OnException(e);
                // We should provide the error synchronously as we are doing blocking io.
                throw e;
            }
        }

        public ByteBuffer Read()
        {
            const int bufferSize = 4 * 1024; // TODO: Prevent constant allocation of buffers.
            byte[] bytes = new byte[bufferSize];
            
            int numOctets = _networkStream.Read(bytes, 0, bytes.Length);

	    /// Read only returns 0 if the socket has been gracefully shutdown.
	    /// http://msdn2.microsoft.com/en-us/library/system.net.sockets.networkstream.read(VS.71).aspx            
	    /// We can use this to block Send so the next Read will force an exception forcing failover.
	    /// Otherwise we need to wait ~20 seconds for the NetworkStream/Socket code to realise that
	    /// the socket has been closed.
	    if (numOctets == 0)
	    {              
		_socket.Shutdown(SocketShutdown.Send);
		_socket.Close();
	    }
             
            ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
            byteBuffer.limit(numOctets);
            
            byteBuffer.flip();

            return byteBuffer;
        }

        public void Disconnect()
        {
            _networkStream.Flush();
            _networkStream.Close();
            _socket.Close();
        }

        public void Close()
        {
            Disconnect();
        }

        public IByteChannel ByteChannel
        {
            get { return _byteChannel; }
        }
    }
}


