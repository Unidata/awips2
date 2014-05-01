/*
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
*/
using System;
using System.IO;
using System.Net.Security;
using System.Net.Sockets;
using System.Security.Authentication;
using System.Security.Cryptography.X509Certificates;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport.network.io
{
    public sealed class IoSSLTransport : IIoTransport
    {
        // constants 
        private const int DEFAULT_READ_WRITE_BUFFER_SIZE = 64*1024;
        private const int TIMEOUT = 60000;
        private const int QUEUE_SIZE = 1000;
        // props
        private static readonly Logger log = Logger.Get(typeof (IoSSLTransport));
        private Stream m_stream;
        private IoSender m_sender;
        private IReceiver<ReceivedPayload<MemoryStream>> m_receiver;
        private TcpClient m_socket;
        private Connection m_con;
        private readonly bool _rejectUntrusted;

        public static Connection Connect(String host, int port, string serverName, string certPath, bool rejectUntrusted,  ConnectionDelegate conndel)
        {            
            IIoTransport transport = new IoSSLTransport(host, port, serverName, certPath, rejectUntrusted, conndel);
            return transport.Connection;
        }

        public IoSSLTransport(String host, int port, string serverName, string certPath, bool rejectUntrusted, ConnectionDelegate conndel)
        {
            _rejectUntrusted = rejectUntrusted;
            CreateSocket(host, port, serverName, certPath);
            Sender = new IoSender(this, QUEUE_SIZE, TIMEOUT);
            Receiver = new IoReceiver(Stream, Socket.ReceiveBufferSize*2, TIMEOUT);
            Assembler assembler = new Assembler();
            InputHandler inputHandler = new InputHandler(InputHandler.State.PROTO_HDR);
            Connection = new Connection(assembler, new Disassembler(Sender, 64*1024 - 1), conndel);
            // Input handler listen to Receiver events
            Receiver.Received += inputHandler.On_ReceivedBuffer;
            // Assembler listen to inputhandler events       
            inputHandler.ReceivedEvent += assembler.On_ReceivedEvent;
            // Connection listen to asembler protocol event 
            Receiver.Closed += Connection.On_ReceivedClosed;
            assembler.Closed += Connection.On_ReceivedClosed;
            Receiver.Exception += Connection.On_ReceivedException;
            inputHandler.ExceptionProcessing += Connection.On_ReceivedException;
            assembler.ReceivedEvent += Connection.On_ReceivedEvent;
        }

        public Connection Connection
        {
            get { return m_con; }
            set { m_con = value; }
        }

        public IReceiver<ReceivedPayload<MemoryStream>> Receiver
        {
            get { return m_receiver; }
            set { m_receiver = value; }
        }

        public IoSender Sender
        {
            get { return m_sender; }
            set { m_sender = value; }
        }


        public Stream Stream
        {
            get { return m_stream; }
            set { m_stream = value; }
        }

        public TcpClient Socket
        {
            get { return m_socket; }
            set { m_socket = value; }
        }

        #region Private Support Functions

        private void CreateSocket(String host, int port, string serverName, string certPath)
        {
            TcpClient socket;
            try
            {
                socket = new TcpClient();
                String noDelay = Environment.GetEnvironmentVariable("qpid.tcpNoDelay");
                String writeBufferSize = Environment.GetEnvironmentVariable("qpid.writeBufferSize");
                String readBufferSize = Environment.GetEnvironmentVariable("qpid.readBufferSize");
                socket.NoDelay = noDelay != null && bool.Parse(noDelay);
                socket.ReceiveBufferSize = readBufferSize == null
                                               ? DEFAULT_READ_WRITE_BUFFER_SIZE
                                               : int.Parse(readBufferSize);
                socket.SendBufferSize = writeBufferSize == null
                                            ? DEFAULT_READ_WRITE_BUFFER_SIZE
                                            : int.Parse(writeBufferSize);

                log.Debug("NoDelay : {0}", socket.NoDelay);
                log.Debug("ReceiveBufferSize : {0}", socket.ReceiveBufferSize);
                log.Debug("SendBufferSize : {0}", socket.SendBufferSize);
                log.Debug("Openning connection with host : {0}; port: {1}", host, port);

                socket.Connect(host, port);
                Socket = socket;
            }
            catch (Exception e)
            {
                throw new TransportException("Error connecting to broker", e);
            }
            try
            { 
                //Initializes a new instance of the SslStream class using the specified Stream, stream closure behavior, certificate validation delegate and certificate selection delegate
                SslStream sslStream = new SslStream(socket.GetStream(), false, ValidateServerCertificate, LocalCertificateSelection);                   
                if (certPath != null)
                {
                    X509CertificateCollection col = new X509CertificateCollection();
                    X509Certificate cert = X509Certificate.CreateFromCertFile(certPath);
                    col.Add(cert);
                    sslStream.AuthenticateAsClient(serverName, col, SslProtocols.Default, true);
                }
                else
                {
                    sslStream.AuthenticateAsClient(serverName);
                }
                Stream = sslStream;
            }
            catch (AuthenticationException e)
            {
                log.Warn("Exception: {0}", e.Message);
                if (e.InnerException != null)
                {
                    log.Warn("Inner exception: {0}", e.InnerException.Message);
                }
                socket.Close();
                throw new TransportException("Authentication failed - closing the connection.");
            }
        }

        // The following method is invoked by the RemoteCertificateValidationDelegate.
        public bool ValidateServerCertificate(
            object sender,
            X509Certificate certificate,
            X509Chain chain,
            SslPolicyErrors sslPolicyErrors)
        {
            bool result = true;
            if (sslPolicyErrors != SslPolicyErrors.None &&  _rejectUntrusted )
            {
                log.Warn("Certificate error: {0}", sslPolicyErrors);
                // Do not allow this client to communicate with unauthenticated servers.
                result =  false;  
            }
            return result;
        }

        public  X509Certificate LocalCertificateSelection(
            Object sender,
            string targetHost,
            X509CertificateCollection localCertificates,
            X509Certificate remoteCertificate,
            string[] acceptableIssuers
            )
        {
            return remoteCertificate;
        }

        #endregion
    }
}
