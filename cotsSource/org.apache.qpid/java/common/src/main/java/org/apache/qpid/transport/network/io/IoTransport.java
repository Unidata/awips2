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

package org.apache.qpid.transport.network.io;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;
import java.nio.ByteBuffer;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;

import org.apache.qpid.protocol.AMQVersionAwareProtocolSession;
import org.apache.qpid.ssl.SSLContextFactory;
import org.apache.qpid.transport.Binding;
import org.apache.qpid.transport.Connection;
import org.apache.qpid.transport.ConnectionDelegate;
import org.apache.qpid.transport.Receiver;
import org.apache.qpid.transport.Sender;
import org.apache.qpid.transport.TransportException;
import org.apache.qpid.transport.network.ConnectionBinding;
import org.apache.qpid.transport.network.ssl.SSLReceiver;
import org.apache.qpid.transport.network.ssl.SSLSender;
import org.apache.qpid.transport.util.Logger;

/**
 * This class provides a socket based transport using the java.io
 * classes.
 *
 * The following params are configurable via JVM arguments
 * TCP_NO_DELAY - amqj.tcpNoDelay
 * SO_RCVBUF    - amqj.receiveBufferSize
 * SO_SNDBUF    - amqj.sendBufferSize
 */
public final class IoTransport<E>
{

    static
    {
        org.apache.mina.common.ByteBuffer.setAllocator
            (new org.apache.mina.common.SimpleByteBufferAllocator());
        org.apache.mina.common.ByteBuffer.setUseDirectBuffers
            (Boolean.getBoolean("amqj.enableDirectBuffers"));
    }

    private static final Logger log = Logger.get(IoTransport.class);

    private static int DEFAULT_READ_WRITE_BUFFER_SIZE = 64 * 1024;
    private static int readBufferSize = Integer.getInteger
        ("amqj.receiveBufferSize", DEFAULT_READ_WRITE_BUFFER_SIZE);
    private static int writeBufferSize = Integer.getInteger
        ("amqj.sendBufferSize", DEFAULT_READ_WRITE_BUFFER_SIZE);

    private Socket socket;
    private Sender<ByteBuffer> sender;
    private E endpoint;
    private IoReceiver receiver;
    private long timeout = 60000;

    IoTransport(Socket socket, Binding<E,ByteBuffer> binding, boolean ssl)
    {
        this.socket = socket;
        
        if (ssl)
        {
            SSLEngine engine = null;
            SSLContext sslCtx;
            try
            {
                sslCtx = createSSLContext();
            }
            catch (Exception e)
            {
                throw new TransportException("Error creating SSL Context", e);
            }
            
            try
            {
                engine = sslCtx.createSSLEngine();
                engine.setUseClientMode(true);
            }
            catch(Exception e)
            {
                throw new TransportException("Error creating SSL Engine", e);
            }
            
            this.sender = new SSLSender(engine,new IoSender(this, 2*writeBufferSize, timeout));
            this.endpoint = binding.endpoint(sender);
            this.receiver = new IoReceiver(this, new SSLReceiver(engine,binding.receiver(endpoint),(SSLSender)sender),
                                           2*readBufferSize, timeout);
        }
        else
        {
            this.sender = new IoSender(this, 2*writeBufferSize, timeout);
            this.endpoint = binding.endpoint(sender);
            this.receiver = new IoReceiver(this, binding.receiver(endpoint),
                                           2*readBufferSize, timeout);
        }
    }

    Sender<ByteBuffer> getSender()
    {
        return sender;
    }

    IoReceiver getReceiver()
    {
        return receiver;
    }

    Socket getSocket()
    {
        return socket;
    }

    public static final <E> E connect(String host, int port,
                                      Binding<E,ByteBuffer> binding,
                                      boolean ssl)
    {
        Socket socket = createSocket(host, port);
        IoTransport<E> transport = new IoTransport<E>(socket, binding,ssl);
        return transport.endpoint;
    }

    public static final Connection connect(String host, int port,
                                           ConnectionDelegate delegate,
                                           boolean ssl)
    {
        return connect(host, port, ConnectionBinding.get(delegate),ssl);
    }

    public static void connect_0_9(AMQVersionAwareProtocolSession session, String host, int port, boolean ssl)
    {
        connect(host, port, new Binding_0_9(session),ssl);
    }

    private static class Binding_0_9
        implements Binding<AMQVersionAwareProtocolSession,ByteBuffer>
    {

        private AMQVersionAwareProtocolSession session;

        Binding_0_9(AMQVersionAwareProtocolSession session)
        {
            this.session = session;
        }

        public AMQVersionAwareProtocolSession endpoint(Sender<ByteBuffer> sender)
        {
            session.setSender(sender);
            return session;
        }

        public Receiver<ByteBuffer> receiver(AMQVersionAwareProtocolSession ssn)
        {
            return new InputHandler_0_9(ssn);
        }

    }

    private static Socket createSocket(String host, int port)
    {
        try
        {
            InetAddress address = InetAddress.getByName(host);
            Socket socket = new Socket();
            socket.setReuseAddress(true);
            socket.setTcpNoDelay(Boolean.getBoolean("amqj.tcpNoDelay"));

            log.debug("default-SO_RCVBUF : %s", socket.getReceiveBufferSize());
            log.debug("default-SO_SNDBUF : %s", socket.getSendBufferSize());

            socket.setSendBufferSize(writeBufferSize);
            socket.setReceiveBufferSize(readBufferSize);

            log.debug("new-SO_RCVBUF : %s", socket.getReceiveBufferSize());
            log.debug("new-SO_SNDBUF : %s", socket.getSendBufferSize());

            socket.connect(new InetSocketAddress(address, port));
            return socket;
        }
        catch (SocketException e)
        {
            throw new TransportException("Error connecting to broker", e);
        }
        catch (IOException e)
        {
            throw new TransportException("Error connecting to broker", e);
        }
    }
    
    private SSLContext createSSLContext() throws Exception
    {
        String trustStorePath = System.getProperty("javax.net.ssl.trustStore");
        String trustStorePassword = System.getProperty("javax.net.ssl.trustStorePassword");
        String trustStoreCertType = System.getProperty("qpid.ssl.trustStoreCertType","SunX509");
                
        String keyStorePath = System.getProperty("javax.net.ssl.keyStore",trustStorePath);
        String keyStorePassword = System.getProperty("javax.net.ssl.keyStorePassword",trustStorePassword);
        String keyStoreCertType = System.getProperty("qpid.ssl.keyStoreCertType","SunX509");
        
        SSLContextFactory sslContextFactory = new SSLContextFactory(trustStorePath,trustStorePassword,
                                                                    trustStoreCertType,keyStorePath,
                                                                    keyStorePassword,keyStoreCertType);
        
        return sslContextFactory.buildServerContext();
        
    }

}
