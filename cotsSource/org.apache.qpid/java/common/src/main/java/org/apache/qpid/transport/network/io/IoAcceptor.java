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
package org.apache.qpid.transport.network.io;

import org.apache.qpid.transport.Binding;
import org.apache.qpid.transport.TransportException;

import java.io.IOException;

import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;

import java.nio.ByteBuffer;


/**
 * IoAcceptor
 *
 */

public class IoAcceptor<E> extends Thread
{


    private ServerSocket socket;
    private Binding<E,ByteBuffer> binding;

    public IoAcceptor(SocketAddress address, Binding<E,ByteBuffer> binding)
        throws IOException
    {
        socket = new ServerSocket();
        socket.setReuseAddress(true);
        socket.bind(address);
        this.binding = binding;

        setName(String.format("IoAcceptor - %s", socket.getInetAddress()));
    }

    /**
        Close the underlying ServerSocket if it has not already been closed.
     */
    public void close() throws IOException
    {
        if (!socket.isClosed())
        {
            socket.close();
        }
    }

    public IoAcceptor(String host, int port, Binding<E,ByteBuffer> binding)
        throws IOException
    {
        this(new InetSocketAddress(host, port), binding);
    }

    public void run()
    {
        while (true)
        {
            try
            {
                Socket sock = socket.accept();
                IoTransport<E> transport = new IoTransport<E>(sock, binding,false);
            }
            catch (IOException e)
            {
                throw new TransportException(e);
            }
        }
    }

}
