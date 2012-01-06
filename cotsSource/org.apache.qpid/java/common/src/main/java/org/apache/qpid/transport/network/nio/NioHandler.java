package org.apache.qpid.transport.network.nio;
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


import java.io.EOFException;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.qpid.transport.Connection;
import org.apache.qpid.transport.ConnectionDelegate;
import org.apache.qpid.transport.Receiver;
import org.apache.qpid.transport.network.Assembler;
import org.apache.qpid.transport.network.Disassembler;
import org.apache.qpid.transport.network.InputHandler;

public class NioHandler implements Runnable
{
    private Receiver<ByteBuffer> _receiver;
    private SocketChannel _ch;
    private ByteBuffer _readBuf;
    private static Map<Integer,NioSender> _handlers = new ConcurrentHashMap<Integer,NioSender>();
    private AtomicInteger _count = new AtomicInteger();

    private NioHandler(){}

    public static final Connection connect(String host, int port,
            ConnectionDelegate delegate)
    {
        NioHandler handler = new NioHandler();
        return handler.connectInternal(host,port,delegate);
    }

    private Connection connectInternal(String host, int port,
            ConnectionDelegate delegate)
    {
        try
        {
            SocketAddress address = new InetSocketAddress(host,port);
            _ch = SocketChannel.open();
            _ch.socket().setReuseAddress(true);
            _ch.configureBlocking(true);
            _ch.socket().setTcpNoDelay(true);
            if (address != null)
            {
                _ch.socket().connect(address);
            }
            while (_ch.isConnectionPending())
            {

            }

        }
        catch (SocketException e)
        {

            e.printStackTrace();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }

        NioSender sender = new NioSender(_ch);
        Connection con = new Connection();
        con.setSender(new Disassembler(sender, 64*1024 - 1));
        con.setConnectionDelegate(delegate);

        con.setConnectionId(_count.incrementAndGet());
        _handlers.put(con.getConnectionId(),sender);

        _receiver = new InputHandler(new Assembler(con), InputHandler.State.FRAME_HDR);

        Thread t = new Thread(this);
        t.start();

        return con;
    }

    public void run()
    {
        _readBuf = ByteBuffer.allocate(512);
        long read = 0;
        while(_ch.isConnected() && _ch.isOpen())
        {
            try
            {
                read = _ch.read(_readBuf);
                if (read > 0)
                {
                    _readBuf.flip();
                    ByteBuffer b = ByteBuffer.allocate(_readBuf.remaining());
                    b.put(_readBuf);
                    b.flip();
                    _readBuf.clear();
                    _receiver.received(b);
                }
            }
            catch(Exception e)
            {
                e.printStackTrace();
            }
        }

        //throw new EOFException("The underlying socket/channel has closed");
    }

    public static void startBatchingFrames(int connectionId)
    {
        NioSender sender = _handlers.get(connectionId);
        sender.setStartBatching();
    }


}
