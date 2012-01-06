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
package org.apache.qpid.codec;

import org.apache.mina.transport.socket.nio.SocketConnector;
import org.apache.mina.common.ConnectFuture;
import org.apache.mina.common.IoHandlerAdapter;
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolCodecFilter;
import org.apache.qpid.framing.AMQDataBlock;
import org.apache.qpid.framing.AMQFrame;
import org.apache.qpid.framing.BasicDeliverBody;
import org.apache.qpid.framing.ContentBody;

import java.net.InetSocketAddress;

public class Client extends IoHandlerAdapter
{
    //private static final int[] DEFAULT_SIZES = new int[]{1024, 512, 256, 128, 56};
    //private static final int[] DEFAULT_SIZES = new int[]{256, 256, 256, 256, 256, 512, 512, 512, 512, 512};
    private static final int[] DEFAULT_SIZES = new int[]{256, 512, 256, 512, 256, 512, 256, 512, 256, 512};
    //private static final int[] DEFAULT_SIZES = new int[]{1024, 1024, 1024, 1024, 1024};

    private final IoSession _session;
    private final long _start;
    private final int _size;
    private final int _count;
    private int _received;
    private boolean _closed;

    Client(String host, int port, int size, int count) throws Exception
    {
        _count = count;
        _size = size;
        AMQDataBlock block = BasicDeliverTest.getDataBlock(size);

        InetSocketAddress address = new InetSocketAddress(host, port);
        ConnectFuture future = new SocketConnector().connect(address, this);
        future.join();
        _session = future.getSession();

        _start = System.currentTimeMillis();
        for(int i = 0; i < count; i++)
        {
            _session.write(block);
        }
    }

    void close()
    {
        long time = System.currentTimeMillis() - _start;
        System.out.println("Received " + _received + " messages of " + _size
                                       + " bytes in " + time + "ms.");
        _session.close();
        synchronized(this)
        {
            _closed = true;
            notify();
        }
    }

    void waitForClose() throws InterruptedException
    {
        synchronized(this)
        {
            while(!_closed)
            {
                wait();
            }
        }
    }

    public void sessionCreated(IoSession session) throws Exception
    {
        session.getFilterChain().addLast("protocolFilter", new ProtocolCodecFilter(new AMQCodecFactory(false)));
    }

    public void messageReceived(IoSession session, Object object) throws Exception
    {
        if(isContent(object) && ++_received == _count) close();
    }

    public void exceptionCaught(IoSession session, Throwable throwable) throws Exception
    {
        throwable.printStackTrace();
        close();
    }

    private static boolean isDeliver(Object o)
    {
        return o instanceof AMQFrame && ((AMQFrame) o).getBodyFrame() instanceof BasicDeliverBody;
    }

    private static boolean isContent(Object o)
    {
        return o instanceof AMQFrame && ((AMQFrame) o).getBodyFrame() instanceof ContentBody;
    }

    public static void main(String[] argv) throws Exception
    {
        String host = argv.length > 0 ? argv[0] : "localhost";
        int port = argv.length > 1 ? Integer.parseInt(argv[1]) : 8888;
        int count = argv.length > 2 ? Integer.parseInt(argv[2]) : 10000;
        int[] sizes = argv.length > 3 ? new int[]{Integer.parseInt(argv[3])} : DEFAULT_SIZES;

        System.out.println("Connecting to " + host + ":" + port);

        for(int i = 0; i < sizes.length; i++)
        {
            new Client(host, port, sizes[i], count).waitForClose();
            Thread.sleep(1000);
        }
    }

}
