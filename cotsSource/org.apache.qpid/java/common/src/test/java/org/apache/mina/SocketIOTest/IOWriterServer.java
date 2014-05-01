/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.apache.mina.SocketIOTest;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.IoAcceptor;
import org.apache.mina.common.IoFilterChain;
import org.apache.mina.common.IoHandlerAdapter;
import org.apache.mina.common.IoSession;
import org.apache.mina.common.SimpleByteBufferAllocator;
import org.apache.mina.filter.ReadThrottleFilterBuilder;
import org.apache.mina.filter.WriteBufferLimitFilterBuilder;
import org.apache.mina.transport.socket.nio.SocketSessionConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.InetSocketAddress;

/** Tests MINA socket performance. This acceptor simply reads data from the network and writes it back again. */
public class IOWriterServer
{
    private static final Logger _logger = LoggerFactory.getLogger(IOWriterServer.class);

    static public int _PORT = 9999;

    private static final String DEFAULT_READ_BUFFER = "262144";
    private static final String DEFAULT_WRITE_BUFFER = "262144";


    private static class TestHandler extends IoHandlerAdapter
    {
        private int _sentCount = 0;

        private int _bytesSent = 0;

        private int _receivedCount = 0;

        public void sessionCreated(IoSession ioSession) throws java.lang.Exception
        {
            IoFilterChain chain = ioSession.getFilterChain();

            ReadThrottleFilterBuilder readfilter = new ReadThrottleFilterBuilder();
            readfilter.setMaximumConnectionBufferSize(Integer.parseInt(System.getProperty("qpid.read.buffer.limit", DEFAULT_READ_BUFFER)));
            readfilter.attach(chain);

            WriteBufferLimitFilterBuilder writefilter = new WriteBufferLimitFilterBuilder();

            writefilter.setMaximumConnectionBufferSize(Integer.parseInt(System.getProperty("qpid.write.buffer.limit", DEFAULT_WRITE_BUFFER)));

            writefilter.attach(chain);

        }

        public void messageReceived(IoSession session, Object message) throws Exception
        {
            ((ByteBuffer) message).acquire();
            session.write(message);

            if (_logger.isDebugEnabled())
            {
                _bytesSent += ((ByteBuffer) message).remaining();

                _sentCount++;

                if (_sentCount % 1000 == 0)
                {
                    _logger.debug("Bytes sent: " + _bytesSent);
                }
            }
        }

        public void messageSent(IoSession session, Object message) throws Exception
        {
            if (_logger.isDebugEnabled())
            {
                ++_receivedCount;

                if (_receivedCount % 1000 == 0)
                {
                    _logger.debug("Receieved count " + _receivedCount);
                }
            }
        }

        public void exceptionCaught(IoSession session, Throwable cause) throws Exception
        {
            _logger.error("Error: " + cause, cause);
        }
    }

    public void startAcceptor() throws IOException
    {
        IoAcceptor acceptor;
        if (Boolean.getBoolean("multinio"))
        {
            _logger.warn("Using MultiThread NIO");
            acceptor = new org.apache.mina.transport.socket.nio.MultiThreadSocketAcceptor();
        }
        else
        {
            _logger.warn("Using MINA NIO");
            acceptor = new org.apache.mina.transport.socket.nio.SocketAcceptor();
        }


        SocketSessionConfig sc = (SocketSessionConfig) acceptor.getDefaultConfig().getSessionConfig();
        sc.setTcpNoDelay(true);
        sc.setSendBufferSize(32768);
        sc.setReceiveBufferSize(32768);

        ByteBuffer.setAllocator(new SimpleByteBufferAllocator());

        //The old mina style
//        acceptor.setLocalAddress(new InetSocketAddress(_PORT));
//        acceptor.setHandler(new TestHandler());
//        acceptor.bind();
        acceptor.bind(new InetSocketAddress(_PORT), new TestHandler());

        _logger.info("Bound on port " + _PORT + ":" + _logger.isDebugEnabled());
        _logger.debug("debug on");
    }

    public static void main(String[] args) throws IOException
    {

        if (args.length > 0)
        {
            try
            {
                _PORT = Integer.parseInt(args[0]);
            }
            catch (NumberFormatException e)
            {
                //IGNORE so use default port 9999;
            }
        }

        IOWriterServer a = new IOWriterServer();
        a.startAcceptor();
    }
}
