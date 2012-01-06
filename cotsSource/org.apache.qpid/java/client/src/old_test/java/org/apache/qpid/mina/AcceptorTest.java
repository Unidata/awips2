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
package org.apache.qpid.mina;

import org.apache.log4j.Logger;
import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.IoAcceptor;
import org.apache.mina.common.IoHandlerAdapter;
import org.apache.mina.common.IoSession;
import org.apache.mina.transport.socket.nio.SocketAcceptor;
import org.apache.mina.transport.socket.nio.SocketAcceptorConfig;
import org.apache.mina.transport.socket.nio.SocketSessionConfig;
import org.apache.qpid.pool.ReadWriteThreadModel;

import java.io.IOException;
import java.net.InetSocketAddress;

import junit.framework.TestCase;

/**
 * Tests MINA socket performance. This acceptor simply reads data from the network and writes it back again.
 *
 */
public class AcceptorTest extends TestCase
{
    private static final Logger _logger = Logger.getLogger(AcceptorTest.class);

    public static int PORT = 9999;

    private static class TestHandler extends IoHandlerAdapter
    {
        private int _sentCount;

        private int _bytesSent;

        public void messageReceived(IoSession session, Object message) throws Exception
        {
            ((ByteBuffer) message).acquire();
            session.write(message);
            _logger.debug("Sent response " + ++_sentCount);
            _bytesSent += ((ByteBuffer)message).remaining();
            _logger.debug("Bytes sent: " + _bytesSent);
        }

        public void messageSent(IoSession session, Object message) throws Exception
        {
            //((ByteBuffer) message).release();
        }

        public void exceptionCaught(IoSession session, Throwable cause) throws Exception
        {
            _logger.error("Error: " + cause, cause);
        }
    }

    public void testStartAcceptor() throws IOException
    {
        IoAcceptor acceptor = null;
        acceptor = new SocketAcceptor();
        
        SocketAcceptorConfig config = (SocketAcceptorConfig) acceptor.getDefaultConfig();
        SocketSessionConfig sc = (SocketSessionConfig) config.getSessionConfig();
        sc.setTcpNoDelay(true);
        sc.setSendBufferSize(32768);
        sc.setReceiveBufferSize(32768);

        config.setThreadModel(ReadWriteThreadModel.getInstance());

        acceptor.bind(new InetSocketAddress(PORT),
                      new TestHandler());
        _logger.info("Bound on port " + PORT);
    }

    public static void main(String[] args) throws IOException
    {
        AcceptorTest a = new AcceptorTest();
        a.testStartAcceptor();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(AcceptorTest.class);
    }
}
