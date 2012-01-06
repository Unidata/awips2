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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;

import junit.framework.TestCase;

public class BlockingAcceptorTest extends TestCase
{
    private static final Logger _logger = Logger.getLogger(BlockingAcceptorTest.class);

    public static int PORT = 9999;

    public void testStartAcceptor() throws IOException
    {

        ServerSocket sock = new ServerSocket(PORT);

        sock.setReuseAddress(true);
        sock.setReceiveBufferSize(32768);
        _logger.info("Bound on port " + PORT);

        while (true)
        {
            final Socket s = sock.accept();
            _logger.info("Received connection from " + s.getRemoteSocketAddress());
            s.setReceiveBufferSize(32768);
            s.setSendBufferSize(32768);
            s.setTcpNoDelay(true);
            new Thread(new Runnable()
            {
                public void run()
                {
                    byte[] chunk = new byte[32768];
                    try
                    {
                        InputStream is = s.getInputStream();
                        OutputStream os = s.getOutputStream();

                        while (true)
                        {
                            int count = is.read(chunk, 0, chunk.length);
                            if (count > 0)
                            {
                                os.write(chunk, 0, count);
                            }
                        }
                    }
                    catch (IOException e)
                    {
                        _logger.error("Error - closing connection: " + e, e);
                    }
                }
            },  "SocketReaderWriter").start();
        }
    }

    public static void main(String[] args) throws IOException
    {
        BlockingAcceptorTest a = new BlockingAcceptorTest();
        a.testStartAcceptor();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(AcceptorTest.class);
    }
}
