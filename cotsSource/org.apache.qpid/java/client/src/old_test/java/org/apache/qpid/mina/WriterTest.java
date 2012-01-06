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
import org.apache.mina.common.*;
import org.apache.mina.transport.socket.nio.SocketConnector;
import org.apache.mina.transport.socket.nio.SocketConnectorConfig;
import org.apache.mina.transport.socket.nio.SocketSessionConfig;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.CountDownLatch;

import junit.framework.TestCase;

public class WriterTest extends TestCase
{
    private static final Logger _logger = Logger.getLogger(WriterTest.class);

    private static class RunnableWriterTest implements Runnable
    {
        private Logger _logger;

        private IoSession _session;

        private long _startTime;

        private long[] _chunkTimes;

        private int _chunkCount = 500000;

        private int _chunkSize = 1024;

        private CountDownLatch _notifier;

        public RunnableWriterTest(Logger logger)
        {
            _logger = logger;
        }

        public void run()
        {
            _startTime = System.currentTimeMillis();
            _notifier = new CountDownLatch(1);
            for (int i = 0; i < _chunkCount; i++)
            {
                ByteBuffer buf = ByteBuffer.allocate(_chunkSize, false);
                byte check = (byte) (i % 128);
                buf.put(check);
                buf.fill((byte)88, buf.remaining());
                buf.flip();
                _session.write(buf);
            }

            try
            {
                _logger.info("All buffers sent; waiting for receipt from server");
                _notifier.await();
            }
            catch (InterruptedException e)
            {
            }
            _logger.info("Completed");
            long totalTime = System.currentTimeMillis() - _startTime;
            _logger.info("Total time: " + totalTime);
            _logger.info("MB per second: " + (_chunkSize * _chunkCount)/totalTime);
            long lastChunkTime = _startTime;
            double average = 0;
            for (int i = 0; i < _chunkTimes.length; i++)
            {
                if (i == 0)
                {
                    average = _chunkTimes[i] - _startTime;
                }
                else
                {
                    long delta = _chunkTimes[i] - lastChunkTime;
                    if (delta != 0)
                    {
                        average = (average + delta)/2;
                    }
                }
                lastChunkTime = _chunkTimes[i];
            }
            _logger.info("Average chunk time: " + average + "ms");
            CloseFuture cf = _session.close();
            cf.join();
        }

        private class WriterHandler extends IoHandlerAdapter
        {
            private int _chunksReceived = 0;

            private int _partialBytesRead = 0;

            private byte _partialCheckNumber;

            private int _totalBytesReceived = 0;

            public void messageReceived(IoSession session, Object message) throws Exception
            {
                ByteBuffer result = (ByteBuffer) message;
                _totalBytesReceived += result.remaining();
                int size = result.remaining();
                long now = System.currentTimeMillis();
                if (_partialBytesRead > 0)
                {
                    int offset = _chunkSize - _partialBytesRead;
                    if (size >= offset)
                    {
                        _chunkTimes[_chunksReceived++] = now;
                        result.position(offset);
                    }
                    else
                    {
                        // have not read even one chunk, including the previous partial bytes
                        _partialBytesRead += size;
                        return;
                    }
                }

                int chunkCount = result.remaining()/_chunkSize;

                for (int i = 0; i < chunkCount; i++)
                {
                    _chunkTimes[_chunksReceived++] = now;
                    byte check = result.get();
                    _logger.debug("Check number " + check + " read");
                    if (check !=  (byte)((_chunksReceived - 1)%128))
                    {
                        _logger.error("Check number " + check + " read when expected " + (_chunksReceived%128));
                    }
                    _logger.debug("Chunk times recorded");

                    try
                    {
                        result.skip(_chunkSize - 1);
                    }
                    catch (IllegalArgumentException e)
                    {
                        _logger.error("Position was: " + result.position());
                        _logger.error("Tried to skip to: " + (_chunkSize * i));
                        _logger.error("limit was; " + result.limit());
                    }
                }
                _logger.debug("Chunks received now " + _chunksReceived);
                _logger.debug("Bytes received: " + _totalBytesReceived);
                _partialBytesRead = result.remaining();

                if (_partialBytesRead > 0)
                {
                    _partialCheckNumber = result.get();
                }

                if (_chunksReceived >= _chunkCount)
                {
                    _notifier.countDown();
                }

            }

            public void exceptionCaught(IoSession session, Throwable cause) throws Exception
            {
                _logger.error("Error: " + cause, cause);
            }
        }

        public void startWriter(int chunkSize) throws IOException, InterruptedException
        {
            _chunkSize = chunkSize;

            IoConnector ioConnector = null;

            ioConnector = new SocketConnector();
        
            SocketConnectorConfig cfg = (SocketConnectorConfig) ioConnector.getDefaultConfig();
            cfg.setThreadModel(ThreadModel.MANUAL);
            SocketSessionConfig scfg = (SocketSessionConfig) cfg.getSessionConfig();
            scfg.setTcpNoDelay(true);
            scfg.setSendBufferSize(32768);
            scfg.setReceiveBufferSize(32768);

            final InetSocketAddress address = new InetSocketAddress("localhost", AcceptorTest.PORT);
            _logger.info("Attempting connection to " + address);
            ConnectFuture future = ioConnector.connect(address, new WriterHandler());
            // wait for connection to complete
            future.join();
            _logger.info("Connection completed");
            // we call getSession which throws an IOException if there has been an error connecting
            _session = future.getSession();
            _chunkTimes = new long[_chunkCount];
            Thread t = new Thread(this);
            t.start();
            t.join();
            _logger.info("Test completed");
        }
    }

    private RunnableWriterTest _runnableWriterTest = new RunnableWriterTest(_logger);

    public void test1k() throws IOException, InterruptedException
    {
        _logger.info("Starting 1k test");
        _runnableWriterTest.startWriter(1024);
    }

    public void test2k() throws IOException, InterruptedException
    {
        _logger.info("Starting 2k test");
        _runnableWriterTest.startWriter(2048);
    }

    public void test4k() throws IOException, InterruptedException
    {
        _logger.info("Starting 4k test");
        _runnableWriterTest.startWriter(4096);
    }

    public void test8k() throws IOException, InterruptedException
    {
        _logger.info("Starting 8k test");
        _runnableWriterTest.startWriter(8192);
    }

    public void test16k() throws IOException, InterruptedException
    {
        _logger.info("Starting 16k test");
        _runnableWriterTest.startWriter(16384);
    }

    public void test32k() throws IOException, InterruptedException
    {
        _logger.info("Starting 32k test");
        _runnableWriterTest.startWriter(32768);
    }

    public static void main(String[] args) throws IOException, InterruptedException
    {
        WriterTest w = new WriterTest();
        //w.test1k();
        //w.test2k();
        //w.test4k();
        w.test8k();
        //w.test16k();
        //w.test32k();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(WriterTest.class);
    }
}
