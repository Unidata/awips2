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
import org.apache.mina.common.CloseFuture;
import org.apache.mina.common.ConnectFuture;
import org.apache.mina.common.IoConnector;
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
import java.util.concurrent.CountDownLatch;

public class IOWriterClient implements Runnable
{
    private static final Logger _logger = LoggerFactory.getLogger(IOWriterClient.class);

    public static int DEFAULT_TEST_SIZE = 2;

    private IoSession _session;

    private long _startTime;

    private long[] _chunkTimes;

    public int _chunkCount = 200000;

    private int _chunkSize = 1024;

    private CountDownLatch _notifier;

    private int _maximumWriteQueueLength;

    static public int _PORT = IOWriterServer._PORT;

    public void run()
    {
        _logger.info("Starting to send " + _chunkCount + " buffers of " + _chunkSize + "B");
        _startTime = System.currentTimeMillis();
        _notifier = new CountDownLatch(1);

        for (int i = 0; i < _chunkCount; i++)
        {
            ByteBuffer buf = ByteBuffer.allocate(_chunkSize, false);
            byte check = (byte) (i % 128);
            buf.put(check);
            buf.fill((byte) 88, buf.remaining());
            buf.flip();

            _session.write(buf);
        }

        long _sentall = System.currentTimeMillis();
        long _receivedall = _sentall;
        try
        {
            _logger.info("All buffers sent; waiting for receipt from server");
            _notifier.await();
            _receivedall = System.currentTimeMillis();
        }
        catch (InterruptedException e)
        {
            //Ignore
        }
        _logger.info("Completed");
        _logger.info("Total time waiting for server after last write: " + (_receivedall - _sentall));

        long totalTime = System.currentTimeMillis() - _startTime;

        _logger.info("Total time: " + totalTime);
        _logger.info("MB per second: " + (int) ((1.0 * _chunkSize * _chunkCount) / totalTime));
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
                    average = (average + delta) / 2;
                }
            }
            lastChunkTime = _chunkTimes[i];
        }
        _logger.info("Average chunk time: " + average + "ms");
        _logger.info("Maximum WriteRequestQueue size: " + _maximumWriteQueueLength);

        CloseFuture cf = _session.close();
        _logger.info("Closing session");
        cf.join();
    }

    private class WriterHandler extends IoHandlerAdapter
    {
        private int _chunksReceived = 0;

        private int _partialBytesRead = 0;

        private byte _partialCheckNumber;

        private int _totalBytesReceived = 0;

        private int _receivedCount = 0;
        private int _sentCount = 0;
        private static final String DEFAULT_READ_BUFFER = "262144";
        private static final String DEFAULT_WRITE_BUFFER = "262144";

        public void sessionCreated(IoSession session) throws Exception
        {
            IoFilterChain chain = session.getFilterChain();

            ReadThrottleFilterBuilder readfilter = new ReadThrottleFilterBuilder();
            readfilter.setMaximumConnectionBufferSize(Integer.parseInt(System.getProperty("qpid.read.buffer.limit", DEFAULT_READ_BUFFER)));
            readfilter.attach(chain);

            WriteBufferLimitFilterBuilder writefilter = new WriteBufferLimitFilterBuilder();

            writefilter.setMaximumConnectionBufferSize(Integer.parseInt(System.getProperty("qpid.write.buffer.limit", DEFAULT_WRITE_BUFFER)));

            writefilter.attach(chain);
        }

        public void messageSent(IoSession session, Object message) throws Exception
        {
            _maximumWriteQueueLength = Math.max(session.getScheduledWriteRequests(), _maximumWriteQueueLength);

            if (_logger.isDebugEnabled())
            {
                ++_sentCount;
                if (_sentCount % 1000 == 0)
                {
                    _logger.debug("Sent count " + _sentCount + ":WQueue" + session.getScheduledWriteRequests());

                }
            }
        }

        public void messageReceived(IoSession session, Object message) throws Exception
        {
            if (_logger.isDebugEnabled())
            {
                ++_receivedCount;

                if (_receivedCount % 1000 == 0)
                {
                    _logger.debug("Receieved count " + _receivedCount);
                }
            }

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


            int chunkCount = result.remaining() / _chunkSize;

            for (int i = 0; i < chunkCount; i++)
            {
                _chunkTimes[_chunksReceived++] = now;
                byte check = result.get();
                _logger.debug("Check number " + check + " read");
                if (check != (byte) ((_chunksReceived - 1) % 128))
                {
                    _logger.error("Check number " + check + " read when expected " + (_chunksReceived % 128));
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

    public void startWriter() throws IOException, InterruptedException
    {

        _maximumWriteQueueLength = 0;

        IoConnector ioConnector = null;

        if (Boolean.getBoolean("multinio"))
        {
            _logger.warn("Using MultiThread NIO");
            ioConnector = new org.apache.mina.transport.socket.nio.MultiThreadSocketConnector();
        }
        else
        {
            _logger.warn("Using MINA NIO");
            ioConnector = new org.apache.mina.transport.socket.nio.SocketConnector();
        }

        SocketSessionConfig scfg = (SocketSessionConfig) ioConnector.getDefaultConfig().getSessionConfig();
        scfg.setTcpNoDelay(true);
        scfg.setSendBufferSize(32768);
        scfg.setReceiveBufferSize(32768);

        ByteBuffer.setAllocator(new SimpleByteBufferAllocator());


        final InetSocketAddress address = new InetSocketAddress("localhost", _PORT);
        _logger.info("Attempting connection to " + address);

        //Old mina style
//        ioConnector.setHandler(new WriterHandler());
//        ConnectFuture future = ioConnector.connect(address);
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
        _logger.info("Test Complete");
    }


    public void test1k() throws IOException, InterruptedException
    {
        _logger.info("Starting 1k test");
        _chunkSize = 1024;
        startWriter();
    }


    public void test2k() throws IOException, InterruptedException
    {
        _logger.info("Starting 2k test");
        _chunkSize = 2048;
        startWriter();
    }


    public void test4k() throws IOException, InterruptedException
    {
        _logger.info("Starting 4k test");
        _chunkSize = 4096;
        startWriter();
    }


    public void test8k() throws IOException, InterruptedException
    {
        _logger.info("Starting 8k test");
        _chunkSize = 8192;
        startWriter();
    }


    public void test16k() throws IOException, InterruptedException
    {
        _logger.info("Starting 16k test");
        _chunkSize = 16384;
        startWriter();
    }


    public void test32k() throws IOException, InterruptedException
    {
        _logger.info("Starting 32k test");
        _chunkSize = 32768;
        startWriter();
    }


    public static int getIntArg(String[] args, int index, int defaultValue)
    {
        if (args.length > index)
        {
            try
            {
                return Integer.parseInt(args[index]);
            }
            catch (NumberFormatException e)
            {
                //Do nothing
            }
        }
        return defaultValue;
    }

    public static void main(String[] args) throws IOException, InterruptedException
    {
        _PORT = getIntArg(args, 0, _PORT);

        int test = getIntArg(args, 1, DEFAULT_TEST_SIZE);

        IOWriterClient w = new IOWriterClient();
        w._chunkCount = getIntArg(args, 2, w._chunkCount);
        switch (test)
        {
            case 0:
                w.test1k();
                w.test2k();
                w.test4k();
                w.test8k();
                w.test16k();
                w.test32k();
                break;
            case 1:
                w.test1k();
                break;
            case 2:
                w.test2k();
                break;
            case 4:
                w.test4k();
                break;
            case 8:
                w.test8k();
                break;
            case 16:
                w.test16k();
                break;
            case 32:
                w.test32k();
                break;
        }
    }
}
