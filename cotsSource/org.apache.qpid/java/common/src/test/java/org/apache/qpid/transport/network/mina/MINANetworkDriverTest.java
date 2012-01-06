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

package org.apache.qpid.transport.network.mina;

import java.net.BindException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import org.apache.qpid.framing.AMQDataBlock;
import org.apache.qpid.protocol.ProtocolEngine;
import org.apache.qpid.protocol.ProtocolEngineFactory;
import org.apache.qpid.transport.NetworkDriver;
import org.apache.qpid.transport.OpenException;

public class MINANetworkDriverTest extends TestCase
{
 
    private static final String TEST_DATA = "YHALOTHAR";
    private static final int TEST_PORT = 2323;
    private NetworkDriver _server;
    private NetworkDriver _client;
    private CountingProtocolEngine _countingEngine; // Keeps a count of how many bytes it's read
    private Exception _thrownEx;

    @Override
    public void setUp()
    {
        _server = new MINANetworkDriver();
        _client = new MINANetworkDriver();
        _thrownEx = null;
        _countingEngine = new CountingProtocolEngine();
    }

    @Override
    public void tearDown()
    {
        if (_server != null)
        {
            _server.close();
        }

        if (_client != null)
        {
            _client.close();
        }
    }
    
    /**
     * Tests that a socket can't be opened if a driver hasn't been bound
     * to the port and can be opened if a driver has been bound.
     * @throws BindException
     * @throws UnknownHostException
     * @throws OpenException 
     */
    public void testBindOpen() throws BindException, UnknownHostException, OpenException  
    {
        try
        {
            _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
        } 
        catch (OpenException e)
        {
            _thrownEx = e;
        }

        assertNotNull("Open should have failed since no engine bound", _thrownEx);        
        
        _server.bind(TEST_PORT, null, new EchoProtocolEngineSingletonFactory(), null, null);
        
        _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
    } 
    
    /**
     * Tests that a socket can't be opened after a bound NetworkDriver has been closed
     * @throws BindException
     * @throws UnknownHostException
     * @throws OpenException
     */
    public void testBindOpenCloseOpen() throws BindException, UnknownHostException, OpenException  
    {
        _server.bind(TEST_PORT, null, new EchoProtocolEngineSingletonFactory(), null, null);
        _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
        _client.close();
        _server.close();
        
        try
        {
            _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
        } 
        catch (OpenException e)
        {
            _thrownEx = e;
        }
        assertNotNull("Open should have failed", _thrownEx);
    }
    
    /**
     * Checks that the right exception is thrown when binding a NetworkDriver to an already
     * existing socket. 
     */
    public void testBindPortInUse() 
    {
        try
        {
            _server.bind(TEST_PORT, null, new EchoProtocolEngineSingletonFactory(), null, null);
        }
        catch (BindException e)
        {
            fail("First bind should not fail");
        }
        
        try
        {
            _client.bind(TEST_PORT, null, new EchoProtocolEngineSingletonFactory(), null, null);
        }
        catch (BindException e)
        {
            _thrownEx = e;
        }
        assertNotNull("Second bind should throw BindException", _thrownEx);
    } 
    
    /**
     * tests that bytes sent on a network driver are received at the other end
     * 
     * @throws UnknownHostException
     * @throws OpenException
     * @throws InterruptedException 
     * @throws BindException 
     */
    public void testSend() throws UnknownHostException, OpenException, InterruptedException, BindException 
    {
        // Open a connection from a counting engine to an echo engine
        _server.bind(TEST_PORT, null,  new EchoProtocolEngineSingletonFactory(), null, null);
        _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
        
        // Tell the counting engine how much data we're sending
        _countingEngine.setNewLatch(TEST_DATA.getBytes().length);
        
        // Send the data and wait for up to 2 seconds to get it back 
        _client.send(ByteBuffer.wrap(TEST_DATA.getBytes()));
        _countingEngine.getLatch().await(2, TimeUnit.SECONDS);
        
        // Check what we got
        assertEquals("Wrong amount of data recieved", TEST_DATA.getBytes().length, _countingEngine.getReadBytes());
    } 
    
    /**
     * Opens a connection with a low read idle and check that it gets triggered
     * @throws BindException 
     * @throws OpenException 
     * @throws UnknownHostException 
     * 
     */
    public void testSetReadIdle() throws BindException, UnknownHostException, OpenException 
    {
        // Open a connection from a counting engine to an echo engine
        _server.bind(TEST_PORT, null,  new EchoProtocolEngineSingletonFactory(), null, null);
        _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
        assertFalse("Reader should not have been idle", _countingEngine.getReaderHasBeenIdle());
        _client.setMaxReadIdle(1);
        sleepForAtLeast(1500);
        assertTrue("Reader should have been idle", _countingEngine.getReaderHasBeenIdle());
    } 
    
    /**
     * Opens a connection with a low write idle and check that it gets triggered
     * @throws BindException 
     * @throws OpenException 
     * @throws UnknownHostException 
     * 
     */
    public void testSetWriteIdle() throws BindException, UnknownHostException, OpenException 
    {
        // Open a connection from a counting engine to an echo engine
        _server.bind(TEST_PORT, null,  new EchoProtocolEngineSingletonFactory(), null, null);
        _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
        assertFalse("Reader should not have been idle", _countingEngine.getWriterHasBeenIdle());
        _client.setMaxWriteIdle(1);
        sleepForAtLeast(1500);
        assertTrue("Reader should have been idle", _countingEngine.getWriterHasBeenIdle());
    } 
    
    
    /**
     * Creates and then closes a connection from client to server and checks that the server
     * has its closed() method called. Then creates a new client and closes the server to check
     * that the client has its closed() method called.  
     * @throws BindException
     * @throws UnknownHostException
     * @throws OpenException
     */
    public void testClosed() throws BindException, UnknownHostException, OpenException 
    {
        // Open a connection from a counting engine to an echo engine
        EchoProtocolEngineSingletonFactory factory = new EchoProtocolEngineSingletonFactory();
        _server.bind(TEST_PORT, null, factory, null, null);
        _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
        EchoProtocolEngine serverEngine = null; 
        while (serverEngine == null)
        {
            serverEngine = factory.getEngine();
            if (serverEngine == null)
            {
                try
                {
                    Thread.sleep(10);
                }
                catch (InterruptedException e)
                {
                }
            }
        }
        assertFalse("Server should not have been closed", serverEngine.getClosed());
        serverEngine.setNewLatch(1);
        _client.close();
        try
        {
            serverEngine.getLatch().await(2, TimeUnit.SECONDS);
        }
        catch (InterruptedException e)
        {
        }
        assertTrue("Server should have been closed", serverEngine.getClosed());

        _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
        _countingEngine.setClosed(false);
        assertFalse("Client should not have been closed", _countingEngine.getClosed());
        _countingEngine.setNewLatch(1);
        _server.close();
        try
        {
            _countingEngine.getLatch().await(2, TimeUnit.SECONDS);
        }
        catch (InterruptedException e)
        {
        }
        assertTrue("Client should have been closed", _countingEngine.getClosed());
    } 

    /**
     * Create a connection and instruct the client to throw an exception when it gets some data
     * and that the latch gets counted down. 
     * @throws BindException
     * @throws UnknownHostException
     * @throws OpenException
     * @throws InterruptedException
     */
    public void testExceptionCaught() throws BindException, UnknownHostException, OpenException, InterruptedException 
    {
        _server.bind(TEST_PORT, null,  new EchoProtocolEngineSingletonFactory(), null, null);
        _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);


        assertEquals("Exception should not have been thrown", 1, 
                _countingEngine.getExceptionLatch().getCount());
        _countingEngine.setErrorOnNextRead(true);
        _countingEngine.setNewLatch(TEST_DATA.getBytes().length);
        _client.send(ByteBuffer.wrap(TEST_DATA.getBytes()));
        _countingEngine.getExceptionLatch().await(2, TimeUnit.SECONDS);
        assertEquals("Exception should have been thrown", 0, 
                _countingEngine.getExceptionLatch().getCount());
    } 
    
    /**
     * Opens a connection and checks that the remote address is the one that was asked for
     * @throws BindException
     * @throws UnknownHostException
     * @throws OpenException
     */
    public void testGetRemoteAddress() throws BindException, UnknownHostException, OpenException
    {
        _server.bind(TEST_PORT, null,  new EchoProtocolEngineSingletonFactory(), null, null);
        _client.open(TEST_PORT, InetAddress.getLocalHost(), _countingEngine, null, null);
        assertEquals(new InetSocketAddress(InetAddress.getLocalHost(), TEST_PORT),
                     _client.getRemoteAddress());
    }

    private class EchoProtocolEngineSingletonFactory implements ProtocolEngineFactory
    {
        EchoProtocolEngine _engine = null;
        
        public ProtocolEngine newProtocolEngine(NetworkDriver driver)
        {
            if (_engine == null)
            {
                _engine = new EchoProtocolEngine();
                _engine.setNetworkDriver(driver);
            }
            return getEngine();
        }
        
        public EchoProtocolEngine getEngine()
        {
            return _engine;
        }
    }
    
    public class CountingProtocolEngine implements ProtocolEngine
    {

        protected NetworkDriver _driver;
        public ArrayList<ByteBuffer> _receivedBytes = new ArrayList<ByteBuffer>();
        private int _readBytes;
        private CountDownLatch _latch = new CountDownLatch(0);
        private boolean _readerHasBeenIdle;
        private boolean _writerHasBeenIdle;
        private boolean _closed = false;
        private boolean _nextReadErrors = false;
        private CountDownLatch _exceptionLatch = new CountDownLatch(1);
        
        public void closed()
        {
            setClosed(true);
            _latch.countDown();
        }

        public void setErrorOnNextRead(boolean b)
        {
            _nextReadErrors = b;
        }

        public void setNewLatch(int length)
        {
            _latch = new CountDownLatch(length);
        }

        public long getReadBytes()
        {
            return _readBytes;
        }

        public SocketAddress getRemoteAddress()
        {
            if (_driver != null)
            {
                return _driver.getRemoteAddress();
            } 
            else
            {
                return null;
            }
        }
        
        public SocketAddress getLocalAddress()
        {            
            if (_driver != null)
            {
                return _driver.getLocalAddress();
            } 
            else
            {
                return null;
            }
        }

        public long getWrittenBytes()
        {
            return 0;
        }

        public void readerIdle()
        {
            _readerHasBeenIdle = true;
        }

        public void setNetworkDriver(NetworkDriver driver)
        {
            _driver = driver;
        }

        public void writeFrame(AMQDataBlock frame)
        {
            
        }

        public void writerIdle()
        {
           _writerHasBeenIdle = true;
        }

        public void exception(Throwable t)
        {
            _exceptionLatch.countDown();
        }

        public CountDownLatch getExceptionLatch()
        {
            return _exceptionLatch;
        }
        
        public void received(ByteBuffer msg)
        {
            // increment read bytes and count down the latch for that many
            int bytes = msg.remaining();
            _readBytes += bytes;
            for (int i = 0; i < bytes; i++)
            {
                _latch.countDown();
            }
            
            // Throw an error if we've been asked too, but we can still count
            if (_nextReadErrors)
            {
                throw new RuntimeException("Was asked to error");
            }
        }
        
        public CountDownLatch getLatch()
        {
            return _latch;
        }

        public boolean getWriterHasBeenIdle()
        {
            return _writerHasBeenIdle;
        }

        public boolean getReaderHasBeenIdle()
        {
            return _readerHasBeenIdle;
        }

        public void setClosed(boolean _closed)
        {
            this._closed = _closed;
        }

        public boolean getClosed()
        {
            return _closed;
        }

    }

    private class EchoProtocolEngine extends CountingProtocolEngine
    {

        public void received(ByteBuffer msg)
        {
            super.received(msg);
            msg.rewind();
            _driver.send(msg);
        }
    }
    
    public static void sleepForAtLeast(long period)
    {
        long start = System.currentTimeMillis();
        long timeLeft = period;
        while (timeLeft > 0)
        {
            try
            {
                Thread.sleep(timeLeft);
            }
            catch (InterruptedException e)
            {
                // Ignore it
            }
            timeLeft = period - (System.currentTimeMillis() - start);
        }
    }
}