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

import java.io.IOException;
import java.net.BindException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;

import org.apache.mina.common.ConnectFuture;
import org.apache.mina.common.IdleStatus;
import org.apache.mina.common.IoAcceptor;
import org.apache.mina.common.IoConnector;
import org.apache.mina.common.IoFilterChain;
import org.apache.mina.common.IoHandlerAdapter;
import org.apache.mina.common.IoSession;
import org.apache.mina.common.SimpleByteBufferAllocator;
import org.apache.mina.common.WriteFuture;
import org.apache.mina.filter.ReadThrottleFilterBuilder;
import org.apache.mina.filter.SSLFilter;
import org.apache.mina.filter.WriteBufferLimitFilterBuilder;
import org.apache.mina.filter.executor.ExecutorFilter;
import org.apache.mina.transport.socket.nio.MultiThreadSocketConnector;
import org.apache.mina.transport.socket.nio.SocketAcceptorConfig;
import org.apache.mina.transport.socket.nio.SocketConnector;
import org.apache.mina.transport.socket.nio.SocketConnectorConfig;
import org.apache.mina.transport.socket.nio.SocketSessionConfig;
import org.apache.mina.util.NewThreadExecutor;
import org.apache.mina.util.SessionUtil;
import org.apache.qpid.protocol.ProtocolEngine;
import org.apache.qpid.protocol.ProtocolEngineFactory;
import org.apache.qpid.ssl.SSLContextFactory;
import org.apache.qpid.thread.QpidThreadExecutor;
import org.apache.qpid.transport.NetworkDriver;
import org.apache.qpid.transport.NetworkDriverConfiguration;
import org.apache.qpid.transport.OpenException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MINANetworkDriver extends IoHandlerAdapter implements NetworkDriver
{

    private static final int DEFAULT_BUFFER_SIZE = 32 * 1024;
    
    ProtocolEngine _protocolEngine;
    private boolean _useNIO = false;
    private int _processors = 4;
    private boolean _executorPool = false;
    private SSLContextFactory _sslFactory = null;
    private IoConnector _socketConnector;
    private IoAcceptor _acceptor;
    private IoSession _ioSession;
    private ProtocolEngineFactory _factory;
    private boolean _protectIO;
    private NetworkDriverConfiguration _config;
    private Throwable _lastException;
    private boolean _acceptingConnections = false;

    private WriteFuture _lastWriteFuture;

    private static final Logger _logger = LoggerFactory.getLogger(MINANetworkDriver.class);
    
    public MINANetworkDriver(boolean useNIO, int processors, boolean executorPool, boolean protectIO)
    {
        _useNIO = useNIO;
        _processors = processors;
        _executorPool = executorPool;
        _protectIO = protectIO;
    }

    public MINANetworkDriver(boolean useNIO, int processors, boolean executorPool, boolean protectIO,
            ProtocolEngine protocolEngine, IoSession session)
    {
        _useNIO = useNIO;
        _processors = processors;
        _executorPool = executorPool;
        _protectIO = protectIO;
        _protocolEngine = protocolEngine;
        _ioSession = session;
        _ioSession.setAttachment(_protocolEngine);
    }
    
    public MINANetworkDriver()
    {

    }

    public MINANetworkDriver(IoConnector ioConnector)
    {
        _socketConnector = ioConnector;
    }
    
    public MINANetworkDriver(IoConnector ioConnector, ProtocolEngine engine)
    {
        _socketConnector = ioConnector;
        _protocolEngine = engine;
    }

    public void bind(int port, InetAddress[] addresses, ProtocolEngineFactory factory,
            NetworkDriverConfiguration config, SSLContextFactory sslFactory) throws BindException
    {

        _factory = factory;
        _config = config;
        
        if (_useNIO)
        {
            _acceptor = new org.apache.mina.transport.socket.nio.MultiThreadSocketAcceptor(_processors,
                    new NewThreadExecutor());
        }
        else
        {
            _acceptor = new org.apache.mina.transport.socket.nio.SocketAcceptor(_processors, new NewThreadExecutor());
        }

        SocketAcceptorConfig sconfig = (SocketAcceptorConfig) _acceptor.getDefaultConfig();
        SocketSessionConfig sc = (SocketSessionConfig) sconfig.getSessionConfig();

        if (config != null)
        {
            sc.setReceiveBufferSize(config.getReceiveBufferSize());
            sc.setSendBufferSize(config.getSendBufferSize());
            sc.setTcpNoDelay(config.getTcpNoDelay());
        }

        if (sslFactory != null)
        {
            _sslFactory = sslFactory;
        }

        if (addresses != null && addresses.length > 0)
        {
            for (InetAddress addr : addresses)
            {
                try
                {
                    _acceptor.bind(new InetSocketAddress(addr, port), this, sconfig);
                }
                catch (IOException e)
                {
                    throw new BindException(String.format("Could not bind to %1s:%2s", addr, port));
                }
            }
        }
        else
        {
            try
            {
                _acceptor.bind(new InetSocketAddress(port), this, sconfig);
            }
            catch (IOException e)
            {
                throw new BindException(String.format("Could not bind to *:%1s", port));
            }
        }
        _acceptingConnections  = true;
    }

    public SocketAddress getRemoteAddress()
    {
        return _ioSession.getRemoteAddress();
    }
    
    public SocketAddress getLocalAddress()
    {
        return _ioSession.getLocalAddress();
    }
    

    public void open(int port, InetAddress destination, ProtocolEngine engine, NetworkDriverConfiguration config,
            SSLContextFactory sslFactory) throws OpenException
    {
        if (sslFactory != null)
        {
            _sslFactory = sslFactory;
        }
        
        if (_useNIO)
        {
            _socketConnector = new MultiThreadSocketConnector(1, new QpidThreadExecutor());
        }
        else
        {
            _socketConnector = new SocketConnector(1, new QpidThreadExecutor()); // non-blocking
                                                                                 // connector
        }
        
        org.apache.mina.common.ByteBuffer.setUseDirectBuffers(Boolean.getBoolean("amqj.enableDirectBuffers"));
        // the MINA default is currently to use the pooled allocator although this may change in future
        // once more testing of the performance of the simple allocator has been done
        if (!Boolean.getBoolean("amqj.enablePooledAllocator"))
        {
            org.apache.mina.common.ByteBuffer.setAllocator(new SimpleByteBufferAllocator());
        }

        SocketConnectorConfig cfg = (SocketConnectorConfig) _socketConnector.getDefaultConfig();
        
        SocketSessionConfig scfg = (SocketSessionConfig) cfg.getSessionConfig();
        scfg.setTcpNoDelay((config != null) ? config.getTcpNoDelay() :  true);
        scfg.setSendBufferSize((config != null) ? config.getSendBufferSize() : DEFAULT_BUFFER_SIZE);
        scfg.setReceiveBufferSize((config != null) ? config.getReceiveBufferSize() : DEFAULT_BUFFER_SIZE);
        
        // Don't have the connector's worker thread wait around for other
        // connections (we only use
        // one SocketConnector per connection at the moment anyway). This allows
        // short-running
        // clients (like unit tests) to complete quickly.
        if (_socketConnector instanceof SocketConnector)
        {
            ((SocketConnector) _socketConnector).setWorkerTimeout(0);
        }
        
        ConnectFuture future = _socketConnector.connect(new InetSocketAddress(destination, port), this, cfg);
        future.join();
        if (!future.isConnected())
        {
            throw new OpenException("Could not open connection", _lastException);
        }
        _ioSession = future.getSession();
        _ioSession.setAttachment(engine);
        engine.setNetworkDriver(this);
        _protocolEngine = engine;
    }

    public void setMaxReadIdle(int idleTime)
    {
        _ioSession.setIdleTime(IdleStatus.READER_IDLE, idleTime);
    }

    public void setMaxWriteIdle(int idleTime)
    {
        _ioSession.setIdleTime(IdleStatus.WRITER_IDLE, idleTime);
    }

    public void close()
    {
        if (_lastWriteFuture != null)
        {
            _lastWriteFuture.join();
        }
        if (_acceptor != null)
        {
            _acceptor.unbindAll();
        }
        if (_ioSession != null)
        {
            _ioSession.close();
        }
    }

    public void flush()
    {
        if (_lastWriteFuture != null)
        {
            _lastWriteFuture.join();
        }
    }

    public void send(ByteBuffer msg)
    {
        org.apache.mina.common.ByteBuffer minaBuf = org.apache.mina.common.ByteBuffer.allocate(msg.capacity());
        minaBuf.put(msg);
        minaBuf.flip();
        _lastWriteFuture = _ioSession.write(minaBuf);
    }

    public void setIdleTimeout(long l)
    {
        // MINA doesn't support setting SO_TIMEOUT
    }

    public void exceptionCaught(IoSession protocolSession, Throwable throwable) throws Exception
    {
        if (_protocolEngine != null)
        {
            _protocolEngine.exception(throwable);
        } 
        else
        {
            _logger.error("Exception thrown and no ProtocolEngine to handle it", throwable);
        }
        _lastException = throwable;
    }

    /**
     * Invoked when a message is received on a particular protocol session. Note
     * that a protocol session is directly tied to a particular physical
     * connection.
     * 
     * @param protocolSession
     *            the protocol session that received the message
     * @param message
     *            the message itself (i.e. a decoded frame)
     * 
     * @throws Exception
     *             if the message cannot be processed
     */
    public void messageReceived(IoSession protocolSession, Object message) throws Exception
    {
        if (message instanceof org.apache.mina.common.ByteBuffer)
        {
            ((ProtocolEngine) protocolSession.getAttachment()).received(((org.apache.mina.common.ByteBuffer) message).buf());
        }
        else
        {
            throw new IllegalStateException("Handed unhandled message. message.class = " + message.getClass() + " message = " + message);
        }
    }

    public void sessionClosed(IoSession protocolSession) throws Exception
    {
        ((ProtocolEngine) protocolSession.getAttachment()).closed();
    }

    public void sessionCreated(IoSession protocolSession) throws Exception
    {
        // Configure the session with SSL if necessary
        SessionUtil.initialize(protocolSession);
        if (_executorPool)
        {
            if (_sslFactory != null)
            {
                protocolSession.getFilterChain().addAfter("AsynchronousReadFilter", "sslFilter",
                        new SSLFilter(_sslFactory.buildServerContext()));
            }
        }
        else
        {
            if (_sslFactory != null)
            {
                protocolSession.getFilterChain().addBefore("protocolFilter", "sslFilter",
                        new SSLFilter(_sslFactory.buildServerContext()));
            }
        }
        // Do we want to have read/write buffer limits?
        if (_protectIO)
        {
            //Add IO Protection Filters
            IoFilterChain chain = protocolSession.getFilterChain();

            protocolSession.getFilterChain().addLast("tempExecutorFilterForFilterBuilder", new ExecutorFilter());

            ReadThrottleFilterBuilder readfilter = new ReadThrottleFilterBuilder();
            readfilter.setMaximumConnectionBufferSize(_config.getReceiveBufferSize());
            readfilter.attach(chain);

            WriteBufferLimitFilterBuilder writefilter = new WriteBufferLimitFilterBuilder();
            writefilter.setMaximumConnectionBufferSize(_config.getSendBufferSize());
            writefilter.attach(chain);

            protocolSession.getFilterChain().remove("tempExecutorFilterForFilterBuilder");
        }

        if (_ioSession == null)
        {
            _ioSession = protocolSession;
        }
        
        if (_acceptingConnections)
        {
            // Set up the protocol engine
            ProtocolEngine protocolEngine = _factory.newProtocolEngine(this);
            MINANetworkDriver newDriver = new MINANetworkDriver(_useNIO, _processors, _executorPool, _protectIO, protocolEngine, protocolSession);
            protocolEngine.setNetworkDriver(newDriver);
        }
    }

    public void sessionIdle(IoSession session, IdleStatus status) throws Exception
    {
        if (IdleStatus.WRITER_IDLE.equals(status))
        {   
            ((ProtocolEngine) session.getAttachment()).writerIdle();
        }
        else if (IdleStatus.READER_IDLE.equals(status))
        {
            ((ProtocolEngine) session.getAttachment()).readerIdle();        
        }
    }

    private ProtocolEngine getProtocolEngine()
    {
       return _protocolEngine;
    }

    public void setProtocolEngineFactory(ProtocolEngineFactory engineFactory, boolean acceptingConnections)
    {
        _factory = engineFactory;
        _acceptingConnections = acceptingConnections;
    }

    public void setProtocolEngine(ProtocolEngine protocolEngine)
    {
        _protocolEngine = protocolEngine;
        if (_ioSession != null)
        {
            _ioSession.setAttachment(protocolEngine);
        }
    }

}
