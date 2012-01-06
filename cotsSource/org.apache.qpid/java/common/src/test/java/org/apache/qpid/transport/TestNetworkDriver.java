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
package org.apache.qpid.transport;

import java.net.BindException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.qpid.protocol.ProtocolEngine;
import org.apache.qpid.protocol.ProtocolEngineFactory;
import org.apache.qpid.ssl.SSLContextFactory;

/**
 * Test implementation of IoSession, which is required for some tests. Methods not being used are not implemented,
 * so if this class is being used and some methods are to be used, then please update those.
 */
public class TestNetworkDriver implements NetworkDriver
{
    private final ConcurrentMap attributes = new ConcurrentHashMap();
    private String _remoteAddress = "127.0.0.1";
    private String _localAddress = "127.0.0.1";
    private int _port = 1;

    public TestNetworkDriver()
    {
    }

    public void setRemoteAddress(String string)
    {
        this._remoteAddress = string;
    }

    public void setPort(int _port)
    {
        this._port = _port;
    }

    public int getPort()
    {
        return _port;
    }

    public void bind(int port, InetAddress[] addresses, ProtocolEngineFactory protocolFactory,
            NetworkDriverConfiguration config, SSLContextFactory sslFactory) throws BindException
    {
        
    }

    public SocketAddress getLocalAddress()
    {
        return new InetSocketAddress(_localAddress, _port);
    }

    public SocketAddress getRemoteAddress()
    {
        return new InetSocketAddress(_remoteAddress, _port);
    }

    public void open(int port, InetAddress destination, ProtocolEngine engine, NetworkDriverConfiguration config,
            SSLContextFactory sslFactory) throws OpenException
    {
        
    }

    public void setMaxReadIdle(int idleTime)
    {
        
    }

    public void setMaxWriteIdle(int idleTime)
    {
        
    }

    public void close()
    {
           
    }

    public void flush()
    {
        
    }

    public void send(ByteBuffer msg)
    {
        
    }

    public void setIdleTimeout(long l)
    {
        
    }

    public void setLocalAddress(String localAddress)
    {
        _localAddress = localAddress;
    }

}
