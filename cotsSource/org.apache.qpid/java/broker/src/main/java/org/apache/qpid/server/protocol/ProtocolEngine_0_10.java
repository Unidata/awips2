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
package org.apache.qpid.server.protocol;

import org.apache.qpid.protocol.ProtocolEngine;
import org.apache.qpid.transport.NetworkDriver;
import org.apache.qpid.transport.Connection;
import org.apache.qpid.transport.network.InputHandler;
import org.apache.qpid.transport.network.Assembler;
import org.apache.qpid.transport.network.Disassembler;

import java.net.SocketAddress;

public class ProtocolEngine_0_10  extends InputHandler implements ProtocolEngine
{
    public static final int MAX_FRAME_SIZE = 64 * 1024 - 1;

    private NetworkDriver _networkDriver;
    private long _readBytes;
    private long _writtenBytes;
    private Connection _connection;

    public ProtocolEngine_0_10(Connection conn, NetworkDriver networkDriver)
    {
        super(new Assembler(conn));
        _connection = conn;
        _networkDriver = networkDriver;
    }

    public void setNetworkDriver(NetworkDriver driver)
    {
        _networkDriver = driver;
        Disassembler dis = new Disassembler(driver, MAX_FRAME_SIZE);
        _connection.setSender(dis);
    }

    public SocketAddress getRemoteAddress()
    {
        return _networkDriver.getRemoteAddress();
    }

    public SocketAddress getLocalAddress()
    {
        return _networkDriver.getLocalAddress();
    }

    public long getReadBytes()
    {
        return _readBytes;
    }

    public long getWrittenBytes()
    {
        return _writtenBytes;
    }

    public void writerIdle()
    {
        //Todo
    }

    public void readerIdle()
    {
        //Todo
    }
}
