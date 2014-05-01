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
package org.apache.qpid.test.utils.protocol;

import java.net.InetSocketAddress;
import java.net.SocketAddress;

import org.apache.mina.common.IoFilterChain;
import org.apache.mina.common.IoHandler;
import org.apache.mina.common.IoService;
import org.apache.mina.common.IoServiceConfig;
import org.apache.mina.common.IoSessionConfig;
import org.apache.mina.common.TransportType;
import org.apache.mina.common.support.BaseIoSession;

public class TestIoSession extends BaseIoSession {

    private String _stringLocalAddress;
    private int _localPort;

    public SocketAddress getLocalAddress()
    {
        //create a new address for testing purposes using member variables
        return new InetSocketAddress(_stringLocalAddress,_localPort);
    }

    protected void updateTrafficMask() {
       //dummy
    }

    public IoService getService() {
        return null;
    }

    public IoServiceConfig getServiceConfig() {
        return null;
    }

    public IoHandler getHandler() {
        return null;
    }

    public IoSessionConfig getConfig() {
        return null;
    }

    public IoFilterChain getFilterChain() {
        return null;
    }

    public TransportType getTransportType() {
        return null;
    }

    public SocketAddress getRemoteAddress() {
        return null;
    }

    public SocketAddress getServiceAddress() {
        return null;
    }

    public int getScheduledWriteRequests() {
        return 0;
    }

    public int getScheduledWriteBytes() {
        return 0;
    }

    public String getStringLocalAddress() {
        return _stringLocalAddress;
    }

    public void setStringLocalAddress(String _stringLocalAddress) {
        this._stringLocalAddress = _stringLocalAddress;
    }

    public int getLocalPort() {
        return _localPort;
    }

    public void setLocalPort(int _localPort) {
        this._localPort = _localPort;
    }
}
