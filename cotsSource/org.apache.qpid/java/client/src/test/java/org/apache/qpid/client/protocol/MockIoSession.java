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
package org.apache.qpid.client.protocol;

import org.apache.mina.common.*;
import org.apache.mina.common.support.DefaultCloseFuture;
import org.apache.mina.common.support.DefaultWriteFuture;
import org.apache.mina.common.support.AbstractIoFilterChain;
import org.apache.qpid.client.protocol.AMQProtocolSession;

import java.net.SocketAddress;
import java.net.InetSocketAddress;
import java.util.Set;

public class MockIoSession implements IoSession
{
    private AMQProtocolSession _protocolSession;

    /**
     * Stores the last response written
     */
    private Object _lastWrittenObject;

    private boolean _closing;
    private IoFilterChain _filterChain;

    public MockIoSession()
    {
        _filterChain = new AbstractIoFilterChain(this)
        {
            protected void doWrite(IoSession ioSession, IoFilter.WriteRequest writeRequest) throws Exception
            {

            }

            protected void doClose(IoSession ioSession) throws Exception
            {

            }
        };
    }

    public Object getLastWrittenObject()
    {
        return _lastWrittenObject;
    }

    public IoService getService()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public IoServiceConfig getServiceConfig()
    {
        return null;
    }

    public IoHandler getHandler()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public IoSessionConfig getConfig()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public IoFilterChain getFilterChain()
    {
        return _filterChain;
    }

    public WriteFuture write(Object message)
    {
        WriteFuture wf = new DefaultWriteFuture(null);
        _lastWrittenObject = message;
        return wf;
    }

    public CloseFuture close()
    {
        _closing = true;
        CloseFuture cf = new DefaultCloseFuture(null);
        cf.setClosed();
        return cf;
    }

    public Object getAttachment()
    {
        return _protocolSession;
    }

    public Object setAttachment(Object attachment)
    {
        Object current = _protocolSession;
        _protocolSession = (AMQProtocolSession) attachment;
        return current;
    }

    public Object getAttribute(String key)
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public Object setAttribute(String key, Object value)
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public Object setAttribute(String key)
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public Object removeAttribute(String key)
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean containsAttribute(String key)
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public Set getAttributeKeys()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public TransportType getTransportType()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isConnected()
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isClosing()
    {
        return _closing;
    }

    public CloseFuture getCloseFuture()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public SocketAddress getRemoteAddress()
    {
        return new InetSocketAddress("127.0.0.1", 1234);  //To change body of implemented methods use File | Settings | File Templates.
    }

    public SocketAddress getLocalAddress()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public SocketAddress getServiceAddress()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getIdleTime(IdleStatus status)
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getIdleTimeInMillis(IdleStatus status)
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setIdleTime(IdleStatus status, int idleTime)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getWriteTimeout()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getWriteTimeoutInMillis()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setWriteTimeout(int writeTimeout)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public TrafficMask getTrafficMask()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setTrafficMask(TrafficMask trafficMask)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void suspendRead()
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void suspendWrite()
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void resumeRead()
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void resumeWrite()
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getReadBytes()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getWrittenBytes()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getReadMessages()
    {
        return 0L;
    }

    public long getWrittenMessages()
    {
        return 0L;
    }

    public long getWrittenWriteRequests()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getScheduledWriteRequests()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getScheduledWriteBytes()
    {
        return 0;  //TODO
    }

    public long getCreationTime()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getLastIoTime()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getLastReadTime()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getLastWriteTime()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isIdle(IdleStatus status)
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getIdleCount(IdleStatus status)
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getLastIdleTime(IdleStatus status)
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
