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
package org.apache.qpid.session;

import org.apache.mina.common.*;

import java.net.SocketAddress;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;

public class TestSession implements IoSession
{
    private final ConcurrentMap attributes = new ConcurrentHashMap();

    public TestSession()
    {
    }

    public IoService getService()
    {
        return null;  //TODO
    }

    public IoServiceConfig getServiceConfig()
    {
        return null;  //TODO
    }

    public IoHandler getHandler()
    {
        return null;  //TODO
    }

    public IoSessionConfig getConfig()
    {
        return null;  //TODO
    }

    public IoFilterChain getFilterChain()
    {
        return null;  //TODO
    }

    public WriteFuture write(Object message)
    {
        return null;  //TODO
    }

    public CloseFuture close()
    {
        return null;  //TODO
    }

    public Object getAttachment()
    {
        return getAttribute("");
    }

    public Object setAttachment(Object attachment)
    {
        return setAttribute("",attachment);
    }

    public Object getAttribute(String key)
    {
        return attributes.get(key);
    }

    public Object setAttribute(String key, Object value)
    {
        return attributes.put(key,value);
    }

    public Object setAttribute(String key)
    {
        return attributes.put(key, Boolean.TRUE);
    }

    public Object removeAttribute(String key)
    {
        return attributes.remove(key);
    }

    public boolean containsAttribute(String key)
    {
        return attributes.containsKey(key);
    }

    public Set getAttributeKeys()
    {
        return attributes.keySet();
    }

    public TransportType getTransportType()
    {
        return null;  //TODO
    }

    public boolean isConnected()
    {
        return false;  //TODO
    }

    public boolean isClosing()
    {
        return false;  //TODO
    }

    public CloseFuture getCloseFuture()
    {
        return null;  //TODO
    }

    public SocketAddress getRemoteAddress()
    {
        return null;  //TODO
    }

    public SocketAddress getLocalAddress()
    {
        return null;  //TODO
    }

    public SocketAddress getServiceAddress()
    {
        return null;  //TODO
    }

    public int getIdleTime(IdleStatus status)
    {
        return 0;  //TODO
    }

    public long getIdleTimeInMillis(IdleStatus status)
    {
        return 0;  //TODO
    }

    public void setIdleTime(IdleStatus status, int idleTime)
    {
        //TODO
    }

    public int getWriteTimeout()
    {
        return 0;  //TODO
    }

    public long getWriteTimeoutInMillis()
    {
        return 0;  //TODO
    }

    public void setWriteTimeout(int writeTimeout)
    {
        //TODO
    }

    public TrafficMask getTrafficMask()
    {
        return null;  //TODO
    }

    public void setTrafficMask(TrafficMask trafficMask)
    {
        //TODO
    }

    public void suspendRead()
    {
        //TODO
    }

    public void suspendWrite()
    {
        //TODO
    }

    public void resumeRead()
    {
        //TODO
    }

    public void resumeWrite()
    {
        //TODO
    }

    public long getReadBytes()
    {
        return 0;  //TODO
    }

    public long getWrittenBytes()
    {
        return 0;  //TODO
    }

    public long getReadMessages()
    {
        return 0;
    }

    public long getWrittenMessages()
    {
        return 0;
    }

    public long getWrittenWriteRequests()
    {
        return 0;  //TODO
    }

    public int getScheduledWriteRequests()
    {
        return 0;  //TODO
    }

    public int getScheduledWriteBytes()
    {
        return 0;  //TODO
    }

    public long getCreationTime()
    {
        return 0;  //TODO
    }

    public long getLastIoTime()
    {
        return 0;  //TODO
    }

    public long getLastReadTime()
    {
        return 0;  //TODO
    }

    public long getLastWriteTime()
    {
        return 0;  //TODO
    }

    public boolean isIdle(IdleStatus status)
    {
        return false;  //TODO
    }

    public int getIdleCount(IdleStatus status)
    {
        return 0;  //TODO
    }

    public long getLastIdleTime(IdleStatus status)
    {
        return 0;  //TODO
    }
}
