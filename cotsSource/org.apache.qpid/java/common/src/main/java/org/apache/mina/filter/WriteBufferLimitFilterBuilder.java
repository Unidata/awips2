/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 */
package org.apache.mina.filter;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.DefaultIoFilterChainBuilder;
import org.apache.mina.common.IoFilterAdapter;
import org.apache.mina.common.IoFilterChain;
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.executor.ExecutorFilter;

import java.util.Iterator;
import java.util.List;

/**
 * This filter will turn the asynchronous filterWrite method in to a blocking send when there are more than
 * the prescribed number of messages awaiting filterWrite. It should be used in conjunction with the
 * {@link ReadThrottleFilterBuilder} on a server as the blocking writes will allow the read thread to
 * cause an Out of Memory exception due to a back log of unprocessed messages.
 *
 * This is should only be viewed as a temporary work around for DIRMINA-302.
 *
 * A true solution should not be implemented as a filter as this issue will always occur. On a machine
 * where the network is slower than the local producer.
 *
 * Suggested improvement is to allow implementation of policices on what to do when buffer is full.
 *
 * They could be:
 * Block - As this does
 * Wait on a given Future - to drain more of the queue.. in essence this filter with high/low watermarks
 * Throw Exception - through the client filterWrite() method to allow them to get immediate feedback on buffer state
 *
 * <p/>
 * <p>Usage:
 * <p/>
 * <pre><code>
 * DefaultFilterChainBuilder builder = ...
 * WriteBufferLimitFilterBuilder filter = new WriteBufferLimitFilterBuilder();
 * filter.attach( builder );
 * </code></pre>
 * <p/>
 * or
 * <p/>
 * <pre><code>
 * IoFilterChain chain = ...
 * WriteBufferLimitFilterBuilder filter = new WriteBufferLimitFilterBuilder();
 * filter.attach( chain );
 * </code></pre>
 *
 * @author The Apache Directory Project (mina-dev@directory.apache.org)
 * @version $Rev: 619823 $, $Date: 2008-02-08 10:09:37 +0000 (Fri, 08 Feb 2008) $
 */
public class WriteBufferLimitFilterBuilder
{
    public static final String PENDING_SIZE = WriteBufferLimitFilterBuilder.class.getName() + ".pendingSize";

    private static int DEFAULT_CONNECTION_BUFFER_MESSAGE_COUNT = 5000;

    private volatile boolean throwNotBlock = false;

    private volatile int maximumConnectionBufferCount;
    private volatile long maximumConnectionBufferSize;

    private final Object _blockLock = new Object();

    private int _blockWaiters = 0;


    public WriteBufferLimitFilterBuilder()
    {
        this(DEFAULT_CONNECTION_BUFFER_MESSAGE_COUNT);
    }

    public WriteBufferLimitFilterBuilder(int maxWriteBufferSize)
    {
        setMaximumConnectionBufferCount(maxWriteBufferSize);
    }


    /**
     * Set the maximum amount pending items in the writeQueue for a given session.
     * Changing the value will only take effect when new data is received for a
     * connection, including existing connections. Default value is 5000 msgs.
     *
     * @param maximumConnectionBufferCount New buffer size. Must be > 0
     */
    public void setMaximumConnectionBufferCount(int maximumConnectionBufferCount)
    {
        this.maximumConnectionBufferCount = maximumConnectionBufferCount;
        this.maximumConnectionBufferSize = 0;
    }

    public void setMaximumConnectionBufferSize(long maximumConnectionBufferSize)
    {
        this.maximumConnectionBufferSize = maximumConnectionBufferSize;
        this.maximumConnectionBufferCount = 0;
    }

    /**
     * Attach this filter to the specified filter chain. It will search for the ThreadPoolFilter, and attach itself
     * before and after that filter.
     *
     * @param chain {@link IoFilterChain} to attach self to.
     */
    public void attach(IoFilterChain chain)
    {
        String name = getThreadPoolFilterEntryName(chain.getAll());

        chain.addBefore(name, getClass().getName() + ".sendlimit", new SendLimit());
    }

    /**
     * Attach this filter to the specified builder. It will search for the
     * {@link ExecutorFilter}, and attach itself before and after that filter.
     *
     * @param builder {@link DefaultIoFilterChainBuilder} to attach self to.
     */
    public void attach(DefaultIoFilterChainBuilder builder)
    {
        String name = getThreadPoolFilterEntryName(builder.getAll());

        builder.addBefore(name, getClass().getName() + ".sendlimit", new SendLimit());
    }

    private String getThreadPoolFilterEntryName(List entries)
    {
        Iterator i = entries.iterator();

        while (i.hasNext())
        {
            IoFilterChain.Entry entry = (IoFilterChain.Entry) i.next();

            if (entry.getFilter().getClass().isAssignableFrom(ExecutorFilter.class))
            {
                return entry.getName();
            }
        }

        throw new IllegalStateException("Chain does not contain a ExecutorFilter");
    }


    public class SendLimit extends IoFilterAdapter
    {
        public void filterWrite(NextFilter nextFilter, IoSession session, WriteRequest writeRequest) throws Exception
        {
            try
            {
                waitTillSendAllowed(session);
            }
            catch (WriteBufferFullExeception wbfe)
            {
                nextFilter.exceptionCaught(session, wbfe);
            }

            if (writeRequest.getMessage() instanceof ByteBuffer)
            {
                increasePendingWriteSize(session, (ByteBuffer) writeRequest.getMessage());
            }

            nextFilter.filterWrite(session, writeRequest);
        }

        private void increasePendingWriteSize(IoSession session, ByteBuffer message)
        {
            synchronized (session)
            {
                Long pendingSize = getScheduledWriteBytes(session) + message.remaining();
                session.setAttribute(PENDING_SIZE, pendingSize);
            }
        }

        private boolean sendAllowed(IoSession session)
        {
            if (session.isClosing())
            {
                return true;
            }

            int lmswm = maximumConnectionBufferCount;
            long lmswb = maximumConnectionBufferSize;

            return (lmswm == 0 || session.getScheduledWriteRequests() < lmswm)
                   && (lmswb == 0 || getScheduledWriteBytes(session) < lmswb);
        }

        private long getScheduledWriteBytes(IoSession session)
        {
            synchronized (session)
            {
                Long i = (Long) session.getAttribute(PENDING_SIZE);
                return null == i ? 0 : i;
            }
        }

        private void waitTillSendAllowed(IoSession session)
        {
            synchronized (_blockLock)
            {
                if (throwNotBlock)
                {
                    throw new WriteBufferFullExeception();
                }

                _blockWaiters++;

                while (!sendAllowed(session))
                {
                    try
                    {
                        _blockLock.wait();
                    }
                    catch (InterruptedException e)
                    {
                        // Ignore.
                    }
                }
                _blockWaiters--;
            }
        }

        public void messageSent(NextFilter nextFilter, IoSession session, Object message) throws Exception
        {
            if (message instanceof ByteBuffer)
            {
                decrementPendingWriteSize(session, (ByteBuffer) message);
            }
            notifyWaitingWriters();
            nextFilter.messageSent(session, message);
        }

        private void decrementPendingWriteSize(IoSession session, ByteBuffer message)
        {
            synchronized (session)
            {
                session.setAttribute(PENDING_SIZE, getScheduledWriteBytes(session) - message.remaining());
            }
        }

        private void notifyWaitingWriters()
        {
            synchronized (_blockLock)
            {
                if (_blockWaiters != 0)
                {
                    _blockLock.notifyAll();
                }
            }

        }

    }//SentLimit


}
