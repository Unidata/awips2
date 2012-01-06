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
package org.apache.qpid.server.store;

import org.apache.qpid.server.message.MessageMetaData;
import org.apache.qpid.framing.abstraction.ContentChunk;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.List;
import java.nio.ByteBuffer;

/**
 * Adds some extra methods to the memory message store for testing purposes.
 */
public class TestMemoryMessageStore extends MemoryMessageStore
{
    private AtomicInteger _messageCount = new AtomicInteger(0);


    public TestMemoryMessageStore()
    {
    }

    @Override
    public StoredMessage addMessage(StorableMessageMetaData metaData)
    {
        return new TestableStoredMessage(super.addMessage(metaData));
    }

    public int getMessageCount()
    {
        return _messageCount.get();
    }

    private class TestableStoredMessage implements StoredMessage
    {
        private final StoredMessage _storedMessage;

        public TestableStoredMessage(StoredMessage storedMessage)
        {
            _messageCount.incrementAndGet();
            _storedMessage = storedMessage;
        }

        public StorableMessageMetaData getMetaData()
        {
            return _storedMessage.getMetaData();
        }

        public long getMessageNumber()
        {
            return _storedMessage.getMessageNumber();
        }

        public void addContent(int offsetInMessage, ByteBuffer src)
        {
            _storedMessage.addContent(offsetInMessage, src);
        }

        public int getContent(int offsetInMessage, ByteBuffer dst)
        {
            return _storedMessage.getContent(offsetInMessage, dst);
        }

        public StoreFuture flushToStore()
        {
            return _storedMessage.flushToStore();
        }

        public void remove()
        {
            _storedMessage.remove();
            _messageCount.decrementAndGet();
        }

    }
    
}
