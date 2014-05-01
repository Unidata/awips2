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
package org.apache.qpid.server.queue;

import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.server.store.TransactionLog;
import org.apache.qpid.server.store.StoredMessage;
import org.apache.qpid.server.message.MessageMetaData;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;

import java.nio.ByteBuffer;

public class MockStoredMessage implements StoredMessage<MessageMetaData>
{
    private long _messageId;
    private MessageMetaData _metaData;
    private final ByteBuffer _content;


    public MockStoredMessage(long messageId)
    {
        this(messageId, new MockMessagePublishInfo(), new ContentHeaderBody(new BasicContentHeaderProperties(), 60));
    }

    public MockStoredMessage(long messageId, MessagePublishInfo info, ContentHeaderBody chb)
    {
        _messageId = messageId;
        _metaData = new MessageMetaData(info, chb, 0);
        _content = ByteBuffer.allocate(_metaData.getContentSize());

    }

    public MessageMetaData getMetaData()
    {
        return _metaData;
    }

    public long getMessageNumber()
    {
        return _messageId;
    }

    public void addContent(int offsetInMessage, ByteBuffer src)
    {
        src = src.duplicate();
        ByteBuffer dst = _content.duplicate();
        dst.position(offsetInMessage);
        dst.put(src);
    }

    public int getContent(int offset, ByteBuffer dst)
    {
        ByteBuffer src = _content.duplicate();
        src.position(offset);
        src = src.slice();
        if(dst.remaining() < src.limit())
        {
            src.limit(dst.remaining());
        }
        dst.put(src);
        return src.limit();
    }

    public TransactionLog.StoreFuture flushToStore()
    {
        return MessageStore.IMMEDIATE_FUTURE;
    }

    public void remove()
    {
    }
}
