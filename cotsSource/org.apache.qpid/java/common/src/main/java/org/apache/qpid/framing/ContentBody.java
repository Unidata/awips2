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
package org.apache.qpid.framing;

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.protocol.AMQVersionAwareProtocolSession;
import org.apache.qpid.AMQException;

public class ContentBody implements AMQBody
{
    public static final byte TYPE = 3;

    public ByteBuffer payload;

    public ContentBody()
    {
    }

    public ContentBody(ByteBuffer buffer, long size) throws AMQFrameDecodingException
    {
        if (size > 0)
        {
            payload = buffer.slice();
            payload.limit((int) size);
            buffer.skip((int) size);
        }

    }


    public ContentBody(ByteBuffer payload)
    {
        this.payload = payload;
    }

    public byte getFrameType()
    {
        return TYPE;
    }

    public int getSize()
    {
        return (payload == null ? 0 : payload.limit());
    }

    public void writePayload(ByteBuffer buffer)
    {
        if (payload != null)
        {
            if(payload.isDirect() || payload.isReadOnly())
            {            
                ByteBuffer copy = payload.duplicate();
                buffer.put(copy.rewind());
            }
            else
            {
                buffer.put(payload.array(),payload.arrayOffset(),payload.limit());
            }
        }
    }

    public void handle(final int channelId, final AMQVersionAwareProtocolSession session)
            throws AMQException
    {
        session.contentBodyReceived(channelId, this);
    }

    protected void populateFromBuffer(ByteBuffer buffer, long size) throws AMQFrameDecodingException
    {
        if (size > 0)
        {
            payload = buffer.slice();
            payload.limit((int) size);
            buffer.skip((int) size);
        }

    }

    public void reduceBufferToFit()
    {
        if (payload != null && (payload.remaining() < payload.capacity() / 2))
        {
            int size = payload.limit();
            ByteBuffer newPayload = ByteBuffer.allocate(size);

            newPayload.put(payload);
            newPayload.flip();

            //reduce reference count on payload
            payload.release();

            payload = newPayload;
        }
    }



    public static AMQFrame createAMQFrame(int channelId, ContentBody body)
    {
        final AMQFrame frame = new AMQFrame(channelId, body);
        return frame;
    }
}
