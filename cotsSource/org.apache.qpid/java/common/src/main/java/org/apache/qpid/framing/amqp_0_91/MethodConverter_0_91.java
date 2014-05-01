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

package org.apache.qpid.framing.amqp_0_91;

import org.apache.mina.common.ByteBuffer;

import org.apache.qpid.framing.abstraction.AbstractMethodConverter;
import org.apache.qpid.framing.abstraction.ProtocolVersionMethodConverter;
import org.apache.qpid.framing.abstraction.ContentChunk;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.framing.abstraction.MessagePublishInfoImpl;
import org.apache.qpid.framing.*;

public class MethodConverter_0_91 extends AbstractMethodConverter implements ProtocolVersionMethodConverter
{
    private int _basicPublishClassId;
    private int _basicPublishMethodId;

    public MethodConverter_0_91()
    {
        super((byte)0,(byte)9);


    }

    public AMQBody convertToBody(ContentChunk contentChunk)
    {
        if(contentChunk instanceof ContentChunk_0_9)
        {
            return ((ContentChunk_0_9)contentChunk).toBody();
        }
        else
        {
            return new ContentBody(contentChunk.getData());
        }
    }

    public ContentChunk convertToContentChunk(AMQBody body)
    {
        final ContentBody contentBodyChunk = (ContentBody) body;

        return new ContentChunk_0_9(contentBodyChunk);

    }

    public void configure()
    {

        _basicPublishClassId = BasicPublishBodyImpl.CLASS_ID;
        _basicPublishMethodId = BasicPublishBodyImpl.METHOD_ID;

    }

    public AMQBody convertToBody(java.nio.ByteBuffer buf)
    {
        return new ContentBody(ByteBuffer.wrap(buf));
    }

    public MessagePublishInfo convertToInfo(AMQMethodBody methodBody)
    {
        final BasicPublishBody publishBody = ((BasicPublishBody) methodBody);

        final AMQShortString exchange = publishBody.getExchange();
        final AMQShortString routingKey = publishBody.getRoutingKey();

        return new MessagePublishInfoImpl(exchange,
                                          publishBody.getImmediate(),
                                          publishBody.getMandatory(),
                                          routingKey);

    }

    public AMQMethodBody convertToBody(MessagePublishInfo info)
    {

        return new BasicPublishBodyImpl(0,
                                    info.getExchange(),
                                    info.getRoutingKey(),
                                    info.isMandatory(),
                                    info.isImmediate()) ;

    }

    private static class ContentChunk_0_9 implements ContentChunk
    {
        private final ContentBody _contentBodyChunk;

        public ContentChunk_0_9(final ContentBody contentBodyChunk)
        {
            _contentBodyChunk = contentBodyChunk;
        }

        public int getSize()
        {
            return _contentBodyChunk.getSize();
        }

        public ByteBuffer getData()
        {
            return _contentBodyChunk.payload;
        }

        public void reduceToFit()
        {
            _contentBodyChunk.reduceBufferToFit();
        }

        public AMQBody toBody()
        {
            return _contentBodyChunk;
        }
    }
}