package org.apache.qpid.server.output.amqp0_9_1;
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


import org.apache.mina.common.ByteBuffer;

import org.apache.qpid.server.output.ProtocolOutputConverter;
import org.apache.qpid.server.output.HeaderPropertiesConverter;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.qpid.server.message.MessageContentSource;
import org.apache.qpid.server.message.MessageTransferMessage;
import org.apache.qpid.framing.*;
import org.apache.qpid.framing.amqp_0_91.BasicGetBodyImpl;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.framing.abstraction.ProtocolVersionMethodConverter;
import org.apache.qpid.AMQException;
import org.apache.qpid.transport.DeliveryProperties;
import org.apache.qpid.protocol.AMQVersionAwareProtocolSession;

public class ProtocolOutputConverterImpl implements ProtocolOutputConverter
{
    private static final MethodRegistry METHOD_REGISTRY = MethodRegistry.getMethodRegistry(ProtocolVersion.v0_91);
    private static final ProtocolVersionMethodConverter
            PROTOCOL_CONVERTER = METHOD_REGISTRY.getProtocolVersionMethodConverter();


    public static Factory getInstanceFactory()
    {
        return new Factory()
        {

            public ProtocolOutputConverter newInstance(AMQProtocolSession session)
            {
                return new ProtocolOutputConverterImpl(session);
            }
        };
    }

    private final AMQProtocolSession _protocolSession;

    private ProtocolOutputConverterImpl(AMQProtocolSession session)
    {
        _protocolSession = session;
    }


    public AMQProtocolSession getProtocolSession()
    {
        return _protocolSession;
    }

    public void writeDeliver(QueueEntry entry, int channelId, long deliveryTag, AMQShortString consumerTag)
            throws AMQException
    {
        AMQBody deliverBody = createEncodedDeliverBody(entry, deliveryTag, consumerTag);
        writeMessageDelivery(entry, channelId, deliverBody);
    }


    private ContentHeaderBody getContentHeaderBody(QueueEntry entry)
            throws AMQException
    {
        if(entry.getMessage() instanceof AMQMessage)
        {
            return ((AMQMessage)entry.getMessage()).getContentHeaderBody();
        }
        else
        {
            final MessageTransferMessage message = (MessageTransferMessage) entry.getMessage();
            BasicContentHeaderProperties props = HeaderPropertiesConverter.convert(message);
            ContentHeaderBody chb = new ContentHeaderBody(props, BasicGetBodyImpl.CLASS_ID);
            chb.bodySize = message.getSize();
            return chb;
        }
    }


    private void writeMessageDelivery(QueueEntry entry, int channelId, AMQBody deliverBody)
            throws AMQException
    {
        writeMessageDelivery(entry.getMessage(), getContentHeaderBody(entry), channelId, deliverBody);
    }

    private void writeMessageDelivery(MessageContentSource message, ContentHeaderBody contentHeaderBody, int channelId, AMQBody deliverBody)
            throws AMQException
    {


        int bodySize = (int) message.getSize();

        if(bodySize == 0)
        {
            SmallCompositeAMQBodyBlock compositeBlock = new SmallCompositeAMQBodyBlock(channelId, deliverBody,
                                                                             contentHeaderBody);

            writeFrame(compositeBlock);
        }
        else
        {
            int maxBodySize = (int) getProtocolSession().getMaxFrameSize() - AMQFrame.getFrameOverhead();


            final int capacity = bodySize > maxBodySize ? maxBodySize : bodySize;
            java.nio.ByteBuffer buf = java.nio.ByteBuffer.allocate(capacity);

            int writtenSize = 0;


            writtenSize += message.getContent(buf, writtenSize);
            buf.flip();
            AMQBody firstContentBody = PROTOCOL_CONVERTER.convertToBody(buf);

            CompositeAMQBodyBlock
                    compositeBlock = new CompositeAMQBodyBlock(channelId, deliverBody, contentHeaderBody, firstContentBody);
            writeFrame(compositeBlock);

            while(writtenSize < bodySize)
            {
                buf = java.nio.ByteBuffer.allocate(capacity);

                writtenSize += message.getContent(buf, writtenSize);
                buf.flip();
                writeFrame(new AMQFrame(channelId, PROTOCOL_CONVERTER.convertToBody(buf)));
            }
        }
    }

    private AMQDataBlock createContentHeaderBlock(final int channelId, final ContentHeaderBody contentHeaderBody)
    {

        AMQDataBlock contentHeader = ContentHeaderBody.createAMQFrame(channelId,
                                                                      contentHeaderBody);
        return contentHeader;
    }


    public void writeGetOk(QueueEntry entry, int channelId, long deliveryTag, int queueSize) throws AMQException
    {
        AMQBody deliver = createEncodedGetOkBody(entry, deliveryTag, queueSize);
        writeMessageDelivery(entry, channelId, deliver);
    }


    private AMQBody createEncodedDeliverBody(QueueEntry entry,
                                              final long deliveryTag,
                                              final AMQShortString consumerTag)
            throws AMQException
    {

        final AMQShortString exchangeName;
        final AMQShortString routingKey;

        if(entry.getMessage() instanceof AMQMessage)
        {
            final AMQMessage message = (AMQMessage) entry.getMessage();
            final MessagePublishInfo pb = message.getMessagePublishInfo();
            exchangeName = pb.getExchange();
            routingKey = pb.getRoutingKey();
        }
        else
        {
            MessageTransferMessage message = (MessageTransferMessage) entry.getMessage();
            DeliveryProperties delvProps = message.getHeader().get(DeliveryProperties.class);
            exchangeName = (delvProps == null || delvProps.getExchange() == null) ? null : new AMQShortString(delvProps.getExchange());
            routingKey = (delvProps == null || delvProps.getRoutingKey() == null) ? null : new AMQShortString(delvProps.getRoutingKey());
        }

        final boolean isRedelivered = entry.isRedelivered();

        final AMQBody returnBlock = new AMQBody()
        {

            public AMQBody _underlyingBody;

            public AMQBody createAMQBody()
            {
                return METHOD_REGISTRY.createBasicDeliverBody(consumerTag,
                                                              deliveryTag,
                                                              isRedelivered,
                                                              exchangeName,
                                                              routingKey);





            }

            public byte getFrameType()
            {
                return AMQMethodBody.TYPE;
            }

            public int getSize()
            {
                if(_underlyingBody == null)
                {
                    _underlyingBody = createAMQBody();
                }
                return _underlyingBody.getSize();
            }

            public void writePayload(ByteBuffer buffer)
            {
                if(_underlyingBody == null)
                {
                    _underlyingBody = createAMQBody();
                }
                _underlyingBody.writePayload(buffer);
            }

            public void handle(final int channelId, final AMQVersionAwareProtocolSession amqMinaProtocolSession)
                throws AMQException
            {
                throw new AMQException("This block should never be dispatched!");
            }
        };
        return returnBlock;
    }

    private AMQBody createEncodedGetOkBody(QueueEntry entry, long deliveryTag, int queueSize)
            throws AMQException
    {
        final AMQShortString exchangeName;
        final AMQShortString routingKey;

        if(entry.getMessage() instanceof AMQMessage)
        {
            final AMQMessage message = (AMQMessage) entry.getMessage();
            final MessagePublishInfo pb = message.getMessagePublishInfo();
            exchangeName = pb.getExchange();
            routingKey = pb.getRoutingKey();
        }
        else
        {
            MessageTransferMessage message = (MessageTransferMessage) entry.getMessage();
            DeliveryProperties delvProps = message.getHeader().get(DeliveryProperties.class);
            exchangeName = (delvProps == null || delvProps.getExchange() == null) ? null : new AMQShortString(delvProps.getExchange());
            routingKey = (delvProps == null || delvProps.getRoutingKey() == null) ? null : new AMQShortString(delvProps.getRoutingKey());
        }

        final boolean isRedelivered = entry.isRedelivered();

        BasicGetOkBody getOkBody =
                METHOD_REGISTRY.createBasicGetOkBody(deliveryTag,
                                                    isRedelivered,
                                                    exchangeName,
                                                    routingKey,
                                                    queueSize);

        return getOkBody;
    }

    public byte getProtocolMinorVersion()
    {
        return getProtocolSession().getProtocolMinorVersion();
    }

    public byte getProtocolMajorVersion()
    {
        return getProtocolSession().getProtocolMajorVersion();
    }

    private AMQBody createEncodedReturnFrame(MessagePublishInfo messagePublishInfo,
                                             int replyCode,
                                             AMQShortString replyText) throws AMQException
    {

        BasicReturnBody basicReturnBody =
                METHOD_REGISTRY.createBasicReturnBody(replyCode,
                                                     replyText,
                                                     messagePublishInfo.getExchange(),
                                                     messagePublishInfo.getRoutingKey());


        return basicReturnBody;
    }

    public void writeReturn(MessagePublishInfo messagePublishInfo, ContentHeaderBody header, MessageContentSource message, int channelId, int replyCode, AMQShortString replyText)
            throws AMQException
    {

        AMQBody returnFrame = createEncodedReturnFrame(messagePublishInfo, replyCode, replyText);

        writeMessageDelivery(message, header, channelId, returnFrame);
    }


    public void writeFrame(AMQDataBlock block)
    {
        getProtocolSession().writeFrame(block);
    }


    public void confirmConsumerAutoClose(int channelId, AMQShortString consumerTag)
    {

        BasicCancelOkBody basicCancelOkBody = METHOD_REGISTRY.createBasicCancelOkBody(consumerTag);
        writeFrame(basicCancelOkBody.generateFrame(channelId));

    }


    public static final class CompositeAMQBodyBlock extends AMQDataBlock
    {
        public static final int OVERHEAD = 3 * AMQFrame.getFrameOverhead();

        private final AMQBody _methodBody;
        private final AMQBody _headerBody;
        private final AMQBody _contentBody;
        private final int _channel;


        public CompositeAMQBodyBlock(int channel, AMQBody methodBody, AMQBody headerBody, AMQBody contentBody)
        {
            _channel = channel;
            _methodBody = methodBody;
            _headerBody = headerBody;
            _contentBody = contentBody;

        }

        public long getSize()
        {
            return OVERHEAD + _methodBody.getSize() + _headerBody.getSize() + _contentBody.getSize();
        }

        public void writePayload(ByteBuffer buffer)
        {
            AMQFrame.writeFrames(buffer, _channel, _methodBody, _headerBody, _contentBody);
        }
    }

    public static final class SmallCompositeAMQBodyBlock extends AMQDataBlock
    {
        public static final int OVERHEAD = 2 * AMQFrame.getFrameOverhead();

        private final AMQBody _methodBody;
        private final AMQBody _headerBody;
        private final int _channel;


        public SmallCompositeAMQBodyBlock(int channel, AMQBody methodBody, AMQBody headerBody)
        {
            _channel = channel;
            _methodBody = methodBody;
            _headerBody = headerBody;

        }

        public long getSize()
        {
            return OVERHEAD + _methodBody.getSize() + _headerBody.getSize() ;
        }

        public void writePayload(ByteBuffer buffer)
        {
            AMQFrame.writeFrames(buffer, _channel, _methodBody, _headerBody);
        }
    }

}