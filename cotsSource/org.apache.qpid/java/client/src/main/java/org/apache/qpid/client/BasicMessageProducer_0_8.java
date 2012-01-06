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
package org.apache.qpid.client;

import java.util.UUID;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Topic;
import javax.jms.Queue;

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.client.message.AbstractJMSMessage;
import org.apache.qpid.client.message.AMQMessageDelegate;
import org.apache.qpid.client.message.AMQMessageDelegate_0_8;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.framing.AMQFrame;
import org.apache.qpid.framing.BasicConsumeBody;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.BasicPublishBody;
import org.apache.qpid.framing.CompositeAMQDataBlock;
import org.apache.qpid.framing.ContentBody;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.ExchangeDeclareBody;

public class BasicMessageProducer_0_8 extends BasicMessageProducer
{

    BasicMessageProducer_0_8(AMQConnection connection, AMQDestination destination, boolean transacted, int channelId,
            AMQSession session, AMQProtocolHandler protocolHandler, long producerId, boolean immediate, boolean mandatory,
            boolean waitUntilSent)
    {
        super(connection, destination,transacted,channelId,session, protocolHandler, producerId, immediate, mandatory,waitUntilSent);
    }

    void declareDestination(AMQDestination destination)
    {

        ExchangeDeclareBody body = getSession().getMethodRegistry().createExchangeDeclareBody(_session.getTicket(),
                                                                                              destination.getExchangeName(),
                                                                                              destination.getExchangeClass(),
                                                                                              false,
                                                                                              false,
                                                                                              false,
                                                                                              false,
                                                                                              true,
                                                                                              null);
        // Declare the exchange
        // Note that the durable and internal arguments are ignored since passive is set to false

        AMQFrame declare = body.generateFrame(_channelId);

        _protocolHandler.writeFrame(declare);
    }

    void sendMessage(AMQDestination destination, Message origMessage, AbstractJMSMessage message,
                     UUID messageId, int deliveryMode,int priority, long timeToLive, boolean mandatory,
                     boolean immediate, boolean wait) throws JMSException
    {
        BasicPublishBody body = getSession().getMethodRegistry().createBasicPublishBody(_session.getTicket(),
                                                                                        destination.getExchangeName(),
                                                                                        destination.getRoutingKey(),
                                                                                        mandatory,
                                                                                        immediate);

        AMQFrame publishFrame = body.generateFrame(_channelId);

        message.prepareForSending();
        ByteBuffer payload = message.getData();
        AMQMessageDelegate_0_8 delegate = (AMQMessageDelegate_0_8) message.getDelegate();
        BasicContentHeaderProperties contentHeaderProperties = delegate.getContentHeaderProperties();

        contentHeaderProperties.setUserId(_userID);

        //Set the JMS_QPID_DESTTYPE for 0-8/9 messages
        int type;
        if (destination instanceof Topic)
        {
            type = AMQDestination.TOPIC_TYPE;
        }
        else if (destination instanceof Queue)
        {
            type = AMQDestination.QUEUE_TYPE;
        }
        else
        {
            type = AMQDestination.UNKNOWN_TYPE;
        }

        //Set JMS_QPID_DESTTYPE
        delegate.getContentHeaderProperties().getHeaders().setInteger(CustomJMSXProperty.JMS_QPID_DESTTYPE.getShortStringName(), type);

        if (!_disableTimestamps)
        {
            final long currentTime = System.currentTimeMillis();
            contentHeaderProperties.setTimestamp(currentTime);

            if (timeToLive > 0)
            {
                contentHeaderProperties.setExpiration(currentTime + timeToLive);
            }
            else
            {
                contentHeaderProperties.setExpiration(0);
            }
        }

        contentHeaderProperties.setDeliveryMode((byte) deliveryMode);
        contentHeaderProperties.setPriority((byte) priority);

        final int size = (payload != null) ? payload.limit() : 0;
        final int contentBodyFrameCount = calculateContentBodyFrameCount(payload);
        final AMQFrame[] frames = new AMQFrame[2 + contentBodyFrameCount];

        if (payload != null)
        {
            createContentBodies(payload, frames, 2, _channelId);
        }

        if ((contentBodyFrameCount != 0) && _logger.isDebugEnabled())
        {
            _logger.debug("Sending content body frames to " + destination);
        }


        // TODO: This is a hacky way of getting the AMQP class-id for the Basic class
        int classIfForBasic = getSession().getMethodRegistry().createBasicQosOkBody().getClazz();

        AMQFrame contentHeaderFrame =
            ContentHeaderBody.createAMQFrame(_channelId,
                                             classIfForBasic, 0, contentHeaderProperties, size);
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Sending content header frame to " + destination);
        }

        frames[0] = publishFrame;
        frames[1] = contentHeaderFrame;
        CompositeAMQDataBlock compositeFrame = new CompositeAMQDataBlock(frames);

        try
        {
            _session.checkFlowControl();
        }
        catch (InterruptedException e)
        {
            JMSException jmsEx = new JMSException("Interrupted while waiting for flow control to be removed");
            jmsEx.setLinkedException(e);
            throw jmsEx;
        }

        _protocolHandler.writeFrame(compositeFrame, wait);
    }

    /**
     * Create content bodies. This will split a large message into numerous bodies depending on the negotiated
     * maximum frame size.
     *
     * @param payload
     * @param frames
     * @param offset
     * @param channelId @return the array of content bodies
     */
    private void createContentBodies(ByteBuffer payload, AMQFrame[] frames, int offset, int channelId)
    {

        if (frames.length == (offset + 1))
        {
            frames[offset] = ContentBody.createAMQFrame(channelId, new ContentBody(payload));
        }
        else
        {

            final long framePayloadMax = _session.getAMQConnection().getMaximumFrameSize() - 1;
            long remaining = payload.remaining();
            for (int i = offset; i < frames.length; i++)
            {
                payload.position((int) framePayloadMax * (i - offset));
                int length = (remaining >= framePayloadMax) ? (int) framePayloadMax : (int) remaining;
                payload.limit(payload.position() + length);
                frames[i] = ContentBody.createAMQFrame(channelId, new ContentBody(payload.slice()));

                remaining -= length;
            }
        }

    }

    private int calculateContentBodyFrameCount(ByteBuffer payload)
    {
        // we substract one from the total frame maximum size to account for the end of frame marker in a body frame
        // (0xCE byte).
        int frameCount;
        if ((payload == null) || (payload.remaining() == 0))
        {
            frameCount = 0;
        }
        else
        {
            int dataLength = payload.remaining();
            final long framePayloadMax = _session.getAMQConnection().getMaximumFrameSize() - 1;
            int lastFrame = ((dataLength % framePayloadMax) > 0) ? 1 : 0;
            frameCount = (int) (dataLength / framePayloadMax) + lastFrame;
        }

        return frameCount;
    }

}
