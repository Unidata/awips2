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
package org.apache.qpid.client.message;

import org.apache.mina.common.ByteBuffer;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ContentBody;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.transport.MessageProperties;
import org.apache.qpid.transport.DeliveryProperties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.JMSException;

import java.util.Iterator;
import java.util.List;

public abstract class AbstractJMSMessageFactory implements MessageFactory
{
    private static final Logger _logger = LoggerFactory.getLogger(AbstractJMSMessageFactory.class);

    protected AbstractJMSMessage create08MessageWithBody(long messageNbr, ContentHeaderBody contentHeader,
                                                         AMQShortString exchange, AMQShortString routingKey,
                                                         List bodies) throws AMQException
    {
        ByteBuffer data;
        final boolean debug = _logger.isDebugEnabled();

        // we optimise the non-fragmented case to avoid copying
        if ((bodies != null) && (bodies.size() == 1))
        {
            if (debug)
            {
                _logger.debug("Non-fragmented message body (bodySize=" + contentHeader.bodySize + ")");
            }

            data = ((ContentBody) bodies.get(0)).payload;
        }
        else if (bodies != null)
        {
            if (debug)
            {
                _logger.debug("Fragmented message body (" + bodies
                        .size() + " frames, bodySize=" + contentHeader.bodySize + ")");
            }

            data = ByteBuffer.allocate((int) contentHeader.bodySize); // XXX: Is cast a problem?
            final Iterator it = bodies.iterator();
            while (it.hasNext())
            {
                ContentBody cb = (ContentBody) it.next();
                final ByteBuffer payload = cb.payload;
                if(payload.isDirect() || payload.isReadOnly())
                {
                    data.put(payload);
                }
                else
                {
                    data.put(payload.array(), payload.arrayOffset(), payload.limit());
                }

                payload.release();
            }

            data.flip();
        }
        else // bodies == null
        {
            data = ByteBuffer.allocate(0);
        }

        if (debug)
        {
            _logger.debug("Creating message from buffer with position=" + data.position() + " and remaining=" + data
                    .remaining());
        }

        AMQMessageDelegate delegate = new AMQMessageDelegate_0_8(messageNbr,
                                                                 (BasicContentHeaderProperties) contentHeader.properties,
                                                                 exchange, routingKey);

        return createMessage(delegate, data);
    }

    protected abstract AbstractJMSMessage createMessage(AMQMessageDelegate delegate, ByteBuffer data) throws AMQException;


    protected AbstractJMSMessage create010MessageWithBody(long messageNbr, MessageProperties msgProps,
                                                          DeliveryProperties deliveryProps,  
                                                          java.nio.ByteBuffer body) throws AMQException
    {
        ByteBuffer data;
        final boolean debug = _logger.isDebugEnabled();


        if (body != null)
        {
            data = ByteBuffer.wrap(body);
        }
        else // body == null
        {
            data = ByteBuffer.allocate(0);
        }

        if (debug)
        {
            _logger.debug("Creating message from buffer with position=" + data.position() + " and remaining=" + data
                    .remaining());
        }
        AMQMessageDelegate delegate = new AMQMessageDelegate_0_10(msgProps, deliveryProps, messageNbr);

        AbstractJMSMessage message = createMessage(delegate, data);
        return message;
    }

    private static final String asString(byte[] bytes)
    {
        if (bytes == null)
        {
            return null;
        }
        else
        {
            return new String(bytes);
        }
    }


    public AbstractJMSMessage createMessage(long messageNbr, boolean redelivered, ContentHeaderBody contentHeader,
                                            AMQShortString exchange, AMQShortString routingKey, List bodies)
            throws JMSException, AMQException
    {
        final AbstractJMSMessage msg = create08MessageWithBody(messageNbr, contentHeader, exchange, routingKey, bodies);
        msg.setJMSRedelivered(redelivered);
        msg.receivedFromServer();
        return msg;
    }

    public AbstractJMSMessage createMessage(long messageNbr, boolean redelivered, MessageProperties msgProps,
                                            DeliveryProperties deliveryProps, java.nio.ByteBuffer body)
            throws JMSException, AMQException
    {
        final AbstractJMSMessage msg =
                create010MessageWithBody(messageNbr,msgProps,deliveryProps, body);
        msg.setJMSRedelivered(redelivered);
        msg.receivedFromServer();
        return msg;
    }

}
