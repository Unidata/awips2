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
import org.apache.mina.common.SimpleByteBufferAllocator;

import javax.jms.JMSException;
import javax.jms.Session;
import javax.jms.ObjectMessage;
import javax.jms.StreamMessage;
import javax.jms.BytesMessage;
import javax.jms.TextMessage;
import javax.jms.DeliveryMode;
import javax.jms.Destination;

public class TestMessageFactory
{
    private static final String MESSAGE_DATA_BYTES = "-message payload-message paylaod-message payload-message paylaod";

    public static TextMessage newTextMessage(Session session, int size) throws JMSException
    {
        return session.createTextMessage(createMessagePayload(size));
    }

    public static TextMessage newJMSTextMessage(Session session, int size, String encoding) throws JMSException
    {

        TextMessage message = session.createTextMessage();
        message.clearBody();
        message.setText(createMessagePayload(size));
        return message;
    }

    public static BytesMessage newBytesMessage(Session session, int size) throws JMSException
    {
        BytesMessage message = session.createBytesMessage();
        message.writeUTF(createMessagePayload(size));
        return message;
    }

    public static StreamMessage newStreamMessage(Session session, int size) throws JMSException
    {
        StreamMessage message = session.createStreamMessage();
        message.writeString(createMessagePayload(size));
        return message;
    }

    public static ObjectMessage newObjectMessage(Session session, int size) throws JMSException
    {
        if (size == 0)
        {
            return session.createObjectMessage();
        }
        else
        {
            return session.createObjectMessage(createMessagePayload(size));
        }
    }

    /**
     * Creates an ObjectMessage with given size and sets the JMS properties (JMSReplyTo and DeliveryMode)
     * @param session
     * @param replyDestination
     * @param size
     * @param persistent
     * @return the new ObjectMessage
     * @throws JMSException
     */
    public static ObjectMessage newObjectMessage(Session session, Destination replyDestination, int size, boolean persistent) throws JMSException
    {
        ObjectMessage msg = newObjectMessage(session, size);

        // Set the messages persistent delivery flag.
        msg.setJMSDeliveryMode(persistent ? DeliveryMode.PERSISTENT : DeliveryMode.NON_PERSISTENT);

        // Ensure that the temporary reply queue is set as the reply to destination for the message.
        if (replyDestination != null)
        {
            msg.setJMSReplyTo(replyDestination);
        }

        return msg;
    }

    public static String createMessagePayload(int size)
    {
        StringBuffer buf = new StringBuffer(size);
        int count = 0;
        while (count <= (size - MESSAGE_DATA_BYTES.length()))
        {
            buf.append(MESSAGE_DATA_BYTES);
            count += MESSAGE_DATA_BYTES.length();
        }
        if (count < size)
        {
            buf.append(MESSAGE_DATA_BYTES, 0, size - count);
        }

        return buf.toString();
    }
}
