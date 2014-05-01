/* Licensed to the Apache Software Foundation (ASF) under one
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
 */
package org.apache.qpid.client;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.nio.ByteBuffer;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.DeliveryMode;

import org.apache.qpid.client.message.AbstractJMSMessage;
import org.apache.qpid.client.message.FiledTableSupport;
import org.apache.qpid.client.message.AMQMessageDelegate_0_10;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.url.AMQBindingURL;
import org.apache.qpid.util.Strings;
import org.apache.qpid.njms.ExceptionHelper;
import org.apache.qpid.transport.*;
import static org.apache.qpid.transport.Option.*;

/**
 * This is a 0_10 message producer.
 */
public class BasicMessageProducer_0_10 extends BasicMessageProducer
{
    private byte[] userIDBytes;

    BasicMessageProducer_0_10(AMQConnection connection, AMQDestination destination, boolean transacted, int channelId,
                              AMQSession session, AMQProtocolHandler protocolHandler, long producerId,
                              boolean immediate, boolean mandatory, boolean waitUntilSent)
    {
        super(connection, destination, transacted, channelId, session, protocolHandler, producerId, immediate,
              mandatory, waitUntilSent);
        
        userIDBytes = Strings.toUTF8(_userID);
    }

    void declareDestination(AMQDestination destination)
    {
        String name = destination.getExchangeName().toString();
        ((AMQSession_0_10) getSession()).getQpidSession().exchangeDeclare
            (name,
             destination.getExchangeClass().toString(),
             null, null,
             name.startsWith("amq.") ? Option.PASSIVE : Option.NONE);
    }

    //--- Overwritten methods

    /**
     * Sends a message to a given destination
     */
    void sendMessage(AMQDestination destination, Message origMessage, AbstractJMSMessage message,
                     UUID messageId, int deliveryMode, int priority, long timeToLive, boolean mandatory,
                     boolean immediate, boolean wait) throws JMSException
    {
        message.prepareForSending();

        AMQMessageDelegate_0_10 delegate = (AMQMessageDelegate_0_10) message.getDelegate();

        DeliveryProperties deliveryProp = delegate.getDeliveryProperties();
        MessageProperties messageProps = delegate.getMessageProperties();

        // On the receiving side, this will be read in to the JMSXUserID as well.
        messageProps.setUserId(userIDBytes);
                
        if (messageId != null)
        {
            messageProps.setMessageId(messageId);
        }
        else if (messageProps.hasMessageId())
        {
            messageProps.clearMessageId();
        }

        long currentTime = 0;
        if (timeToLive > 0 || !_disableTimestamps)
        {
            currentTime = System.currentTimeMillis();
        }        
        
        if (timeToLive > 0)
        {
            deliveryProp.setTtl(timeToLive);
            message.setJMSExpiration(currentTime + timeToLive);
        }
        
        if (!_disableTimestamps)
        {
            
            deliveryProp.setTimestamp(currentTime);            
            message.setJMSTimestamp(currentTime);
        }

        if (!deliveryProp.hasDeliveryMode() || deliveryProp.getDeliveryMode().getValue() != deliveryMode)
        {
            MessageDeliveryMode mode;
            switch (deliveryMode)
            {
            case DeliveryMode.PERSISTENT:
                mode = MessageDeliveryMode.PERSISTENT;
                break;
            case DeliveryMode.NON_PERSISTENT:
                mode = MessageDeliveryMode.NON_PERSISTENT;
                break;
            default:
                throw new IllegalArgumentException("illegal delivery mode: " + deliveryMode);
            }
            deliveryProp.setDeliveryMode(mode);
            message.setJMSDeliveryMode(deliveryMode);
        }
        if (!deliveryProp.hasPriority() || deliveryProp.getPriority().getValue() != priority)
        {
            deliveryProp.setPriority(MessageDeliveryPriority.get((short) priority));
            message.setJMSPriority(priority);
        }
        String exchangeName = destination.getExchangeName().toString();
        if ( deliveryProp.getExchange() == null || ! deliveryProp.getExchange().equals(exchangeName))
        {
            deliveryProp.setExchange(exchangeName);
        }
        String routingKey = destination.getRoutingKey().toString();
        if (deliveryProp.getRoutingKey() == null || ! deliveryProp.getRoutingKey().equals(routingKey))
        {
            deliveryProp.setRoutingKey(routingKey);
        }

        messageProps.setContentLength(message.getContentLength());

        // send the message
        try
        {
            org.apache.qpid.transport.Session ssn = (org.apache.qpid.transport.Session)
                ((AMQSession_0_10) getSession()).getQpidSession();

            // if true, we need to sync the delivery of this message
            boolean sync = false;

            sync = ( (publishMode == PublishMode.SYNC_PUBLISH_ALL) ||
                     (publishMode == PublishMode.SYNC_PUBLISH_PERSISTENT && 
                         deliveryMode == DeliveryMode.PERSISTENT)
                   );  
            
            org.apache.mina.common.ByteBuffer data = message.getData();
            ByteBuffer buffer = data == null ? ByteBuffer.allocate(0) : data.buf().slice();
            
            ssn.messageTransfer(destination.getExchangeName().toString(), MessageAcceptMode.NONE,
                                MessageAcquireMode.PRE_ACQUIRED,
                                new Header(deliveryProp, messageProps),
                    buffer, sync ? SYNC : NONE);
            if (sync)
            {
                ssn.sync();
            }
            
            
        }
        catch (RuntimeException rte)
        {
            JMSException ex = new JMSException("Exception when sending message");
            ex.setLinkedException(rte);
            throw ex;
        }
    }


    public boolean isBound(AMQDestination destination) throws JMSException
    {
        return _session.isQueueBound(destination);
    }
}

