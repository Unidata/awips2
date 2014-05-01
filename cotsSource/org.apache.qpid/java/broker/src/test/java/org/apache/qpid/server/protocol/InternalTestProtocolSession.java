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
package org.apache.qpid.server.protocol;

import java.security.Principal;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.server.output.ProtocolOutputConverter;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.message.MessageContentSource;
import org.apache.qpid.transport.TestNetworkDriver;

public class InternalTestProtocolSession extends AMQProtocolEngine implements ProtocolOutputConverter
{
    // ChannelID(LIST)  -> LinkedList<Pair>
    final Map<Integer, Map<AMQShortString, LinkedList<DeliveryPair>>> _channelDelivers;
    private AtomicInteger _deliveryCount = new AtomicInteger(0);

    public InternalTestProtocolSession(VirtualHost virtualHost) throws AMQException
    {
        super(ApplicationRegistry.getInstance().getVirtualHostRegistry(), new TestNetworkDriver());

        _channelDelivers = new HashMap<Integer, Map<AMQShortString, LinkedList<DeliveryPair>>>();

        // Need to authenticate session for it to be representative testing.
        setAuthorizedID(new Principal()
        {
            public String getName()
            {
                return "InternalTestProtocolSession";
            }
        });

        setVirtualHost(virtualHost);
    }

    public ProtocolOutputConverter getProtocolOutputConverter()
    {
        return this;
    }

    public byte getProtocolMajorVersion()
    {
        return (byte) 8;
    }

    public void writeReturn(MessagePublishInfo messagePublishInfo,
                            ContentHeaderBody header,
                            MessageContentSource msgContent,
                            int channelId,
                            int replyCode,
                            AMQShortString replyText) throws AMQException
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public byte getProtocolMinorVersion()
    {
        return (byte) 0;
    }

    // ***

    public List<DeliveryPair> getDelivers(int channelId, AMQShortString consumerTag, int count)
    {
        synchronized (_channelDelivers)
        {
            List<DeliveryPair> all =_channelDelivers.get(channelId).get(consumerTag);

            if (all == null)
            {
                return new ArrayList<DeliveryPair>(0);
            }

            List<DeliveryPair> msgs = all.subList(0, count);

            List<DeliveryPair> response = new ArrayList<DeliveryPair>(msgs);

            //Remove the msgs from the receivedList.
            msgs.clear();

            return response;
        }
    }

    // *** ProtocolOutputConverter Implementation
    public void writeReturn(AMQMessage message, int channelId, int replyCode, AMQShortString replyText) throws AMQException
    {
    }

    public void confirmConsumerAutoClose(int channelId, AMQShortString consumerTag)
    {
    }

    public void writeDeliver(QueueEntry entry, int channelId, long deliveryTag, AMQShortString consumerTag) throws AMQException
    {
        _deliveryCount.incrementAndGet();

        synchronized (_channelDelivers)
        {
            Map<AMQShortString, LinkedList<DeliveryPair>> consumers = _channelDelivers.get(channelId);

            if (consumers == null)
            {
                consumers = new HashMap<AMQShortString, LinkedList<DeliveryPair>>();
                _channelDelivers.put(channelId, consumers);
            }

            LinkedList<DeliveryPair> consumerDelivers = consumers.get(consumerTag);

            if (consumerDelivers == null)
            {
                consumerDelivers = new LinkedList<DeliveryPair>();
                consumers.put(consumerTag, consumerDelivers);
            }

            consumerDelivers.add(new DeliveryPair(deliveryTag, (AMQMessage)entry.getMessage()));
        }
    }

    public void writeGetOk(QueueEntry message, int channelId, long deliveryTag, int queueSize) throws AMQException
    {
    }

    public void awaitDelivery(int msgs)
    {
        while (msgs > _deliveryCount.get())
        {
            try
            {
                Thread.sleep(100);
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
            }
        }
    }

    public class DeliveryPair
    {
        private long _deliveryTag;
        private AMQMessage _message;

        public DeliveryPair(long deliveryTag, AMQMessage message)
        {
            _deliveryTag = deliveryTag;
            _message = message;
        }

        public AMQMessage getMessage()
        {
            return _message;
        }

        public long getDeliveryTag()
        {
            return _deliveryTag;
        }
    }

    public boolean isClosed()
    {
        return _closed;
    }

    public void closeProtocolSession(boolean waitLast)
    {
        // Override as we don't have a real IOSession to close.
        //  The alternative is to fully implement the TestIOSession to return a CloseFuture from close();
        //  Then the AMQMinaProtocolSession can join on the returning future without a NPE.
    }
}
