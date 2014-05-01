/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */

package org.apache.qpid.server.handler;

import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.BasicGetBody;
import org.apache.qpid.framing.BasicGetEmptyBody;
import org.apache.qpid.framing.MethodRegistry;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.server.flow.FlowCreditManager;
import org.apache.qpid.server.flow.MessageOnlyCreditManager;
import org.apache.qpid.server.subscription.SubscriptionImpl;
import org.apache.qpid.server.subscription.ClientDeliveryMethod;
import org.apache.qpid.server.subscription.RecordDeliveryMethod;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.subscription.SubscriptionFactoryImpl;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.qpid.server.state.AMQStateManager;
import org.apache.qpid.server.state.StateAwareMethodListener;
import org.apache.qpid.server.virtualhost.VirtualHost;

public class BasicGetMethodHandler implements StateAwareMethodListener<BasicGetBody>
{
    private static final Logger _log = Logger.getLogger(BasicGetMethodHandler.class);

    private static final BasicGetMethodHandler _instance = new BasicGetMethodHandler();

    public static BasicGetMethodHandler getInstance()
    {
        return _instance;
    }

    private BasicGetMethodHandler()
    {
    }

    public void methodReceived(AMQStateManager stateManager, BasicGetBody body, int channelId) throws AMQException
    {
        AMQProtocolSession session = stateManager.getProtocolSession();


        VirtualHost vHost = session.getVirtualHost();

        AMQChannel channel = session.getChannel(channelId);
        if (channel == null)
        {
            throw body.getChannelNotFoundException(channelId);
        }
        else
        {
            AMQQueue queue = body.getQueue() == null ? channel.getDefaultQueue() : vHost.getQueueRegistry().getQueue(body.getQueue());
            if (queue == null)
            {
                _log.info("No queue for '" + body.getQueue() + "'");
                if(body.getQueue()!=null)
                {
                    throw body.getConnectionException(AMQConstant.NOT_FOUND,
                                                      "No such queue, '" + body.getQueue()+ "'");
                }
                else
                {
                    throw body.getConnectionException(AMQConstant.NOT_ALLOWED,
                                                      "No queue name provided, no default queue defined.");
                }
            }
            else
            {

                //Perform ACLs
                if (!vHost.getAccessManager().authoriseConsume(session, body.getNoAck(), queue))
                {
                    throw body.getConnectionException(AMQConstant.ACCESS_REFUSED, "Permission denied");
                }
                else if (queue.isExclusive() && queue.getExclusiveOwner() != session)
                {
                    throw body.getConnectionException(AMQConstant.NOT_ALLOWED,
                                                      "Queue is exclusive, but not created on this Connection.");
                }

                if (!performGet(queue,session, channel, !body.getNoAck()))
                {
                    MethodRegistry methodRegistry = session.getMethodRegistry();
                    // TODO - set clusterId
                    BasicGetEmptyBody responseBody = methodRegistry.createBasicGetEmptyBody(null);


                    session.writeFrame(responseBody.generateFrame(channelId));
                }
            }
        }
    }

    public static boolean performGet(final AMQQueue queue,
                                     final AMQProtocolSession session,
                                     final AMQChannel channel,
                                     final boolean acks)
            throws AMQException
    {

        final FlowCreditManager singleMessageCredit = new MessageOnlyCreditManager(1L);

        final ClientDeliveryMethod getDeliveryMethod = new ClientDeliveryMethod()
        {

            int _msg;

            public void deliverToClient(final Subscription sub, final QueueEntry entry, final long deliveryTag)
            throws AMQException
            {
                singleMessageCredit.useCreditForMessage(entry.getMessage());
                if(entry.getMessage() instanceof AMQMessage)
                {
                    session.getProtocolOutputConverter().writeGetOk(entry, channel.getChannelId(),
                                                                            deliveryTag, queue.getMessageCount());
                }
                else
                {
                    //TODO Convert AMQP 0-10 message
                    throw new RuntimeException("Not implemented conversion of 0-10 message");
                }

            }
        };
        final RecordDeliveryMethod getRecordMethod = new RecordDeliveryMethod()
        {

            public void recordMessageDelivery(final Subscription sub, final QueueEntry entry, final long deliveryTag)
            {
                channel.addUnacknowledgedMessage(entry, deliveryTag, null);
            }
        };

        Subscription sub;
        if(acks)
        {
            sub = SubscriptionFactoryImpl.INSTANCE.createSubscription(channel, session, null, acks, null, false, singleMessageCredit, getDeliveryMethod, getRecordMethod);
        }
        else
        {
            sub = new GetNoAckSubscription(channel,
                                                 session,
                                                 null,
                                                 null,
                                                 false,
                                                 singleMessageCredit,
                                                 getDeliveryMethod,
                                                 getRecordMethod);
        }

        queue.registerSubscription(sub,false);
        queue.flushSubscription(sub);
        queue.unregisterSubscription(sub);
        return(!singleMessageCredit.hasCredit());


    }

    public static final class GetNoAckSubscription extends SubscriptionImpl.NoAckSubscription
    {
        public GetNoAckSubscription(AMQChannel channel, AMQProtocolSession protocolSession,
                               AMQShortString consumerTag, FieldTable filters,
                               boolean noLocal, FlowCreditManager creditManager,
                                   ClientDeliveryMethod deliveryMethod,
                                   RecordDeliveryMethod recordMethod)
            throws AMQException
        {
            super(channel, protocolSession, consumerTag, filters, noLocal, creditManager, deliveryMethod, recordMethod);
        }

        public boolean isTransient()
        {
            return true;
        }

        public boolean wouldSuspend(QueueEntry msg)
        {
            return !getCreditManager().useCreditForMessage(msg.getMessage());
        }

    }
}
