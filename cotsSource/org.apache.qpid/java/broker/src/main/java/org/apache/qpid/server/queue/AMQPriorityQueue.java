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

import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.subscription.SubscriptionList;
import org.apache.qpid.server.subscription.Subscription;

public class AMQPriorityQueue extends SimpleAMQQueue
{
    protected AMQPriorityQueue(final AMQShortString name,
                               final boolean durable,
                               final AMQShortString owner,
                               final boolean autoDelete,
                               final VirtualHost virtualHost,
                               int priorities)
    {
        super(name, durable, owner, autoDelete, virtualHost, new PriorityQueueList.Factory(priorities));
    }

    public AMQPriorityQueue(String queueName,
                            boolean durable,
                            String owner,
                            boolean autoDelete,
                            VirtualHost virtualHost, int priorities)
    {
        this(new AMQShortString(queueName), durable, new AMQShortString(owner),autoDelete,virtualHost,priorities);
    }

    public int getPriorities()
    {
        return ((PriorityQueueList) _entries).getPriorities();
    }

    @Override
    protected void checkSubscriptionsNotAheadOfDelivery(final QueueEntry entry)
    {
        // check that all subscriptions are not in advance of the entry
        SubscriptionList.SubscriptionNodeIterator subIter = _subscriptionList.iterator();
        while(subIter.advance() && !entry.isAcquired())
        {
            final Subscription subscription = subIter.getNode().getSubscription();
            if(!subscription.isClosed())
            {
                QueueContext context = (QueueContext) subscription.getQueueContext();
                if(context != null)
                {
                    QueueEntry subnode = context._lastSeenEntry;
                    QueueEntry released = context._releasedEntry;
                    while(subnode != null && entry.compareTo(subnode) < 0 && !entry.isAcquired() && (released == null || released.compareTo(entry) < 0))
                    {
                        if(QueueContext._releasedUpdater.compareAndSet(context,released,entry))
                        {
                            break;
                        }
                        else
                        {
                            subnode = context._lastSeenEntry;
                            released = context._releasedEntry;
                        }
                    }
                }
            }

        }
    }
}
