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
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.server.configuration.QueueConfiguration;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.management.ManagedObject;
import org.apache.qpid.server.message.ServerMessage;
import org.apache.qpid.server.security.PrincipalHolder;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.txn.ServerTransaction;
import org.apache.qpid.AMQException;

import java.util.List;
import java.util.Set;
import java.util.Map;

public class MockAMQQueue implements AMQQueue
{
    private boolean _deleted = false;
    private AMQShortString _name;
    private VirtualHost _virtualhost;

    private PrincipalHolder _principalHolder;

    private Object _exclusiveOwner;

    public MockAMQQueue(String name)
    {
       _name = new AMQShortString(name);
    }

    public boolean getDeleteOnNoConsumers()
    {
        return false;
    }

    public void setDeleteOnNoConsumers(boolean b)
    {        
    }

    public AMQShortString getName()
    {
        return _name;
    }

    public void setNoLocal(boolean b)
    {
        
    }

    public boolean isDurable()
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isAutoDelete()
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public AMQShortString getOwner()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setVirtualHost(VirtualHost virtualhost)
    {
        _virtualhost = virtualhost;
    }

    public VirtualHost getVirtualHost()
    {
        return _virtualhost;
    }

    public void bind(Exchange exchange, AMQShortString routingKey, FieldTable arguments) throws AMQException
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void unBind(Exchange exchange, AMQShortString routingKey, FieldTable arguments) throws AMQException
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public List<ExchangeBinding> getExchangeBindings()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void registerSubscription(Subscription subscription, boolean exclusive) throws AMQException
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void unregisterSubscription(Subscription subscription) throws AMQException
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getConsumerCount()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getActiveConsumerCount()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isUnused()
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isEmpty()
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getMessageCount()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getUndeliveredMessageCount()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getQueueDepth()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getReceivedMessageCount()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getOldestMessageArrivalTime()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isDeleted()
    {
        return _deleted;
    }

    public int delete() throws AMQException
    {
       return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public QueueEntry enqueue(ServerMessage message) throws AMQException
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void requeue(QueueEntry entry)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void requeue(QueueEntryImpl storeContext, Subscription subscription)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void dequeue(QueueEntry entry)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean resend(QueueEntry entry, Subscription subscription) throws AMQException
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void addQueueDeleteTask(Task task)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public List<QueueEntry> getMessagesOnTheQueue()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public List<QueueEntry> getMessagesOnTheQueue(long fromMessageId, long toMessageId)
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public List<Long> getMessagesOnTheQueue(int num)
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public List<Long> getMessagesOnTheQueue(int num, int offest)
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public QueueEntry getMessageOnTheQueue(long messageId)
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public List<QueueEntry> getMessagesRangeOnTheQueue(long fromPosition, long toPosition)
    {
        return null;
    }

    public void moveMessagesToAnotherQueue(long fromMessageId, long toMessageId, String queueName, ServerTransaction storeContext)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void copyMessagesToAnotherQueue(long fromMessageId, long toMessageId, String queueName, ServerTransaction storeContext)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void removeMessagesFromQueue(long fromMessageId, long toMessageId)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getMaximumMessageSize()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setMaximumMessageSize(long value)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getMaximumMessageCount()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setMaximumMessageCount(long value)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getMaximumQueueDepth()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setMaximumQueueDepth(long value)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getMaximumMessageAge()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setMaximumMessageAge(long maximumMessageAge)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean getBlockOnQueueFull()
    {
        return false;
    }

    public void setBlockOnQueueFull(boolean block)
    {
    }

    public long getMinimumAlertRepeatGap()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void deleteMessageFromTop()
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public long clearQueue()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }


    public void checkMessageStatus() throws AMQException
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public Set<NotificationCheck> getNotificationChecks()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void flushSubscription(Subscription sub) throws AMQException
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void deliverAsync(Subscription sub)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void deliverAsync()
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void stop()
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isExclusive()
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public Exchange getAlternateExchange()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setAlternateExchange(Exchange exchange)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public Map<String, Object> getArguments()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void checkCapacity(AMQChannel channel)
    {
    }

    public ManagedObject getManagedObject()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int compareTo(AMQQueue o)
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setMinimumAlertRepeatGap(long value)
    {

    }

    public long getCapacity()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setCapacity(long capacity)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getFlowResumeCapacity()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setFlowResumeCapacity(long flowResumeCapacity)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void configure(QueueConfiguration config)
    {

    }

    public PrincipalHolder getPrincipalHolder()
    {
        return _principalHolder;
    }

    public void setPrincipalHolder(PrincipalHolder principalHolder)
    {
        _principalHolder = principalHolder;
    }

    public Object getExclusiveOwner()
    {
        return _exclusiveOwner;
    }

    public void setExclusiveOwner(Object exclusiveOwner)
    {
        _exclusiveOwner = exclusiveOwner;
    }


    public String getResourceName()
    {
        return _name.toString();
    }

    public boolean isOverfull()
    {
        return false;
    }
}
