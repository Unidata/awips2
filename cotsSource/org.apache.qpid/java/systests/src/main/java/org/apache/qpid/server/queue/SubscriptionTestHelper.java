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

import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.logging.LogActor;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.framing.AMQShortString;

import java.util.ArrayList;
import java.util.List;
import java.util.Queue;

public class SubscriptionTestHelper implements Subscription
{
    private final List<QueueEntry> messages;
    private final Object key;
    private boolean isSuspended;
    private AMQQueue.Context _queueContext;

    public SubscriptionTestHelper(Object key)
    {
        this(key, new ArrayList<QueueEntry>());
    }

    public SubscriptionTestHelper(final Object key, final boolean isSuspended)
    {
        this(key);
        setSuspended(isSuspended);
    }

    SubscriptionTestHelper(Object key, List<QueueEntry> messages)
    {
        this.key = key;
        this.messages = messages;
    }

    List<QueueEntry> getMessages()
    {
        return messages;
    }

    public void setQueue(AMQQueue queue, boolean exclusive)
    {

    }

    public void setNoLocal(boolean noLocal)
    {
        
    }

    public void send(QueueEntry msg)
    {
        messages.add(msg);
    }

    public void setSuspended(boolean suspended)
    {
        isSuspended = suspended;
    }

    public boolean isSuspended()
    {
        return isSuspended;
    }

    public boolean wouldSuspend(QueueEntry msg)
    {
        return isSuspended;
    }

    public void addToResendQueue(QueueEntry msg)
    {
        //no-op
    }

    public void getSendLock()
    {
        return;
    }

    public void releaseSendLock()
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void resend(final QueueEntry entry)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void onDequeue(final QueueEntry queueEntry)
    {

    }

    public void restoreCredit(QueueEntry queueEntry)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void setStateListener(final StateListener listener)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public State getState()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public AMQQueue.Context getQueueContext()
    {
        return _queueContext;
    }

    public void setQueueContext(AMQQueue.Context queueContext)
    {
        _queueContext = queueContext;
    }

    public boolean setLastSeenEntry(QueueEntry expected, QueueEntry newValue)
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public AMQChannel getChannel()
    {
        return null;
    }

    public void start()
    {
        //no-op
    }

    public AMQShortString getConsumerTag()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public long getSubscriptionID()
    {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isActive()
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void confirmAutoClose()
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void set(String key, Object value)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public Object get(String key)
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public LogActor getLogActor()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isTransient()
    {
        return false;
    }

    public AMQQueue getQueue()
    {
        return null;
    }

    public QueueEntry.SubscriptionAcquiredState getOwningState()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public QueueEntry.SubscriptionAssignedState getAssignedState()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void queueDeleted(AMQQueue queue)
    {
    }

    public boolean filtersMessages()
    {
        return false;
    }

    public boolean hasInterest(QueueEntry msg)
    {
        return true;
    }

    public boolean isAutoClose()
    {
        return false;
    }

    public Queue<QueueEntry> getPreDeliveryQueue()
    {
        return null;
    }

    public Queue<QueueEntry> getResendQueue()
    {
        return null;
    }

    public Queue<QueueEntry> getNextQueue(Queue<QueueEntry> messages)
    {
        return messages;
    }

    public void enqueueForPreDelivery(QueueEntry msg, boolean deliverFirst)
    {
        //no-op
    }

    public void close()
    {
        //no-op
    }

    public boolean isClosed()
    {
        return false;
    }

    public boolean acquires()
    {
        return true;
    }

    public boolean seesRequeues()
    {
        return true;
    }

    public boolean isBrowser()
    {
        return false;
    }

    public int hashCode()
    {
        return key.hashCode();
    }

    public boolean equals(Object o)
    {
        return o instanceof SubscriptionTestHelper && ((SubscriptionTestHelper) o).key.equals(key);
    }

    public String toString()
    {
        return key.toString();
    }
}
