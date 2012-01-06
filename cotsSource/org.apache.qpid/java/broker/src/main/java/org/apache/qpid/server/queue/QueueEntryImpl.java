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

import org.apache.qpid.AMQException;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.message.ServerMessage;
import org.apache.qpid.server.message.MessageReference;
import org.apache.qpid.server.message.AMQMessageHeader;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.txn.AutoCommitTransaction;
import org.apache.qpid.server.txn.ServerTransaction;
import org.apache.log4j.Logger;

import java.util.*;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;
import java.util.concurrent.atomic.AtomicLongFieldUpdater;
import java.util.concurrent.CopyOnWriteArraySet;


public class QueueEntryImpl implements QueueEntry
{

    /**
     * Used for debugging purposes.
     */
    private static final Logger _log = Logger.getLogger(QueueEntryImpl.class);

    private final SimpleQueueEntryList _queueEntryList;

    private MessageReference _message;

    private Set<Subscription> _rejectedBy = null;

    private volatile EntryState _state = AVAILABLE_STATE;

    private static final
        AtomicReferenceFieldUpdater<QueueEntryImpl, EntryState>
            _stateUpdater =
        AtomicReferenceFieldUpdater.newUpdater
        (QueueEntryImpl.class, EntryState.class, "_state");


    private volatile Set<StateChangeListener> _stateChangeListeners;

    private static final
        AtomicReferenceFieldUpdater<QueueEntryImpl, Set>
                _listenersUpdater =
        AtomicReferenceFieldUpdater.newUpdater
        (QueueEntryImpl.class, Set.class, "_stateChangeListeners");


    private static final
        AtomicLongFieldUpdater<QueueEntryImpl>
            _entryIdUpdater =
        AtomicLongFieldUpdater.newUpdater
        (QueueEntryImpl.class, "_entryId");


    private volatile long _entryId;

    volatile QueueEntryImpl _next;

    private static final int DELIVERED_TO_CONSUMER = 1;
    private static final int REDELIVERED = 2;

    private volatile int _deliveryState;


    QueueEntryImpl(SimpleQueueEntryList queueEntryList)
    {
        this(queueEntryList,null,Long.MIN_VALUE);
        _state = DELETED_STATE;
    }


    public QueueEntryImpl(SimpleQueueEntryList queueEntryList, ServerMessage message, final long entryId)
    {
        _queueEntryList = queueEntryList;

        _message = message == null ? null : message.newReference();

        _entryIdUpdater.set(this, entryId);
    }

    public QueueEntryImpl(SimpleQueueEntryList queueEntryList, ServerMessage message)
    {
        _queueEntryList = queueEntryList;
        _message = message == null ? null :  message.newReference();
    }

    protected void setEntryId(long entryId)
    {
        _entryIdUpdater.set(this, entryId);
    }

    protected long getEntryId()
    {
        return _entryId;
    }

    public AMQQueue getQueue()
    {
        return _queueEntryList.getQueue();
    }

    public ServerMessage getMessage()
    {
        return  _message == null ? null : _message.getMessage();
    }

    public long getSize()
    {
        return getMessage() == null ? 0 : getMessage().getSize();
    }

    public boolean getDeliveredToConsumer()
    {
        return (_deliveryState & DELIVERED_TO_CONSUMER) != 0;
    }

    public boolean expired() throws AMQException
    {
        ServerMessage message = getMessage();
        if(message != null)
        {
            long expiration = message.getExpiration();
            if (expiration != 0L)
            {
                long now = System.currentTimeMillis();

                return (now > expiration);
            }
        }
        return false;

    }

    public boolean isAcquired()
    {
        return _state.getState() == State.ACQUIRED;
    }

    public boolean acquire()
    {
        return acquire(NON_SUBSCRIPTION_ACQUIRED_STATE);
    }

    private boolean acquire(final EntryState state)
    {
        boolean acquired = _stateUpdater.compareAndSet(this,AVAILABLE_STATE, state);

        // deal with the case where the node has been assigned to a given subscription already
        // including the case that the node is assigned to a closed subscription
        if(!acquired)
        {
            if(state != NON_SUBSCRIPTION_ACQUIRED_STATE)
            {
                EntryState currentState = _state;
                if(currentState.getState() == State.AVAILABLE
                   && ((currentState == AVAILABLE_STATE)
                       || (((SubscriptionAcquiredState)state).getSubscription() ==
                           ((SubscriptionAssignedState)currentState).getSubscription())
                       || ((SubscriptionAssignedState)currentState).getSubscription().isClosed() ))
                {
                    acquired = _stateUpdater.compareAndSet(this,currentState, state);
                }
            }
        }
        if(acquired && _stateChangeListeners != null)
        {
            notifyStateChange(State.AVAILABLE, State.ACQUIRED);
        }

        return acquired;
    }

    public boolean acquire(Subscription sub)
    {
        final boolean acquired = acquire(sub.getOwningState());
        if(acquired)
        {
            _deliveryState |= DELIVERED_TO_CONSUMER;
        }
        return acquired;
    }

    public boolean acquiredBySubscription()
    {

        return (_state instanceof SubscriptionAcquiredState);
    }

    public boolean isAcquiredBy(Subscription subscription)
    {
        EntryState state = _state;
        return state instanceof SubscriptionAcquiredState
               && ((SubscriptionAcquiredState)state).getSubscription() == subscription;
    }

    public void release()
    {
        _stateUpdater.set(this,AVAILABLE_STATE);
        if(!getQueue().isDeleted())
        {
            getQueue().requeue(this);
            if(_stateChangeListeners != null)
            {
                notifyStateChange(QueueEntry.State.ACQUIRED, QueueEntry.State.AVAILABLE);
            }

        }
        else if(acquire())
        {
            routeToAlternate();
        }


    }

    public boolean releaseButRetain()
    {
        EntryState state = _state;

        boolean stateUpdated = false;

        if(state instanceof SubscriptionAcquiredState)
        {
            Subscription sub = ((SubscriptionAcquiredState) state).getSubscription();
            if(_stateUpdater.compareAndSet(this, state, sub.getAssignedState()))
            {
                System.err.println("Message released (and retained)" + getMessage().getMessageNumber());
                getQueue().requeue(this);
                if(_stateChangeListeners != null)
                {
                    notifyStateChange(QueueEntry.State.ACQUIRED, QueueEntry.State.AVAILABLE);
                }
                stateUpdated = true;
            }
        }

        return stateUpdated;

    }

    public boolean immediateAndNotDelivered()
    {
        return !getDeliveredToConsumer() && isImmediate();
    }

    private boolean isImmediate()
    {
        final ServerMessage message = getMessage();
        return message != null && message.isImmediate();
    }

    public void setRedelivered()
    {
        _deliveryState |= REDELIVERED;
    }

    public AMQMessageHeader getMessageHeader()
    {
        final ServerMessage message = getMessage();
        return message == null ? null : message.getMessageHeader();
    }

    public boolean isPersistent()
    {
        final ServerMessage message = getMessage();
        return message != null && message.isPersistent();
    }

    public boolean isRedelivered()
    {
        return (_deliveryState & REDELIVERED) != 0;
    }

    public Subscription getDeliveredSubscription()
    {
            EntryState state = _state;
            if (state instanceof SubscriptionAcquiredState)
            {
                return ((SubscriptionAcquiredState) state).getSubscription();
            }
            else
            {
                return null;
            }

    }

    public void reject()
    {
        reject(getDeliveredSubscription());
    }

    public void reject(Subscription subscription)
    {
        if (subscription != null)
        {
            if (_rejectedBy == null)
            {
                _rejectedBy = new HashSet<Subscription>();
            }

            _rejectedBy.add(subscription);
        }
        else
        {
            _log.warn("Requesting rejection by null subscriber:" + this);
        }
    }

    public boolean isRejectedBy(Subscription subscription)
    {

        if (_rejectedBy != null) // We have subscriptions that rejected this message
        {
            return _rejectedBy.contains(subscription);
        }
        else // This messasge hasn't been rejected yet.
        {
            return false;
        }
    }

    public void requeue(Subscription subscription)
    {
        getQueue().requeue(this, subscription);
        if(_stateChangeListeners != null)
        {
            notifyStateChange(QueueEntry.State.ACQUIRED, QueueEntry.State.AVAILABLE);
        }
    }

    public void dequeue()
    {
        EntryState state = _state;

        if((state.getState() == State.ACQUIRED) &&_stateUpdater.compareAndSet(this, state, DEQUEUED_STATE))
        {
            if (state instanceof SubscriptionAcquiredState)
            {
                Subscription s = ((SubscriptionAcquiredState) state).getSubscription();
                s.onDequeue(this);
            }

            getQueue().dequeue(this);
            if(_stateChangeListeners != null)
            {
                notifyStateChange(state.getState() , QueueEntry.State.DEQUEUED);
            }

        }

    }

    private void notifyStateChange(final State oldState, final State newState)
    {
        for(StateChangeListener l : _stateChangeListeners)
        {
            l.stateChanged(this, oldState, newState);
        }
    }

    public void dispose()
    {
        if(delete())
        {
            _message.release();
        }
    }

    public void discard()
    {
        //if the queue is null then the message is waiting to be acked, but has been removed.
        if (getQueue() != null)
        {
            dequeue();
        }

        dispose();
    }

    public void routeToAlternate()
    {
        final AMQQueue currentQueue = getQueue();
            Exchange alternateExchange = currentQueue.getAlternateExchange();

            if(alternateExchange != null)
            {
                final List<AMQQueue> rerouteQueues = alternateExchange.route(new InboundMessageAdapter(this));
                final ServerMessage message = getMessage();
                if(rerouteQueues != null && rerouteQueues.size() != 0)
                {
                    ServerTransaction txn = new AutoCommitTransaction(getQueue().getVirtualHost().getTransactionLog());

                    txn.enqueue(rerouteQueues, message, new ServerTransaction.Action() {
                        public void postCommit()
                        {
                            try
                            {
                                for(AMQQueue queue : rerouteQueues)
                                {
                                    QueueEntry entry = queue.enqueue(message);
                                }
                            }
                            catch (AMQException e)
                            {
                                throw new RuntimeException(e);
                            }
                        }

                        public void onRollback()
                        {

                        }
                    });
                    txn.dequeue(currentQueue,message,
                                new ServerTransaction.Action()
                                {
                                    public void postCommit()
                                    {
                                        discard();
                                    }

                                    public void onRollback()
                                    {

                                    }
                                });
                }
            }
    }

    public boolean isQueueDeleted()
    {
        return getQueue().isDeleted();
    }

    public void addStateChangeListener(StateChangeListener listener)
    {
        Set<StateChangeListener> listeners = _stateChangeListeners;
        if(listeners == null)
        {
            _listenersUpdater.compareAndSet(this, null, new CopyOnWriteArraySet<StateChangeListener>());
            listeners = _stateChangeListeners;
        }

        listeners.add(listener);
    }

    public boolean removeStateChangeListener(StateChangeListener listener)
    {
        Set<StateChangeListener> listeners = _stateChangeListeners;
        if(listeners != null)
        {
            return listeners.remove(listener);
        }

        return false;
    }


    public int compareTo(final QueueEntry o)
    {
        QueueEntryImpl other = (QueueEntryImpl)o;
        return getEntryId() > other.getEntryId() ? 1 : getEntryId() < other.getEntryId() ? -1 : 0;
    }

    public QueueEntryImpl getNext()
    {

        QueueEntryImpl next = nextNode();
        while(next != null && next.isDeleted())
        {

            final QueueEntryImpl newNext = next.nextNode();
            if(newNext != null)
            {
                SimpleQueueEntryList._nextUpdater.compareAndSet(this,next, newNext);
                next = nextNode();
            }
            else
            {
                next = null;
            }

        }
        return next;
    }

    QueueEntryImpl nextNode()
    {
        return _next;
    }

    public boolean isDeleted()
    {
        return _state == DELETED_STATE;
    }

    public boolean delete()
    {
        EntryState state = _state;

        if(state != DELETED_STATE && _stateUpdater.compareAndSet(this,state,DELETED_STATE))
        {
            _queueEntryList.advanceHead();
            return true;
        }
        else
        {
            return false;
        }
    }

    public QueueEntryList getQueueEntryList()
    {
        return _queueEntryList;
    }

}
