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
package org.apache.qpid.server;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.framing.*;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.framing.abstraction.ContentChunk;
import org.apache.qpid.server.ack.UnacknowledgedMessageMap;
import org.apache.qpid.server.ack.UnacknowledgedMessageMapImpl;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.flow.FlowCreditManager;
import org.apache.qpid.server.flow.Pre0_10CreditManager;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.queue.*;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.subscription.SubscriptionFactoryImpl;
import org.apache.qpid.server.subscription.ClientDeliveryMethod;
import org.apache.qpid.server.subscription.RecordDeliveryMethod;
import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.server.store.StoredMessage;
import org.apache.qpid.server.txn.*;
import org.apache.qpid.server.message.ServerMessage;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.server.message.MessageMetaData;
import org.apache.qpid.server.message.MessageReference;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.logging.LogActor;
import org.apache.qpid.server.logging.LogSubject;
import org.apache.qpid.server.logging.messages.ChannelMessages;
import org.apache.qpid.server.logging.subjects.ChannelLogSubject;
import org.apache.qpid.server.logging.actors.AMQPChannelActor;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.output.ProtocolOutputConverter;

public class AMQChannel
{
    public static final int DEFAULT_PREFETCH = 5000;

    private static final Logger _logger = Logger.getLogger(AMQChannel.class);

    private static final boolean MSG_AUTH =
        ApplicationRegistry.getInstance().getConfiguration().getMsgAuth();


    private final int _channelId;


    private final Pre0_10CreditManager _creditManager = new Pre0_10CreditManager(0l,0l);

    /**
     * The delivery tag is unique per channel. This is pre-incremented before putting into the deliver frame so that
     * value of this represents the <b>last</b> tag sent out
     */
    private long _deliveryTag = 0;

    /** A channel has a default queue (the last declared) that is used when no queue name is explictily set */
    private AMQQueue _defaultQueue;

    /** This tag is unique per subscription to a queue. The server returns this in response to a basic.consume request. */
    private int _consumerTag;

    /**
     * The current message - which may be partial in the sense that not all frames have been received yet - which has
     * been received by this channel. As the frames are received the message gets updated and once all frames have been
     * received the message can then be routed.
     */
    private IncomingMessage _currentMessage;

    /** Maps from consumer tag to subscription instance. Allows us to unsubscribe from a queue. */
    protected final Map<AMQShortString, Subscription> _tag2SubscriptionMap = new HashMap<AMQShortString, Subscription>();

    private final MessageStore _messageStore;

    private UnacknowledgedMessageMap _unacknowledgedMessageMap = new UnacknowledgedMessageMapImpl(DEFAULT_PREFETCH);

    // Set of messages being acknoweledged in the current transaction
    private SortedSet<QueueEntry> _acknowledgedMessages = new TreeSet<QueueEntry>();

    private final AtomicBoolean _suspended = new AtomicBoolean(false);

    private ServerTransaction _transaction;

    // Why do we need this reference ? - ritchiem
    private final AMQProtocolSession _session;
    private boolean _closing;

    private final ConcurrentMap<AMQQueue, Boolean> _blockingQueues = new ConcurrentHashMap<AMQQueue, Boolean>();

    private final AtomicBoolean _blocking = new AtomicBoolean(false);


    private LogActor _actor;
    private LogSubject _logSubject;
    private volatile boolean _rollingBack;

    private static final Runnable NULL_TASK = new Runnable() { public void run() {} };
    private List<QueueEntry> _resendList = new ArrayList<QueueEntry>();
    private static final
    AMQShortString IMMEDIATE_DELIVERY_REPLY_TEXT = new AMQShortString("Immediate delivery is not possible.");

    public AMQChannel(AMQProtocolSession session, int channelId, MessageStore messageStore)
            throws AMQException
    {
        _session = session;
        _channelId = channelId;

        _actor = new AMQPChannelActor(this, session.getLogActor().getRootMessageLogger());
        _logSubject = new ChannelLogSubject(this);

        _actor.message(ChannelMessages.CHN_CREATE());

        _messageStore = messageStore;

        // by default the session is non-transactional
        _transaction = new AutoCommitTransaction(_messageStore);
    }

    /** Sets this channel to be part of a local transaction */
    public void setLocalTransactional()
    {
        _transaction = new LocalTransaction(_messageStore);
    }

    public boolean isTransactional()
    {
        // this does not look great but there should only be one "non-transactional"
        // transactional context, while there could be several transactional ones in
        // theory
        return !(_transaction instanceof AutoCommitTransaction);
    }

    public int getChannelId()
    {
        return _channelId;
    }

    public void setPublishFrame(MessagePublishInfo info, final Exchange e) throws AMQException
    {

        _currentMessage = new IncomingMessage(info);
        _currentMessage.setExchange(e);
    }

    public void publishContentHeader(ContentHeaderBody contentHeaderBody)
            throws AMQException
    {
        if (_currentMessage == null)
        {
            throw new AMQException("Received content header without previously receiving a BasicPublish frame");
        }
        else
        {
            if (_logger.isDebugEnabled())
            {
                _logger.debug("Content header received on channel " + _channelId);
            }

            _currentMessage.setContentHeaderBody(contentHeaderBody);

            _currentMessage.setExpiration();


            MessageMetaData mmd = _currentMessage.headersReceived();
            final StoredMessage<MessageMetaData> handle = _messageStore.addMessage(mmd);
            _currentMessage.setStoredMessage(handle);

            routeCurrentMessage();


            _transaction.addPostCommitAction(new ServerTransaction.Action()
            {

                public void postCommit()
                {
                }

                public void onRollback()
                {
                    handle.remove();
                }
            });

            deliverCurrentMessageIfComplete();

        }
    }

    private void deliverCurrentMessageIfComplete()
            throws AMQException
    {
        // check and deliver if header says body length is zero
        if (_currentMessage.allContentReceived())
        {
            try
            {

                final ArrayList<AMQQueue> destinationQueues = _currentMessage.getDestinationQueues();

                if(!checkMessageUserId(_currentMessage.getContentHeader()))
                {
                    _transaction.addPostCommitAction(new WriteReturnAction(AMQConstant.ACCESS_REFUSED, "Access Refused", _currentMessage));
                }
                else
                {
                    if(destinationQueues == null || _currentMessage.getDestinationQueues().isEmpty())
                    {
                        if (_currentMessage.isMandatory() || _currentMessage.isImmediate())
                        {
                            _transaction.addPostCommitAction(new WriteReturnAction(AMQConstant.NO_ROUTE, "No Route for message", _currentMessage));
                        }
                        else
                        {
                            _logger.warn("MESSAGE DISCARDED: No routes for message - " + createAMQMessage(_currentMessage));
                        }

                    }
                    else
                    {
                        _transaction.enqueue(destinationQueues, _currentMessage, new MessageDeliveryAction(_currentMessage, destinationQueues));

                    }
                }
            }
            finally
            {
                _currentMessage = null;
            }
        }

    }

    public void publishContentBody(ContentBody contentBody) throws AMQException
    {
        if (_currentMessage == null)
        {
            throw new AMQException("Received content body without previously receiving a JmsPublishBody");
        }

        if (_logger.isDebugEnabled())
        {
            _logger.debug(debugIdentity() + "Content body received on channel " + _channelId);
        }

        try
        {

            // returns true iff the message was delivered (i.e. if all data was
            // received
            final ContentChunk contentChunk =
                    _session.getMethodRegistry().getProtocolVersionMethodConverter().convertToContentChunk(contentBody);

            _currentMessage.addContentBodyFrame(contentChunk);

            deliverCurrentMessageIfComplete();
        }
        catch (AMQException e)
        {
            // we want to make sure we don't keep a reference to the message in the
            // event of an error
            _currentMessage = null;
            throw e;
        }
    }

    protected void routeCurrentMessage() throws AMQException
    {
        _currentMessage.route();
    }

    public long getNextDeliveryTag()
    {
        return ++_deliveryTag;
    }

    public int getNextConsumerTag()
    {
        return ++_consumerTag;
    }


    public Subscription getSubscription(AMQShortString subscription)
    {
        return _tag2SubscriptionMap.get(subscription);
    }

    /**
     * Subscribe to a queue. We register all subscriptions in the channel so that if the channel is closed we can clean
     * up all subscriptions, even if the client does not explicitly unsubscribe from all queues.
     *
     * @param tag       the tag chosen by the client (if null, server will generate one)
     * @param queue     the queue to subscribe to
     * @param acks      Are acks enabled for this subscriber
     * @param filters   Filters to apply to this subscriber
     *
     * @param noLocal   Flag stopping own messages being receivied.
     * @param exclusive Flag requesting exclusive access to the queue
     * @return the consumer tag. This is returned to the subscriber and used in subsequent unsubscribe requests
     *
     * @throws AMQException                  if something goes wrong
     */
    public AMQShortString subscribeToQueue(AMQShortString tag, AMQQueue queue, boolean acks,
                                           FieldTable filters, boolean noLocal, boolean exclusive) throws AMQException
    {
        if (tag == null)
        {
            tag = new AMQShortString("sgen_" + getNextConsumerTag());
        }

        if (_tag2SubscriptionMap.containsKey(tag))
        {
            throw new AMQException("Consumer already exists with same tag: " + tag);
        }

         Subscription subscription =
                SubscriptionFactoryImpl.INSTANCE.createSubscription(_channelId, _session, tag, acks, filters, noLocal, _creditManager);


        // So to keep things straight we put before the call and catch all exceptions from the register and tidy up.
        // We add before we register as the Async Delivery process may AutoClose the subscriber
        // so calling _cT2QM.remove before we have done put which was after the register succeeded.
        // So to keep things straight we put before the call and catch all exceptions from the register and tidy up.

        _tag2SubscriptionMap.put(tag, subscription);

        try
        {
            queue.registerSubscription(subscription, exclusive);
        }
        catch (AMQException e)
        {
            _tag2SubscriptionMap.remove(tag);
            throw e;
        }
        return tag;
    }

    /**
     * Unsubscribe a consumer from a queue.
     * @param consumerTag
     * @return true if the consumerTag had a mapped queue that could be unregistered.
     * @throws AMQException
     */
    public boolean unsubscribeConsumer(AMQShortString consumerTag) throws AMQException
    {

        Subscription sub = _tag2SubscriptionMap.remove(consumerTag);
        if (sub != null)
        {
            try
            {
                sub.getSendLock();
                sub.getQueue().unregisterSubscription(sub);
            }
            finally
            {
                sub.releaseSendLock();
            }
            return true;
        }
        else
        {
            _logger.warn("Attempt to unsubscribe consumer with tag '"+consumerTag+"' which is not registered.");
        }
        return false;
    }

    /**
     * Called from the protocol session to close this channel and clean up. T
     *
     * @throws AMQException if there is an error during closure
     */
    public void close() throws AMQException
    {
        setClosing(true);

        unsubscribeAllConsumers();
        _transaction.rollback();

        try
        {
            requeue();
        }
        catch (AMQException e)
        {
            _logger.error("Caught AMQException whilst attempting to reque:" + e);
        }

    }

    private void setClosing(boolean closing)
    {
        _closing = closing;

        CurrentActor.get().message(_logSubject, ChannelMessages.CHN_CLOSE());
    }

    private void unsubscribeAllConsumers() throws AMQException
    {
        if (_logger.isInfoEnabled())
        {
            if (!_tag2SubscriptionMap.isEmpty())
            {
                _logger.info("Unsubscribing all consumers on channel " + toString());
            }
            else
            {
                _logger.info("No consumers to unsubscribe on channel " + toString());
            }
        }

        for (Map.Entry<AMQShortString, Subscription> me : _tag2SubscriptionMap.entrySet())
        {
            if (_logger.isInfoEnabled())
            {
                _logger.info("Unsubscribing consumer '" + me.getKey() + "' on channel " + toString());
            }

            Subscription sub = me.getValue();

            try
            {
                sub.getSendLock();
                sub.getQueue().unregisterSubscription(sub);
            }
            finally
            {
                sub.releaseSendLock();
            }

        }

        _tag2SubscriptionMap.clear();
    }

    /**
     * Add a message to the channel-based list of unacknowledged messages
     *
     * @param entry       the record of the message on the queue that was delivered
     * @param deliveryTag the delivery tag used when delivering the message (see protocol spec for description of the
     *                    delivery tag)
     * @param subscription The consumer that is to acknowledge this message.
     */
    public void addUnacknowledgedMessage(QueueEntry entry, long deliveryTag, Subscription subscription)
    {
        if (_logger.isDebugEnabled())
        {
            if (entry.getQueue() == null)
            {
                _logger.debug("Adding unacked message with a null queue:" + entry);
            }
            else
            {
                if (_logger.isDebugEnabled())
                {
                    _logger.debug(debugIdentity() + " Adding unacked message(" + entry.getMessage().toString() + " DT:" + deliveryTag
                               + ") with a queue(" + entry.getQueue() + ") for " + subscription);
                }
            }
        }

        _unacknowledgedMessageMap.add(deliveryTag, entry);

    }

    private final String id = "(" + System.identityHashCode(this) + ")";

    public String debugIdentity()
    {
        return _channelId + id;
    }

    /**
     * Called to attempt re-delivery all outstanding unacknowledged messages on the channel. May result in delivery to
     * this same channel or to other subscribers.
     *
     * @throws org.apache.qpid.AMQException if the requeue fails
     */
    public void requeue() throws AMQException
    {
        // we must create a new map since all the messages will get a new delivery tag when they are redelivered
        Collection<QueueEntry> messagesToBeDelivered = _unacknowledgedMessageMap.cancelAllMessages();

        if (!messagesToBeDelivered.isEmpty())
        {
            if (_logger.isInfoEnabled())
            {
                _logger.info("Requeuing " + messagesToBeDelivered.size() + " unacked messages. for " + toString());
            }

        }

        for (QueueEntry unacked : messagesToBeDelivered)
        {
            if (!unacked.isQueueDeleted())
            {
                // Mark message redelivered
                unacked.setRedelivered();

                // Ensure message is released for redelivery
                unacked.release();

            }
            else
            {
                unacked.discard();
            }
        }

    }

    /**
     * Requeue a single message
     *
     * @param deliveryTag The message to requeue
     *
     * @throws AMQException If something goes wrong.
     */
    public void requeue(long deliveryTag) throws AMQException
    {
        QueueEntry unacked = _unacknowledgedMessageMap.remove(deliveryTag);

        if (unacked != null)
        {
            // Mark message redelivered
            unacked.setRedelivered();

            // Ensure message is released for redelivery
            if (!unacked.isQueueDeleted())
            {

                // Ensure message is released for redelivery
                unacked.release();

            }
            else
            {
                _logger.warn(System.identityHashCode(this) + " Requested requeue of message(" + unacked
                          + "):" + deliveryTag + " but no queue defined and no DeadLetter queue so DROPPING message.");

                unacked.discard();
            }
        }
        else
        {
            _logger.warn("Requested requeue of message:" + deliveryTag + " but no such delivery tag exists."
                      + _unacknowledgedMessageMap.size());

        }

    }

    /**
     * Called to resend all outstanding unacknowledged messages to this same channel.
     *
     * @param requeue Are the messages to be requeued or dropped.
     *
     * @throws AMQException When something goes wrong.
     */
    public void resend(final boolean requeue) throws AMQException
    {


        final Map<Long, QueueEntry> msgToRequeue = new LinkedHashMap<Long, QueueEntry>();
        final Map<Long, QueueEntry> msgToResend = new LinkedHashMap<Long, QueueEntry>();

        if (_logger.isDebugEnabled())
        {
            _logger.debug("unacked map Size:" + _unacknowledgedMessageMap.size());
        }

        // Process the Unacked-Map.
        // Marking messages who still have a consumer for to be resent
        // and those that don't to be requeued.
        _unacknowledgedMessageMap.visit(new ExtractResendAndRequeue(_unacknowledgedMessageMap,
                                                                    msgToRequeue,
                                                                    msgToResend,
                                                                    requeue,
                                                                    _messageStore));


        // Process Messages to Resend
        if (_logger.isDebugEnabled())
        {
            if (!msgToResend.isEmpty())
            {
                _logger.debug("Preparing (" + msgToResend.size() + ") message to resend.");
            }
            else
            {
                _logger.debug("No message to resend.");
            }
        }

        for (Map.Entry<Long, QueueEntry> entry : msgToResend.entrySet())
        {
            QueueEntry message = entry.getValue();
            long deliveryTag = entry.getKey();



            ServerMessage msg = message.getMessage();
            AMQQueue queue = message.getQueue();

            // Our Java Client will always suspend the channel when resending!
            // If the client has requested the messages be resent then it is
            // their responsibility to ensure that thay are capable of receiving them
            // i.e. The channel hasn't been server side suspended.
            // if (isSuspended())
            // {
            // _logger.info("Channel is suspended so requeuing");
            // //move this message to requeue
            // msgToRequeue.add(message);
            // }
            // else
            // {
            // release to allow it to be delivered

            // Without any details from the client about what has been processed we have to mark
            // all messages in the unacked map as redelivered.
            message.setRedelivered();

            Subscription sub = message.getDeliveredSubscription();

            if (sub != null)
            {

                if(!queue.resend(message,sub))
                {
                    msgToRequeue.put(deliveryTag, message);
                }
            }
            else
            {

                if (_logger.isInfoEnabled())
                {
                    _logger.info("DeliveredSubscription not recorded so just requeueing(" + message.toString()
                              + ")to prevent loss");
                }
                // move this message to requeue
                msgToRequeue.put(deliveryTag, message);
            }
        } // for all messages
        // } else !isSuspend

        if (_logger.isInfoEnabled())
        {
            if (!msgToRequeue.isEmpty())
            {
                _logger.info("Preparing (" + msgToRequeue.size() + ") message to requeue to.");
            }
        }

        // Process Messages to Requeue at the front of the queue
        for (Map.Entry<Long, QueueEntry> entry : msgToRequeue.entrySet())
        {
            QueueEntry message = entry.getValue();
            long deliveryTag = entry.getKey();
            _unacknowledgedMessageMap.remove(deliveryTag);

            message.setRedelivered();
            message.release();

        }
    }


    /**
     * Acknowledge one or more messages.
     *
     * @param deliveryTag the last delivery tag
     * @param multiple    if true will acknowledge all messages up to an including the delivery tag. if false only
     *                    acknowledges the single message specified by the delivery tag
     *
     * @throws AMQException if the delivery tag is unknown (e.g. not outstanding) on this channel
     */
    public void acknowledgeMessage(long deliveryTag, boolean multiple) throws AMQException
    {
        Collection<QueueEntry> ackedMessages = getAckedMessages(deliveryTag, multiple);
        _transaction.dequeue(ackedMessages, new MessageAcknowledgeAction(ackedMessages));
    }

    private Collection<QueueEntry> getAckedMessages(long deliveryTag, boolean multiple)
    {

        Map<Long, QueueEntry> ackedMessageMap = new LinkedHashMap<Long,QueueEntry>();
        _unacknowledgedMessageMap.collect(deliveryTag, multiple, ackedMessageMap);
        _unacknowledgedMessageMap.remove(ackedMessageMap);
        return ackedMessageMap.values();
    }

    /**
     * Used only for testing purposes.
     *
     * @return the map of unacknowledged messages
     */
    public UnacknowledgedMessageMap getUnacknowledgedMessageMap()
    {
        return _unacknowledgedMessageMap;
    }

    /**
     * Called from the ChannelFlowHandler to suspend this Channel
     * @param suspended boolean, should this Channel be suspended
     */
    public void setSuspended(boolean suspended)
    {
        boolean wasSuspended = _suspended.getAndSet(suspended);
        if (wasSuspended != suspended)
        {
            // Log Flow Started before we start the subscriptions
            if (!suspended)
            {
                _actor.message(_logSubject, ChannelMessages.CHN_FLOW("Started"));
            }


            // This section takes two different approaches to perform to perform
            // the same function. Ensuring that the Subscription has taken note
            // of the change in Channel State

            // Here we have become unsuspended and so we ask each the queue to
            // perform an Async delivery for each of the subscriptions in this
            // Channel. The alternative would be to ensure that the subscription
            // had received the change in suspension state. That way the logic
            // behind decieding to start an async delivery was located with the
            // Subscription.
            if (wasSuspended)
            {
                // may need to deliver queued messages
                for (Subscription s : _tag2SubscriptionMap.values())
                {
                    s.getQueue().deliverAsync(s);
                }
            }


            // Here we have become suspended so we need to ensure that each of
            // the Subscriptions has noticed this change so that we can be sure
            // they are not still sending messages. Again the code here is a
            // very simplistic approach to ensure that the change of suspension
            // has been noticed by each of the Subscriptions. Unlike the above
            // case we don't actually need to do anything else.
            if (!wasSuspended)
            {
                // may need to deliver queued messages
                for (Subscription s : _tag2SubscriptionMap.values())
                {
                    try
                    {
                        s.getSendLock();
                    }
                    finally
                    {
                        s.releaseSendLock();
                    }
                }
            }


            // Log Suspension only after we have confirmed all suspensions are
            // stopped.
            if (suspended)
            {
                _actor.message(_logSubject, ChannelMessages.CHN_FLOW("Stopped"));
            }

        }
    }

    public boolean isSuspended()
    {
        return _suspended.get()  || _closing || _session.isClosing();
    }

    public void commit() throws AMQException
    {
        if (!isTransactional())
        {
            throw new AMQException("Fatal error: commit called on non-transactional channel");
        }

        _transaction.commit();

    }

    public void rollback() throws AMQException
    {
        rollback(NULL_TASK);
    }

    public void rollback(Runnable postRollbackTask) throws AMQException
    {
        if (!isTransactional())
        {
            throw new AMQException("Fatal error: commit called on non-transactional channel");
        }

        // stop all subscriptions
        _rollingBack = true;
        boolean requiresSuspend = _suspended.compareAndSet(false,true);

        // ensure all subscriptions have seen the change to the channel state
        for(Subscription sub : _tag2SubscriptionMap.values())
        {
            sub.getSendLock();
            sub.releaseSendLock();
        }

        try
        {
            _transaction.rollback();
        }
        finally
        {
            _rollingBack = false;
        }

        postRollbackTask.run();

        for(QueueEntry entry : _resendList)
        {
            Subscription sub = entry.getDeliveredSubscription();
            if(sub == null || sub.isClosed())
            {
                entry.release();
            }
            else
            {
                sub.getQueue().resend(entry, sub);
            }
        }
        _resendList.clear();

        if(requiresSuspend)
        {
            _suspended.set(false);
            for(Subscription sub : _tag2SubscriptionMap.values())
            {
                sub.getQueue().deliverAsync(sub);
            }

        }


    }

    public String toString()
    {
        return "["+_session.toString()+":"+_channelId+"]";
    }

    public void setDefaultQueue(AMQQueue queue)
    {
        _defaultQueue = queue;
    }

    public AMQQueue getDefaultQueue()
    {
        return _defaultQueue;
    }


    public boolean isClosing()
    {
        return _closing;
    }

    public AMQProtocolSession getProtocolSession()
    {
        return _session;
    }

    public FlowCreditManager getCreditManager()
    {
        return _creditManager;
    }

    public void setCredit(final long prefetchSize, final int prefetchCount)
    {
        _actor.message(ChannelMessages.CHN_PREFETCH_SIZE(prefetchSize, prefetchCount));
        _creditManager.setCreditLimits(prefetchSize, prefetchCount);
    }

    public MessageStore getMessageStore()
    {
        return _messageStore;
    }

    private final ClientDeliveryMethod _clientDeliveryMethod = new ClientDeliveryMethod()
        {

            public void deliverToClient(final Subscription sub, final QueueEntry entry, final long deliveryTag)
                    throws AMQException
            {
                getProtocolSession().getProtocolOutputConverter().writeDeliver(entry, getChannelId(),
                                                                               deliveryTag, sub.getConsumerTag());
            }

        };

    public ClientDeliveryMethod getClientDeliveryMethod()
    {
        return _clientDeliveryMethod;
    }

    private final RecordDeliveryMethod _recordDeliveryMethod = new RecordDeliveryMethod()
        {

            public void recordMessageDelivery(final Subscription sub, final QueueEntry entry, final long deliveryTag)
            {
                addUnacknowledgedMessage(entry, deliveryTag, sub);
            }
        };

    public RecordDeliveryMethod getRecordDeliveryMethod()
    {
        return _recordDeliveryMethod;
    }


    private AMQMessage createAMQMessage(IncomingMessage incomingMessage)
            throws AMQException
    {

        AMQMessage message = new AMQMessage(incomingMessage.getStoredMessage());

        message.setExpiration(incomingMessage.getExpiration());
        message.setClientIdentifier(_session);
        return message;
    }

    private boolean checkMessageUserId(ContentHeaderBody header)
    {
        AMQShortString userID =
                header.properties instanceof BasicContentHeaderProperties
                    ? ((BasicContentHeaderProperties) header.properties).getUserId()
                    : null;

        return (!MSG_AUTH || _session.getPrincipal().getName().equals(userID == null? "" : userID.toString()));

    }

    private class MessageDeliveryAction implements ServerTransaction.Action
    {
        private IncomingMessage _incommingMessage;
        private ArrayList<AMQQueue> _destinationQueues;

        public MessageDeliveryAction(IncomingMessage currentMessage,
                                     ArrayList<AMQQueue> destinationQueues)
        {
            _incommingMessage = currentMessage;
            _destinationQueues = destinationQueues;
        }

        public void postCommit()
        {
            try
            {
                final boolean immediate = _incommingMessage.isImmediate();

                final AMQMessage amqMessage = createAMQMessage(_incommingMessage);
                MessageReference ref = amqMessage.newReference();

                for(AMQQueue queue : _destinationQueues)
                {

                    QueueEntry entry = queue.enqueue(amqMessage);
                    queue.checkCapacity(AMQChannel.this);


                    if(immediate && !entry.getDeliveredToConsumer() && entry.acquire())
                    {


                        ServerTransaction txn = new LocalTransaction(_messageStore);
                        Collection<QueueEntry> entries = new ArrayList<QueueEntry>(1);
                        entries.add(entry);
                        final AMQMessage message = (AMQMessage) entry.getMessage();
                        txn.dequeue(queue, entry.getMessage(),
                                    new MessageAcknowledgeAction(entries)
                                    {
                                        @Override
                                        public void postCommit()
                                        {
                                            try
                                            {
                                                final
                                                ProtocolOutputConverter outputConverter =
                                                                            _session.getProtocolOutputConverter();

                                                outputConverter.writeReturn(message.getMessagePublishInfo(),
                                                                            message.getContentHeaderBody(),
                                                                            message,
                                                                            _channelId,
                                                                            AMQConstant.NO_CONSUMERS.getCode(),
                                                                            IMMEDIATE_DELIVERY_REPLY_TEXT);
                                            }
                                            catch (AMQException e)
                                            {
                                                throw new RuntimeException(e);
                                            }
                                            super.postCommit();
                                        }
                                    }
                        );
                        txn.commit();




                    }

                }
                ref.release();
            }
            catch (AMQException e)
            {
                // TODO
                throw new RuntimeException(e);
            }





        }

        public void onRollback()
        {
            // Maybe keep track of entries that were created and then delete them here in case of failure
            // to in memory enqueue
        }
    }

    private class MessageAcknowledgeAction implements ServerTransaction.Action
    {
        private final Collection<QueueEntry> _ackedMessages;


        public MessageAcknowledgeAction(Collection<QueueEntry> ackedMessages)
        {
            _ackedMessages = ackedMessages;
        }

        public void postCommit()
        {
            try
            {
                for(QueueEntry entry : _ackedMessages)
                {
                    entry.discard();
                }
            }
            finally
            {
                _acknowledgedMessages.clear();
            }

        }

        public void onRollback()
        {
            // explicit rollbacks resend the message after the rollback-ok is sent
            if(_rollingBack)
            {
                 _resendList.addAll(_ackedMessages);
            }
            else
            {
                try
                {
                        for(QueueEntry entry : _ackedMessages)
                        {
                            entry.release();
                        }
                }
                finally
                {
                    _acknowledgedMessages.clear();
                }
            }

        }
    }

    private class WriteReturnAction implements ServerTransaction.Action
    {
        private final AMQConstant _errorCode;
        private final IncomingMessage _message;
        private final String _description;

        public WriteReturnAction(AMQConstant errorCode,
                                 String description,
                                 IncomingMessage message)
        {
            _errorCode = errorCode;
            _message = message;
            _description = description;
        }

        public void postCommit()
        {
            try
            {
                _session.getProtocolOutputConverter().writeReturn(_message.getMessagePublishInfo(),
                                                              _message.getContentHeader(),
                                                              _message,
                                                              _channelId,
                                                              _errorCode.getCode(),
                                                             new AMQShortString(_description));
            }
            catch (AMQException e)
            {
                //TODO
                throw new RuntimeException(e);
            }

        }

        public void onRollback()
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }


    public LogActor getLogActor()
    {
        return _actor;
    }

    public void block(AMQQueue queue)
    {
        if(_blockingQueues.putIfAbsent(queue, Boolean.TRUE) == null)
        {

            if(_blocking.compareAndSet(false,true))
            {
                _actor.message(_logSubject, ChannelMessages.CHN_FLOW_ENFORCED(queue.getName().toString()));
                flow(false);
            }
        }
    }

    public void unblock(AMQQueue queue)
    {
        if(_blockingQueues.remove(queue))
        {
            if(_blocking.compareAndSet(true,false))
            {
                _actor.message(_logSubject, ChannelMessages.CHN_FLOW_REMOVED());

                flow(true);
            }
        }
    }

    private void flow(boolean flow)
    {
        MethodRegistry methodRegistry = _session.getMethodRegistry();
        AMQMethodBody responseBody = methodRegistry.createChannelFlowBody(flow);
        _session.writeFrame(responseBody.generateFrame(_channelId));
    }
    
    public boolean getBlocking()
    {
        return _blocking.get();
    }
}
