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
package org.apache.qpid.client;

import org.apache.qpid.AMQException;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.client.message.*;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.framing.*;
import org.apache.qpid.jms.MessageConsumer;
import org.apache.qpid.jms.Session;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.TreeSet;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public abstract class BasicMessageConsumer<U> extends Closeable implements MessageConsumer
{
    private static final Logger _logger = LoggerFactory.getLogger(BasicMessageConsumer.class);

    /** The connection being used by this consumer */
    protected final AMQConnection _connection;

    protected final String _messageSelector;

    private final boolean _noLocal;

    private final AMQDestination _destination;

    /**
     * When true indicates that a blocking receive call is in progress
     */
    private final AtomicBoolean _receiving = new AtomicBoolean(false);
    /**
     * Holds an atomic reference to the listener installed.
     */
    private final AtomicReference<MessageListener> _messageListener = new AtomicReference<MessageListener>();

    /** The consumer tag allows us to close the consumer by sending a jmsCancel method to the broker */
    protected int _consumerTag;

    /** We need to know the channel id when constructing frames */
    protected final int _channelId;

    /**
     * Used in the blocking receive methods to receive a message from the Session thread. <p/> Or to notify of errors
     * <p/> Argument true indicates we want strict FIFO semantics
     */
    protected final BlockingQueue _synchronousQueue;

    protected final MessageFactoryRegistry _messageFactory;

    protected final AMQSession _session;

    protected final AMQProtocolHandler _protocolHandler;

    /**
     * We need to store the "raw" field table so that we can resubscribe in the event of failover being required
     */
    private final FieldTable _arguments;

    /**
     * We store the high water prefetch field in order to be able to reuse it when resubscribing in the event of
     * failover
     */
    private final int _prefetchHigh;

    /**
     * We store the low water prefetch field in order to be able to reuse it when resubscribing in the event of
     * failover
     */
    private final int _prefetchLow;

    /**
     * We store the exclusive field in order to be able to reuse it when resubscribing in the event of failover
     */
    private final boolean _exclusive;

    /**
     * The acknowledge mode in force for this consumer. Note that the AMQP protocol allows different ack modes per
     * consumer whereas JMS defines this at the session level, hence why we associate it with the consumer in our
     * implementation.
     */
    protected final int _acknowledgeMode;

    /**
     * Number of messages unacknowledged in DUPS_OK_ACKNOWLEDGE mode
     */
    private int _outstanding;

    /**
     * Switch to enable sending of acknowledgements when using DUPS_OK_ACKNOWLEDGE mode. Enabled when _outstannding
     * number of msgs >= _prefetchHigh and disabled at < _prefetchLow
     */
    private boolean _dups_ok_acknowledge_send;

    /**
     * List of tags delievered, The last of which which should be acknowledged on commit in transaction mode.
     */
    private ConcurrentLinkedQueue<Long> _receivedDeliveryTags = new ConcurrentLinkedQueue<Long>();

    /** The last tag that was "multiple" acknowledged on this session (if transacted) */
    private long _lastAcked;

    /** set of tags which have previously been acked; but not part of the multiple ack (transacted mode only) */
    private final SortedSet<Long> _previouslyAcked = new TreeSet<Long>();

    private final Object _commitLock = new Object();

    /**
     * The thread that was used to call receive(). This is important for being able to interrupt that thread if a
     * receive() is in progress.
     */
    private Thread _receivingThread;


    /**
     * Used to store this consumer queue name
     * Usefull when more than binding key should be used
     */
    private AMQShortString _queuename;

    /**
     * autoClose denotes that the consumer will automatically cancel itself when there are no more messages to receive
     * on the queue.  This is used for queue browsing.
     */
    private final boolean _autoClose;

    private final boolean _noConsume;
    private List<StackTraceElement> _closedStack = null;



    protected BasicMessageConsumer(int channelId, AMQConnection connection, AMQDestination destination,
                                   String messageSelector, boolean noLocal, MessageFactoryRegistry messageFactory,
                                   AMQSession session, AMQProtocolHandler protocolHandler,
                                   FieldTable arguments, int prefetchHigh, int prefetchLow,
                                   boolean exclusive, int acknowledgeMode, boolean noConsume, boolean autoClose)
    {
        _channelId = channelId;
        _connection = connection;
        _messageSelector = messageSelector;
        _noLocal = noLocal;
        _destination = destination;
        _messageFactory = messageFactory;
        _session = session;
        _protocolHandler = protocolHandler;
        _arguments = arguments;
        _prefetchHigh = prefetchHigh;
        _prefetchLow = prefetchLow;
        _exclusive = exclusive;

        _synchronousQueue = new LinkedBlockingQueue();
        _autoClose = autoClose;
        _noConsume = noConsume;

        // Force queue browsers not to use acknowledge modes.
        if (_noConsume)
        {
            _acknowledgeMode = Session.NO_ACKNOWLEDGE;
        }
        else
        {
            _acknowledgeMode = acknowledgeMode;
        }
    }

    public AMQDestination getDestination()
    {
        return _destination;
    }

    public String getMessageSelector() throws JMSException
    {
        checkPreConditions();

        return _messageSelector;
    }

    public MessageListener getMessageListener() throws JMSException
    {
        checkPreConditions();

        return _messageListener.get();
    }

    public int getAcknowledgeMode()
    {
        return _acknowledgeMode;
    }

    protected boolean isMessageListenerSet()
    {
        return _messageListener.get() != null;
    }

    public void setMessageListener(final MessageListener messageListener) throws JMSException
    {
        checkPreConditions();

        // if the current listener is non-null and the session is not stopped, then
        // it is an error to call this method.

        // i.e. it is only valid to call this method if
        //
        // (a) the connection is stopped, in which case the dispatcher is not running
        // OR
        // (b) the listener is null AND we are not receiving synchronously at present
        //

        if (!_session.getAMQConnection().started())
        {
            _messageListener.set(messageListener);
            _session.setHasMessageListeners();

            if (_logger.isDebugEnabled())
            {
                _logger.debug(
                        "Session stopped : Message listener(" + messageListener + ") set for destination " + _destination);
            }
        }
        else
        {
            if (_receiving.get())
            {
                throw new javax.jms.IllegalStateException("Another thread is already receiving synchronously.");
            }

            if (!_messageListener.compareAndSet(null, messageListener))
            {
                throw new javax.jms.IllegalStateException("Attempt to alter listener while session is started.");
            }

            _logger.debug("Message listener set for destination " + _destination);

            if (messageListener != null)
            {
                //todo: handle case where connection has already been started, and the dispatcher has alreaded started
                // putting values on the _synchronousQueue

                synchronized (_session)
                {
                    _messageListener.set(messageListener);
                    _session.setHasMessageListeners();
                    _session.startDispatcherIfNecessary();
                    
                    // If we already have messages on the queue, deliver them to the listener
                    Object o = _synchronousQueue.poll();
                    while (o != null)
                    {
                        notifyMessage((AbstractJMSMessage) o);
                        o = _synchronousQueue.poll();
                    }
                }
            }
        }
    }

    protected void preApplicationProcessing(AbstractJMSMessage jmsMsg) throws JMSException
    {
        if (_session.getAcknowledgeMode() == Session.CLIENT_ACKNOWLEDGE)
        {
            _session.addUnacknowledgedMessage(jmsMsg.getDeliveryTag());
        }
        
        _session.setInRecovery(false);
        preDeliver(jmsMsg);
    }

    /**
     * @param immediate if true then return immediately if the connection is failing over
     *
     * @return boolean if the acquisition was successful
     *
     * @throws JMSException if a listener has already been set or another thread is receiving
     * @throws InterruptedException if interrupted
     */
    private boolean acquireReceiving(boolean immediate) throws JMSException, InterruptedException
    {
        if (_connection.isFailingOver())
        {
            if (immediate)
            {
                return false;
            }
            else
            {
                _connection.blockUntilNotFailingOver();
            }
        }

        if (!_receiving.compareAndSet(false, true))
        {
            throw new javax.jms.IllegalStateException("Another thread is already receiving.");
        }

        if (isMessageListenerSet())
        {
            throw new javax.jms.IllegalStateException("A listener has already been set.");
        }

        _receivingThread = Thread.currentThread();
        return true;
    }

    private void releaseReceiving()
    {
        _receiving.set(false);
        _receivingThread = null;
    }

    public FieldTable getArguments()
    {
        return _arguments;
    }

    public int getPrefetch()
    {
        return _prefetchHigh;
    }

    public int getPrefetchHigh()
    {
        return _prefetchHigh;
    }

    public int getPrefetchLow()
    {
        return _prefetchLow;
    }

    public boolean isNoLocal()
    {
        return _noLocal;
    }

    public boolean isExclusive()
    {
        return _exclusive;
    }

    public boolean isReceiving()
    {
        return _receiving.get();
    }

    public Message receive() throws JMSException
    {
        return receive(0);
    }

    public Message receive(long l) throws JMSException
    {

        checkPreConditions();

        try
        {
            acquireReceiving(false);
        }
        catch (InterruptedException e)
        {
            _logger.warn("Interrupted acquire: " + e);
            if (isClosed())
            {
                return null;
            }
        }

        _session.startDispatcherIfNecessary();

        try
        {
            Object o = getMessageFromQueue(l);
            final AbstractJMSMessage m = returnMessageOrThrow(o);
            if (m != null)
            {
                preApplicationProcessing(m);
                postDeliver(m);
            }
            return m;
        }
        catch (InterruptedException e)
        {
            _logger.warn("Interrupted: " + e);

            return null;
        }
        finally
        {
            releaseReceiving();
        }
    }

    public  Object getMessageFromQueue(long l) throws InterruptedException
    {
         Object o;
         if (l > 0)
         {
             o = _synchronousQueue.poll(l, TimeUnit.MILLISECONDS);
         }
         else if (l < 0)
         {
             o = _synchronousQueue.poll();
         }
         else
         {
             o = _synchronousQueue.take();
         }
         return o;
    }

    abstract Message receiveBrowse() throws JMSException;

    public Message receiveNoWait() throws JMSException
    {
        checkPreConditions();

        try
        {
            if (!acquireReceiving(true))
            {
                //If we couldn't acquire the receiving thread then return null.
                // This will occur if failing over.
                return null;
            }
        }
        catch (InterruptedException e)
        {
            /*
             *  This seems slightly shoddy but should never actually be executed
             *  since we told acquireReceiving to return immediately and it shouldn't
             *  block on anything.
             */

            return null;
        }

        _session.startDispatcherIfNecessary();

        try
        {
            Object o = getMessageFromQueue(-1);
            final AbstractJMSMessage m = returnMessageOrThrow(o);
            if (m != null)
            {
                preApplicationProcessing(m);
                postDeliver(m);
            }

            return m;
        }
        catch (InterruptedException e)
        {
            _logger.warn("Interrupted: " + e);

            return null;
        }
        finally
        {
            releaseReceiving();
        }
    }

    /**
     * We can get back either a Message or an exception from the queue. This method examines the argument and deals with
     * it by throwing it (if an exception) or returning it (in any other case).
     *
     * @param o the object to return or throw
     * @return a message only if o is a Message
     * @throws JMSException if the argument is a throwable. If it is a JMSException it is rethrown as is, but if not a
     *                      JMSException is created with the linked exception set appropriately
     */
    private AbstractJMSMessage returnMessageOrThrow(Object o) throws JMSException
    {
        // errors are passed via the queue too since there is no way of interrupting the poll() via the API.
        if (o instanceof Throwable)
        {
            JMSException e = new JMSException("Message consumer forcibly closed due to error: " + o);
            if (o instanceof Exception)
            {
                e.setLinkedException((Exception) o);
            }

            throw e;
        }
        else if (o instanceof CloseConsumerMessage)
        {
            _closed.set(true);
            deregisterConsumer();
            return null;
        }
        else
        {
            return (AbstractJMSMessage) o;
        }
    }

    public void close() throws JMSException
    {
        close(true);
    }

    public void close(boolean sendClose) throws JMSException
    {
        if (_logger.isInfoEnabled())
        {
            _logger.info("Closing consumer:" + debugIdentity());
        }

        if (!_closed.getAndSet(true))
        {
            _closing.set(true);
            if (_logger.isDebugEnabled())
            {
                StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
                if (_closedStack != null)
                {
                    _logger.debug(_consumerTag + " previously:" + _closedStack.toString());
                }
                else
                {
                    _closedStack = Arrays.asList(stackTrace).subList(3, stackTrace.length - 1);
                }
            }

            if (sendClose)
            {
                // The Synchronized block only needs to protect network traffic.
                synchronized (_connection.getFailoverMutex())
                {
                    try
                    {
                        // If the session is open or we are in the process
                        // of closing the session then send a cance
                        // no point otherwise as the connection will be gone
                        if (!_session.isClosed() || _session.isClosing())
                        {
                            sendCancel();
                        }
                    }
                    catch (AMQException e)
                    {
                        throw new JMSAMQException("Error closing consumer: " + e, e);
                    }
                    catch (FailoverException e)
                    {
                        throw new JMSAMQException("FailoverException interrupted basic cancel.", e);
                    }
                }
            }
            else
            {
            	// FIXME: wow this is ugly
                // //fixme this probably is not right
                // if (!isNoConsume())
                { // done in BasicCancelOK Handler but not sending one so just deregister.
                    deregisterConsumer();
                }
            }

            // This will occur if session.close is called closing all consumers we may be blocked waiting for a receive
            // so we need to let it know it is time to close.
            if ((_messageListener != null) && _receiving.get())
            {
                if (_logger.isInfoEnabled())
                {
                    _logger.info("Interrupting thread: " + _receivingThread);
                }

                _receivingThread.interrupt();
            }
        }
    }

    abstract void sendCancel() throws AMQException, FailoverException;

    /**
     * Called when you need to invalidate a consumer. Used for example when failover has occurred and the client has
     * vetoed automatic resubscription. The caller must hold the failover mutex.
     */
    void markClosed()
    {
        // synchronized (_closed)
        {
            _closed.set(true);

            if (_logger.isDebugEnabled())
            {
                if (_closedStack != null)
                {
                    StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
                    _logger.debug(_consumerTag + " markClosed():"
                                  + Arrays.asList(stackTrace).subList(3, stackTrace.length - 1));
                    _logger.debug(_consumerTag + " previously:" + _closedStack.toString());
                }
                else
                {
                	StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
                    _closedStack = Arrays.asList(stackTrace).subList(3, stackTrace.length - 1);
                }
            }
        }

        deregisterConsumer();
    }

    /**
     * @param closeMessage
     *            this message signals that we should close the browser
     */
    public void notifyCloseMessage(CloseConsumerMessage closeMessage)
    {
        if (isMessageListenerSet())
        {
            // Currently only possible to get this msg type with a browser.
            // If we get the message here then we should probably just close
            // this consumer.
            // Though an AutoClose consumer with message listener is quite odd..
            // Just log out the fact so we know where we are
            _logger.warn("Using an AutoCloseconsumer with message listener is not supported.");
        }
        else
        {
            try
            {
                _synchronousQueue.put(closeMessage);
            }
            catch (InterruptedException e)
            {
                _logger.info(" SynchronousQueue.put interupted. Usually result of connection closing,"
                        + "but we shouldn't have close yet");
            }
        }
    }

    
    /**
     * Called from the AMQSession when a message has arrived for this consumer. This methods handles both the case of a
     * message listener or a synchronous receive() caller.
     *
     * @param messageFrame the raw unprocessed mesage
     */
    void notifyMessage(U messageFrame)
    {
        if (messageFrame instanceof CloseConsumerMessage)
        {
            notifyCloseMessage((CloseConsumerMessage) messageFrame);
            return;
        }



        try
        {
            AbstractJMSMessage jmsMessage = createJMSMessageFromUnprocessedMessage(_session.getMessageDelegateFactory(), messageFrame);

            if (_logger.isDebugEnabled())
            {
                _logger.debug("Message is of type: " + jmsMessage.getClass().getName());
            }
            // synchronized (_closed)

            {
                // if (!_closed.get())
                {

                    //preDeliver(jmsMessage);

                    notifyMessage(jmsMessage);
                }
                // else
                // {
                // _logger.error("MESSAGE REJECTING!");
                // _session.rejectMessage(jmsMessage, true);
                // //_logger.error("MESSAGE JUST DROPPED!");
                // }
            }
        }
        catch (Exception e)
        {
            if (e instanceof InterruptedException)
            {
                _logger.info("SynchronousQueue.put interupted. Usually result of connection closing");
            }
            else
            {
                _logger.error("Caught exception (dump follows) - ignoring...", e);
            }
        }
    }

    public abstract AbstractJMSMessage createJMSMessageFromUnprocessedMessage(AMQMessageDelegateFactory delegateFactory, U messageFrame)
            throws Exception;

    /** @param jmsMessage this message has already been processed so can't redo preDeliver */
    public void notifyMessage(AbstractJMSMessage jmsMessage)
    {
        try
        {
            if (isMessageListenerSet())
            {
                preApplicationProcessing(jmsMessage);
                getMessageListener().onMessage(jmsMessage);
                postDeliver(jmsMessage);
            }
            else
            {
                // we should not be allowed to add a message is the
                // consumer is closed
                _synchronousQueue.put(jmsMessage);
            }
        }
        catch (Exception e)
        {
            if (e instanceof InterruptedException)
            {
                _logger.info("reNotification : SynchronousQueue.put interupted. Usually result of connection closing");
            }
            else
            {
                _logger.error("reNotification : Caught exception (dump follows) - ignoring...", e);
            }
        }
    }

    void preDeliver(AbstractJMSMessage msg)
    {
        switch (_acknowledgeMode)
        {

            case Session.PRE_ACKNOWLEDGE:
                _session.acknowledgeMessage(msg.getDeliveryTag(), false);
                break;

            case Session.CLIENT_ACKNOWLEDGE:
                // we set the session so that when the user calls acknowledge() it can call the method on session
                // to send out the appropriate frame
                msg.setAMQSession(_session);
                break;
            case Session.SESSION_TRANSACTED:
                if (isNoConsume())
                {
                    _session.acknowledgeMessage(msg.getDeliveryTag(), false);
                }
                else
                {
                    _session.addDeliveredMessage(msg.getDeliveryTag());
                    _session.markDirty();
                }

                break;
        }

    }

    void postDeliver(AbstractJMSMessage msg) throws JMSException
    {
        switch (_acknowledgeMode)
        {

            case Session.CLIENT_ACKNOWLEDGE:
                if (isNoConsume())
                {
                    _session.acknowledgeMessage(msg.getDeliveryTag(), false);
                }
                _session.markDirty();
                break;

            case Session.DUPS_OK_ACKNOWLEDGE:
            case Session.AUTO_ACKNOWLEDGE:
                // we do not auto ack a message if the application code called recover()
                if (!_session.isInRecovery())
                {
                    _session.acknowledgeMessage(msg.getDeliveryTag(), false);
                }

                break;
        }
    }


    /**
     * Acknowledge up to last message delivered (if any). Used when commiting.
     *
     * @return the lastDeliveryTag to acknowledge
     */
    Long getLastDelivered()
    {
        if (!_receivedDeliveryTags.isEmpty())
        {
            Long lastDeliveryTag = _receivedDeliveryTags.poll();

            while (!_receivedDeliveryTags.isEmpty())
            {
                lastDeliveryTag = _receivedDeliveryTags.poll();
            }

            assert _receivedDeliveryTags.isEmpty();

            return lastDeliveryTag;
        }

        return null;
    }

    /**
     * Acknowledge up to last message delivered (if any). Used when commiting.
     */
    void acknowledgeDelivered()
    {
        synchronized(_commitLock)
        {
            ArrayList<Long> tagsToAck = new ArrayList<Long>();

            while (!_receivedDeliveryTags.isEmpty())
            {
                tagsToAck.add(_receivedDeliveryTags.poll());
            }

            Collections.sort(tagsToAck);

            long prevAcked = _lastAcked;
            long oldAckPoint = -1;

            while(oldAckPoint != prevAcked)
            {
                oldAckPoint = prevAcked;

                Iterator<Long> tagsToAckIterator = tagsToAck.iterator();

                while(tagsToAckIterator.hasNext() && tagsToAckIterator.next() == prevAcked+1)
                {
                    tagsToAckIterator.remove();
                    prevAcked++;
                }

                Iterator<Long> previousAckIterator = _previouslyAcked.iterator();
                while(previousAckIterator.hasNext() && previousAckIterator.next() == prevAcked+1)
                {
                    previousAckIterator.remove();
                    prevAcked++;
                }

            }
            if(prevAcked != _lastAcked)
            {
                _session.acknowledgeMessage(prevAcked, true);
                _lastAcked = prevAcked;
            }

            Iterator<Long> tagsToAckIterator = tagsToAck.iterator();

            while(tagsToAckIterator.hasNext())
            {
                Long tag = tagsToAckIterator.next();
                _session.acknowledgeMessage(tag, false);
                _previouslyAcked.add(tag);
            }
        }
    }


    void notifyError(Throwable cause)
    {
        // synchronized (_closed)
        {
            _closed.set(true);
            if (_logger.isDebugEnabled())
            {
                StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
                if (_closedStack != null)
                {
                    _logger.debug(_consumerTag + " notifyError():"
                                  + Arrays.asList(stackTrace).subList(3, stackTrace.length - 1));
                    _logger.debug(_consumerTag + " previously" + _closedStack.toString());
                }
                else
                {
                    _closedStack = Arrays.asList(stackTrace).subList(3, stackTrace.length - 1);
                }
            }
        }
        // QPID-293 can "request redelivery of this error through dispatcher"

        // we have no way of propagating the exception to a message listener - a JMS limitation - so we
        // deal with the case where we have a synchronous receive() waiting for a message to arrive
        if (!isMessageListenerSet())
        {
            // offer only succeeds if there is a thread waiting for an item from the queue
            if (_synchronousQueue.offer(cause))
            {
                _logger.debug("Passed exception to synchronous queue for propagation to receive()");
            }
        }

        deregisterConsumer();
    }

    /**
     * Perform cleanup to deregister this consumer. This occurs when closing the consumer in both the clean case and in
     * the case of an error occurring.
     */
    private void deregisterConsumer()
    {
        _session.deregisterConsumer(this);
    }

    public int getConsumerTag()
    {
        return _consumerTag;
    }

    public void setConsumerTag(int consumerTag)
    {
        _consumerTag = consumerTag;
    }

    public AMQSession getSession()
    {
        return _session;
    }

    private void checkPreConditions() throws JMSException
    {

        this.checkNotClosed();

        if ((_session == null) || _session.isClosed())
        {
            throw new javax.jms.IllegalStateException("Invalid Session");
        }
    }

    public boolean isAutoClose()
    {
        return _autoClose;
    }

    public boolean isNoConsume()
    {
        return _noConsume;
    }

    public void rollback()
    {
        rollbackPendingMessages();
    }

    public void rollbackPendingMessages()
    {
        if (_synchronousQueue.size() > 0)
        {
            if (_logger.isDebugEnabled())
            {
                _logger.debug("Rejecting the messages(" + _synchronousQueue
                        .size() + ") in _syncQueue (PRQ)" + "for consumer with tag:" + _consumerTag);
            }

            Iterator iterator = _synchronousQueue.iterator();

            int initialSize = _synchronousQueue.size();

            boolean removed = false;
            while (iterator.hasNext())
            {

                Object o = iterator.next();
                if (o instanceof AbstractJMSMessage)
                {
                    _session.rejectMessage(((AbstractJMSMessage) o), true);

                    if (_logger.isDebugEnabled())
                    {
                        _logger.debug("Rejected message:" + ((AbstractJMSMessage) o).getDeliveryTag());
                    }

                    iterator.remove();
                    removed = true;

                }
                else
                {
                    _logger.error("Queue contained a :" + o.getClass()
                                  + " unable to reject as it is not an AbstractJMSMessage. Will be cleared");
                    iterator.remove();
                    removed = true;
                }
                }

            if (removed && (initialSize == _synchronousQueue.size()))
            {
                _logger.error("Queue had content removed but didn't change in size." + initialSize);
            }


            if (_synchronousQueue.size() != 0)
            {
                _logger.warn("Queue was not empty after rejecting all messages Remaining:" + _synchronousQueue.size());
                rollback();
            }

            clearReceiveQueue();
        }
    }

    public String debugIdentity()
    {
        return String.valueOf(_consumerTag) + "[" + System.identityHashCode(this) + "]";
    }

    public void clearReceiveQueue()
    {
        _synchronousQueue.clear();
    }

    public AMQShortString getQueuename()
    {
        return _queuename;
    }

    public void setQueuename(AMQShortString queuename)
    {
        this._queuename = queuename;
    }

    public void addBindingKey(AMQDestination amqd, String routingKey) throws AMQException
    {
        _session.addBindingKey(this,amqd,routingKey);
    }

    /** to be called when a failover has occured */
    public void failedOverPre()
    {
        clearReceiveQueue();
        // TGM FIXME: think this should just be removed
        // clearUnackedMessages();
    }

    public void failedOverPost() {}

}
