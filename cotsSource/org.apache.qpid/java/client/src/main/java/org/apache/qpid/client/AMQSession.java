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

import java.io.Serializable;
import java.net.URISyntaxException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.IllegalStateException;
import javax.jms.InvalidDestinationException;
import javax.jms.InvalidSelectorException;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.ObjectMessage;
import javax.jms.Queue;
import javax.jms.QueueBrowser;
import javax.jms.QueueReceiver;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.StreamMessage;
import javax.jms.TemporaryQueue;
import javax.jms.TemporaryTopic;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;
import javax.jms.TransactionRolledBackException;

import org.apache.qpid.AMQDisconnectedException;
import org.apache.qpid.AMQException;
import org.apache.qpid.AMQInvalidArgumentException;
import org.apache.qpid.AMQInvalidRoutingKeyException;
import org.apache.qpid.AMQChannelClosedException;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.client.failover.FailoverNoopSupport;
import org.apache.qpid.client.failover.FailoverProtectedOperation;
import org.apache.qpid.client.failover.FailoverRetrySupport;
import org.apache.qpid.client.message.AMQMessageDelegateFactory;
import org.apache.qpid.client.message.AbstractJMSMessage;
import org.apache.qpid.client.message.CloseConsumerMessage;
import org.apache.qpid.client.message.JMSBytesMessage;
import org.apache.qpid.client.message.JMSMapMessage;
import org.apache.qpid.client.message.JMSObjectMessage;
import org.apache.qpid.client.message.JMSStreamMessage;
import org.apache.qpid.client.message.JMSTextMessage;
import org.apache.qpid.client.message.MessageFactoryRegistry;
import org.apache.qpid.client.message.UnprocessedMessage;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.client.state.AMQState;
import org.apache.qpid.client.state.AMQStateManager;
import org.apache.qpid.client.util.FlowControllingBlockingQueue;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.FieldTableFactory;
import org.apache.qpid.framing.MethodRegistry;
import org.apache.qpid.jms.Session;
import org.apache.qpid.thread.Threading;
import org.apache.qpid.url.AMQBindingURL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td>
 * </table>
 *
 * @todo Different FailoverSupport implementation are needed on the same method call, in different situations. For
 * example, when failing-over and reestablishing the bindings, the bind cannot be interrupted by a second
 * fail-over, if it fails with an exception, the fail-over process should also fail. When binding outside of
 * the fail-over process, the retry handler could be used to automatically retry the operation once the connection
 * has been reestablished. All fail-over protected operations should be placed in private methods, with
 * FailoverSupport passed in by the caller to provide the correct support for the calling context. Sometimes the
 * fail-over process sets a nowait flag and uses an async method call instead.
 * @todo Two new objects created on every failover supported method call. Consider more efficient ways of doing this,
 * after looking at worse bottlenecks first.
 */
public abstract class AMQSession<C extends BasicMessageConsumer, P extends BasicMessageProducer> extends Closeable implements Session, QueueSession, TopicSession
{

    public static final class IdToConsumerMap<C extends BasicMessageConsumer>
    {
        private final BasicMessageConsumer[] _fastAccessConsumers = new BasicMessageConsumer[16];
        private final ConcurrentHashMap<Integer, C> _slowAccessConsumers = new ConcurrentHashMap<Integer, C>();

        public C get(int id)
        {
            if ((id & 0xFFFFFFF0) == 0)
            {
                return (C) _fastAccessConsumers[id];
            }
            else
            {
                return _slowAccessConsumers.get(id);
            }
        }

        public C put(int id, C consumer)
        {
            C oldVal;
            if ((id & 0xFFFFFFF0) == 0)
            {
                oldVal = (C) _fastAccessConsumers[id];
                _fastAccessConsumers[id] = consumer;
            }
            else
            {
                oldVal = _slowAccessConsumers.put(id, consumer);
            }

            return consumer;

        }

        public C remove(int id)
        {
            C consumer;
            if ((id & 0xFFFFFFF0) == 0)
            {
                consumer = (C) _fastAccessConsumers[id];
                _fastAccessConsumers[id] = null;
            }
            else
            {
                consumer = _slowAccessConsumers.remove(id);
            }

            return consumer;

        }

        public Collection<C> values()
        {
            ArrayList<C> values = new ArrayList<C>();

            for (int i = 0; i < 16; i++)
            {
                if (_fastAccessConsumers[i] != null)
                {
                    values.add((C) _fastAccessConsumers[i]);
                }
            }
            values.addAll(_slowAccessConsumers.values());

            return values;
        }

        public void clear()
        {
            _slowAccessConsumers.clear();
            for (int i = 0; i < 16; i++)
            {
                _fastAccessConsumers[i] = null;
            }
        }
    }

    /** Used for debugging. */
    private static final Logger _logger = LoggerFactory.getLogger(AMQSession.class);

    /**
     * The default value for immediate flag used by producers created by this session is false. That is, a consumer does
     * not need to be attached to a queue.
     */
    protected final boolean DEFAULT_IMMEDIATE = Boolean.parseBoolean(System.getProperty("qpid.default_immediate", "false"));

    /**
     * The default value for mandatory flag used by producers created by this session is true. That is, server will not
     * silently drop messages where no queue is connected to the exchange for the message.
     */
    protected final boolean DEFAULT_MANDATORY = Boolean.parseBoolean(System.getProperty("qpid.default_mandatory", "true"));

    protected final boolean DEFAULT_WAIT_ON_SEND = Boolean.parseBoolean(System.getProperty("qpid.default_wait_on_send", "false"));

    /**
     * The period to wait while flow controlled before sending a log message confirming that the session is still
     * waiting on flow control being revoked
     */
    protected final long FLOW_CONTROL_WAIT_PERIOD = Long.getLong("qpid.flow_control_wait_notify_period",5000L);

    /**
     * The period to wait while flow controlled before declaring a failure
     */
    public static final long DEFAULT_FLOW_CONTROL_WAIT_FAILURE = 120000L;
    protected final long FLOW_CONTROL_WAIT_FAILURE = Long.getLong("qpid.flow_control_wait_failure",
                                                                  DEFAULT_FLOW_CONTROL_WAIT_FAILURE);

    protected final boolean DECLARE_QUEUES =
        Boolean.parseBoolean(System.getProperty("qpid.declare_queues", "true"));

    protected final boolean DECLARE_EXCHANGES =
        Boolean.parseBoolean(System.getProperty("qpid.declare_exchanges", "true"));

    /** System property to enable strict AMQP compliance. */
    public static final String STRICT_AMQP = "STRICT_AMQP";

    /** Strict AMQP default setting. */
    public static final String STRICT_AMQP_DEFAULT = "false";

    /** System property to enable failure if strict AMQP compliance is violated. */
    public static final String STRICT_AMQP_FATAL = "STRICT_AMQP_FATAL";

    /** Strickt AMQP failure default. */
    public static final String STRICT_AMQP_FATAL_DEFAULT = "true";

    /** System property to enable immediate message prefetching. */
    public static final String IMMEDIATE_PREFETCH = "IMMEDIATE_PREFETCH";

    /** Immediate message prefetch default. */
    public static final String IMMEDIATE_PREFETCH_DEFAULT = "false";

    /** The connection to which this session belongs. */
    protected AMQConnection _connection;

    /** Used to indicate whether or not this is a transactional session. */
    protected boolean _transacted;

    /** Holds the sessions acknowledgement mode. */
    protected final int _acknowledgeMode;

    /** Holds this session unique identifier, used to distinguish it from other sessions. */
    protected int _channelId;

    private int _ticket;

    /** Holds the high mark for prefetched message, at which the session is suspended. */
    private int _prefetchHighMark;

    /** Holds the low mark for prefetched messages, below which the session is resumed. */
    private int _prefetchLowMark;

    /** Holds the message listener, if any, which is attached to this session. */
    private MessageListener _messageListener = null;

    /** Used to indicate that this session has been started at least once. */
    private AtomicBoolean _startedAtLeastOnce = new AtomicBoolean(false);

    /**
     * Used to reference durable subscribers so that requests for unsubscribe can be handled correctly.  Note this only
     * keeps a record of subscriptions which have been created in the current instance. It does not remember
     * subscriptions between executions of the client.
     */
    protected final ConcurrentHashMap<String, TopicSubscriberAdaptor> _subscriptions =
            new ConcurrentHashMap<String, TopicSubscriberAdaptor>();

    /**
     * Holds a mapping from message consumers to their identifying names, so that their subscriptions may be looked
     * up in the {@link #_subscriptions} map.
     */
    protected final ConcurrentHashMap<C, String> _reverseSubscriptionMap =
            new ConcurrentHashMap<C, String>();

    /**
     * Used to hold incoming messages.
     *
     * @todo Weaken the type once {@link FlowControllingBlockingQueue} implements Queue.
     */
    protected final FlowControllingBlockingQueue _queue;

    /** Holds the highest received delivery tag. */
    private final AtomicLong _highestDeliveryTag = new AtomicLong(-1);
    private final AtomicLong _rollbackMark = new AtomicLong(-1);

    /** All the not yet acknowledged message tags */
    protected ConcurrentLinkedQueue<Long> _unacknowledgedMessageTags = new ConcurrentLinkedQueue<Long>();

    /** All the delivered message tags */
    protected ConcurrentLinkedQueue<Long> _deliveredMessageTags = new ConcurrentLinkedQueue<Long>();

    /** Holds the dispatcher thread for this session. */
    protected Dispatcher _dispatcher;

    protected Thread _dispatcherThread;

    /** Holds the message factory factory for this session. */
    protected MessageFactoryRegistry _messageFactoryRegistry;

    /** Holds all of the producers created by this session, keyed by their unique identifiers. */
    private Map<Long, MessageProducer> _producers = new ConcurrentHashMap<Long, MessageProducer>();

    /**
     * Used as a source of unique identifiers so that the consumers can be tagged to match them to BasicConsume
     * methods.
     */
    private int _nextTag = 1;

    /**
     * Maps from identifying tags to message consumers, in order to pass dispatch incoming messages to the right
     * consumer.
     */
    protected final IdToConsumerMap<C> _consumers = new IdToConsumerMap<C>();

    //Map<AMQShortString, BasicMessageConsumer> _consumers =
    //new ConcurrentHashMap<AMQShortString, BasicMessageConsumer>();

    /**
     * Contains a list of consumers which have been removed but which might still have
     * messages to acknowledge, eg in client ack or transacted modes
     */
    private CopyOnWriteArrayList<C> _removedConsumers = new CopyOnWriteArrayList<C>();

    /** Provides a count of consumers on destinations, in order to be able to know if a destination has consumers. */
    private ConcurrentHashMap<Destination, AtomicInteger> _destinationConsumerCount =
            new ConcurrentHashMap<Destination, AtomicInteger>();

    /**
     * Used as a source of unique identifiers for producers within the session.
     *
     * <p/> Access to this id does not require to be synchronized since according to the JMS specification only one
     * thread of control is allowed to create producers for any given session instance.
     */
    private long _nextProducerId;

    /**
     * Set when recover is called. This is to handle the case where recover() is called by application code during
     * onMessage() processing to enure that an auto ack is not sent.
     */
    private boolean _inRecovery;

    /** Used to indicates that the connection to which this session belongs, has been stopped. */
    private boolean _connectionStopped;

    /** Used to indicate that this session has a message listener attached to it. */
    private boolean _hasMessageListeners;

    /** Used to indicate that this session has been suspended. */
    private boolean _suspended;

    /**
     * Used to protect the suspension of this session, so that critical code can be executed during suspension,
     * without the session being resumed by other threads.
     */
    private final Object _suspensionLock = new Object();

    /**
     * Used to ensure that onlt the first call to start the dispatcher can unsuspend the channel.
     *
     * @todo This is accessed only within a synchronized method, so does not need to be atomic.
     */
    protected final AtomicBoolean _firstDispatcher = new AtomicBoolean(true);

    /** Used to indicate that the session should start pre-fetching messages as soon as it is started. */
    protected final boolean _immediatePrefetch;

    /** Indicates that warnings should be generated on violations of the strict AMQP. */
    protected final boolean _strictAMQP;

    /** Indicates that runtime exceptions should be generated on vilations of the strict AMQP. */
    protected final boolean _strictAMQPFATAL;
    private final Object _messageDeliveryLock = new Object();

    /** Session state : used to detect if commit is a) required b) allowed , i.e. does the tx span failover. */
    private boolean _dirty;
    /** Has failover occured on this session with outstanding actions to commit? */
    private boolean _failedOverDirty;

    private static final class FlowControlIndicator
    {
        private volatile boolean _flowControl = true;

        public synchronized void setFlowControl(boolean flowControl)
        {
            _flowControl = flowControl;
            notify();
        }

        public boolean getFlowControl()
        {
            return _flowControl;
        }
    }

    /** Flow control */
    private FlowControlIndicator _flowControl = new FlowControlIndicator();

    /**
     * Creates a new session on a connection.
     *
     * @param con                     The connection on which to create the session.
     * @param channelId               The unique identifier for the session.
     * @param transacted              Indicates whether or not the session is transactional.
     * @param acknowledgeMode         The acknoledgement mode for the session.
     * @param messageFactoryRegistry  The message factory factory for the session.
     * @param defaultPrefetchHighMark The maximum number of messages to prefetched before suspending the session.
     * @param defaultPrefetchLowMark  The number of prefetched messages at which to resume the session.
     */
    protected AMQSession(AMQConnection con, int channelId, boolean transacted, int acknowledgeMode,
               MessageFactoryRegistry messageFactoryRegistry, int defaultPrefetchHighMark, int defaultPrefetchLowMark)
    {

        _strictAMQP = Boolean.parseBoolean(System.getProperties().getProperty(STRICT_AMQP, STRICT_AMQP_DEFAULT));
        _strictAMQPFATAL =
                Boolean.parseBoolean(System.getProperties().getProperty(STRICT_AMQP_FATAL, STRICT_AMQP_FATAL_DEFAULT));
        _immediatePrefetch =
                _strictAMQP
                || Boolean.parseBoolean(System.getProperties().getProperty(IMMEDIATE_PREFETCH, IMMEDIATE_PREFETCH_DEFAULT));

        _connection = con;
        _transacted = transacted;
        if (transacted)
        {
            _acknowledgeMode = javax.jms.Session.SESSION_TRANSACTED;
        }
        else
        {
            _acknowledgeMode = acknowledgeMode;
        }

        _channelId = channelId;
        _messageFactoryRegistry = messageFactoryRegistry;
        _prefetchHighMark = defaultPrefetchHighMark;
        _prefetchLowMark = defaultPrefetchLowMark;

        if (_acknowledgeMode == NO_ACKNOWLEDGE)
        {
            _queue =
                    new FlowControllingBlockingQueue(_prefetchHighMark, _prefetchLowMark,
                                                     new FlowControllingBlockingQueue.ThresholdListener()
                                                     {
                                                         private final AtomicBoolean _suspendState = new AtomicBoolean();

                                                         public void aboveThreshold(int currentValue)
                                                         {
                                                             _logger.debug(
                                                                     "Above threshold(" + _prefetchHighMark
                                                                     + ") so suspending channel. Current value is " + currentValue);
                                                             _suspendState.set(true);
                                                             new Thread(new SuspenderRunner(_suspendState)).start();

                                                         }

                                                         public void underThreshold(int currentValue)
                                                         {
                                                             _logger.debug(
                                                                     "Below threshold(" + _prefetchLowMark
                                                                     + ") so unsuspending channel. Current value is " + currentValue);
                                                             _suspendState.set(false);
                                                             new Thread(new SuspenderRunner(_suspendState)).start();

                                                         }
                                                     });
        }
        else
        {
            _queue = new FlowControllingBlockingQueue(_prefetchHighMark, null);
        }
    }

    /**
     * Creates a new session on a connection with the default message factory factory.
     *
     * @param con                 The connection on which to create the session.
     * @param channelId           The unique identifier for the session.
     * @param transacted          Indicates whether or not the session is transactional.
     * @param acknowledgeMode     The acknoledgement mode for the session.
     * @param defaultPrefetchHigh The maximum number of messages to prefetched before suspending the session.
     * @param defaultPrefetchLow  The number of prefetched messages at which to resume the session.
     */
    AMQSession(AMQConnection con, int channelId, boolean transacted, int acknowledgeMode, int defaultPrefetchHigh,
               int defaultPrefetchLow)
    {
        this(con, channelId, transacted, acknowledgeMode, MessageFactoryRegistry.newDefaultRegistry(), defaultPrefetchHigh,
             defaultPrefetchLow);
    }

    // ===== JMS Session methods.

    /**
     * Closes the session with no timeout.
     *
     * @throws JMSException If the JMS provider fails to close the session due to some internal error.
     */
    public void close() throws JMSException
    {
        close(-1);
    }

    public void checkNotClosed() throws JMSException
    {
        try
        {
            super.checkNotClosed();
        }
        catch (IllegalStateException ise)
        {
            // if the Connection has closed then we should throw any exception that has occured that we were not waiting for
            AMQStateManager manager = _connection.getProtocolHandler().getStateManager();

            if (manager.getCurrentState().equals(AMQState.CONNECTION_CLOSED) && manager.getLastException() != null)
            {
                ise.setLinkedException(manager.getLastException());
            }

            throw ise;
        }
    }

    public BytesMessage createBytesMessage() throws JMSException
    {
        checkNotClosed();
        return new JMSBytesMessage(getMessageDelegateFactory());
    }

    /**
     * Acknowledges all unacknowledged messages on the session, for all message consumers on the session.
     *
     * @throws IllegalStateException If the session is closed.
     */
    public void acknowledge() throws IllegalStateException
    {
        if (isClosed())
        {
            throw new IllegalStateException("Session is already closed");
        }
        else if (hasFailedOver())
        {
            throw new IllegalStateException("has failed over");
        }

        while (true)
        {
            Long tag = _unacknowledgedMessageTags.poll();
            if (tag == null)
            {
                break;
            }
            acknowledgeMessage(tag, false);
        }
    }

    /**
     * Acknowledge one or many messages.
     *
     * @param deliveryTag The tag of the last message to be acknowledged.
     * @param multiple    <tt>true</tt> to acknowledge all messages up to and including the one specified by the
     *                    delivery tag, <tt>false</tt> to just acknowledge that message.
     *
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    public abstract void acknowledgeMessage(long deliveryTag, boolean multiple);

    public MethodRegistry getMethodRegistry()
    {
        MethodRegistry methodRegistry = getProtocolHandler().getMethodRegistry();
        return methodRegistry;
    }

    /**
     * Binds the named queue, with the specified routing key, to the named exchange.
     *
     * <p/>Note that this operation automatically retries in the event of fail-over.
     *
     * @param queueName    The name of the queue to bind.
     * @param routingKey   The routing key to bind the queue with.
     * @param arguments    Additional arguments.
     * @param exchangeName The exchange to bind the queue on.
     *
     * @throws AMQException If the queue cannot be bound for any reason.
     * @todo Be aware of possible changes to parameter order as versions change.
     * @todo Document the additional arguments that may be passed in the field table. Are these for headers exchanges?
     */
    public void bindQueue(final AMQShortString queueName, final AMQShortString routingKey, final FieldTable arguments,
                          final AMQShortString exchangeName, final AMQDestination destination) throws AMQException
    {
        bindQueue(queueName, routingKey, arguments, exchangeName, destination, false);
    }

    public void bindQueue(final AMQShortString queueName, final AMQShortString routingKey, final FieldTable arguments,
                          final AMQShortString exchangeName, final AMQDestination destination,
                          final boolean nowait) throws AMQException
    {
        /*new FailoverRetrySupport<Object, AMQException>(new FailoverProtectedOperation<Object, AMQException>()*/
        new FailoverNoopSupport<Object, AMQException>(new FailoverProtectedOperation<Object, AMQException>()
        {
            public Object execute() throws AMQException, FailoverException
            {
                sendQueueBind(queueName, routingKey, arguments, exchangeName, destination, nowait);
                return null;
            }
        }, _connection).execute();
    }

    public void addBindingKey(C consumer, AMQDestination amqd, String routingKey) throws AMQException
    {
        if (consumer.getQueuename() != null)
        {
            bindQueue(consumer.getQueuename(), new AMQShortString(routingKey), new FieldTable(), amqd.getExchangeName(), amqd);
        }
    }

    public abstract void sendQueueBind(final AMQShortString queueName, final AMQShortString routingKey, final FieldTable arguments,
                                       final AMQShortString exchangeName, AMQDestination destination,
                                       final boolean nowait) throws AMQException, FailoverException;

    /**
     * Closes the session.
     *
     * <p/>Note that this operation succeeds automatically if a fail-over interupts the sycnronous request to close
     * the channel. This is because the channel is marked as closed before the request to close it is made, so the
     * fail-over should not re-open it.
     *
     * @param timeout The timeout in milliseconds to wait for the session close acknoledgement from the broker.
     *
     * @throws JMSException If the JMS provider fails to close the session due to some internal error.
     * @todo Be aware of possible changes to parameter order as versions change.
     * @todo Not certain about the logic of ignoring the failover exception, because the channel won't be
     * re-opened. May need to examine this more carefully.
     * @todo Note that taking the failover mutex doesn't prevent this operation being interrupted by a failover,
     * because the failover process sends the failover event before acquiring the mutex itself.
     */
    public void close(long timeout) throws JMSException
    {
        close(timeout, true);
    }

    private void close(long timeout, boolean sendClose) throws JMSException
    {
        if (_logger.isInfoEnabled())
        {
            StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
            _logger.info("Closing session: " + this); // + ":"
            // + Arrays.asList(stackTrace).subList(3, stackTrace.length - 1));
        }

        // Ensure we only try and close an open session.
        if (!_closed.getAndSet(true))
        {
            _closing.set(true);
            synchronized (getFailoverMutex())
            {
                // We must close down all producers and consumers in an orderly fashion. This is the only method
                // that can be called from a different thread of control from the one controlling the session.
                synchronized (_messageDeliveryLock)
                {
                    // we pass null since this is not an error case
                    closeProducersAndConsumers(null);

                    try
                    {
                        // If the connection is open or we are in the process
                        // of closing the connection then send a cance
                        // no point otherwise as the connection will be gone
                        if (!_connection.isClosed() || _connection.isClosing())
                        {
                            if (sendClose)
                            {
                                sendClose(timeout);
                            }
                        }
                    }
                    catch (AMQException e)
                    {
                        JMSException jmse = new JMSException("Error closing session: " + e);
                        jmse.setLinkedException(e);
                        throw jmse;
                    }
                    // This is ignored because the channel is already marked as closed so the fail-over process will
                    // not re-open it.
                    catch (FailoverException e)
                    {
                        _logger.debug(
                                "Got FailoverException during channel close, ignored as channel already marked as closed.");
                    }
                    finally
                    {
                        _connection.deregisterSession(_channelId);
                    }
                }
            }
        }
    }

    public abstract void sendClose(long timeout) throws AMQException, FailoverException;

    /**
     * Called when the server initiates the closure of the session unilaterally.
     *
     * @param e the exception that caused this session to be closed. Null causes the
     */
    public void closed(Throwable e) throws JMSException
    {
        // This method needs to be improved. Throwables only arrive here from the mina : exceptionRecived
        // calls through connection.closeAllSessions which is also called by the public connection.close()
        // with a null cause
        // When we are closing the Session due to a protocol session error we simply create a new AMQException
        // with the correct error code and text this is cleary WRONG as the instanceof check below will fail.
        // We need to determin here if the connection should be

        if (e instanceof AMQDisconnectedException)
        {
            if (_dispatcher != null)
            {
                // Failover failed and ain't coming back. Knife the dispatcher.
                _dispatcherThread.interrupt();
            }

       }

        //if we don't have an exception then we can perform closing operations  
        _closing.set(e == null);

        if (!_closed.getAndSet(true))
        {
            synchronized (_messageDeliveryLock)
            {
                // An AMQException has an error code and message already and will be passed in when closure occurs as a
                // result of a channel close request
                AMQException amqe;
                if (e instanceof AMQException)
                {
                    amqe = (AMQException) e;
                }
                else
                {
                    amqe = new AMQException("Closing session forcibly", e);
                }

                _connection.deregisterSession(_channelId);
                closeProducersAndConsumers(amqe);
            }
        }
    }

    /**
     * Commits all messages done in this transaction and releases any locks currently held.
     *
     * <p/>If the commit fails, because the commit itself is interrupted by a fail-over between requesting that the
     * commit be done, and receiving an acknowledgement that it has been done, then a JMSException will be thrown.
     * The client will be unable to determine whether or not the commit actually happened on the broker in this case.
     *
     * @throws JMSException If the JMS provider fails to commit the transaction due to some internal error. This does
     *                      not mean that the commit is known to have failed, merely that it is not known whether it
     *                      failed or not.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    public void commit() throws JMSException
    {
        checkTransacted();

        try
        {
            //Check that we are clean to commit.
            if (_failedOverDirty)
            {
                rollback();

                throw new TransactionRolledBackException("Connection failover has occured since last send. " +
                                                         "Forced rollback");
            }


            // Acknowledge all delivered messages
            while (true)
            {
                Long tag = _deliveredMessageTags.poll();
                if (tag == null)
                {
                    break;
                }

                acknowledgeMessage(tag, false);
            }
            // Commits outstanding messages and acknowledgments
            sendCommit();
            markClean();
        }
        catch (AMQException e)
        {
            throw new JMSAMQException("Failed to commit: " + e.getMessage() + ":" + e.getCause(), e);
        }
        catch (FailoverException e)
        {
            throw new JMSAMQException("Fail-over interrupted commit. Status of the commit is uncertain.", e);
        }
    }

    public abstract void sendCommit() throws AMQException, FailoverException;


    public void confirmConsumerCancelled(int consumerTag)
    {

        // Remove the consumer from the map
        C consumer = _consumers.get(consumerTag);
        if (consumer != null)
        {
            if (!consumer.isNoConsume())  // Normal Consumer
            {
                // Clean the Maps up first
                // Flush any pending messages for this consumerTag
                if (_dispatcher != null)
                {
                    _logger.info("Dispatcher is not null");
                }
                else
                {
                    _logger.info("Dispatcher is null so created stopped dispatcher");
                    startDispatcherIfNecessary(true);
                }

                _dispatcher.rejectPending(consumer);
            }
            else // Queue Browser
            {
                // Just close the consumer
                // fixme  the CancelOK is being processed before the arriving messages..
                // The dispatcher is still to process them so the server sent in order but the client
                // has yet to receive before the close comes in.

                // consumer.markClosed();

                if (consumer.isAutoClose())
                {
                    // There is a small window where the message is between the two queues in the dispatcher.
                    if (consumer.isClosed())
                    {
                        if (_logger.isInfoEnabled())
                        {
                            _logger.info("Closing consumer:" + consumer.debugIdentity());
                        }

                        deregisterConsumer(consumer);
                    }
                    else
                    {
                        _queue.add(new CloseConsumerMessage(consumer));
                    }
                }
            }
        }
    }

    public QueueBrowser createBrowser(Queue queue) throws JMSException
    {
        if (isStrictAMQP())
        {
            throw new UnsupportedOperationException();
        }

        return createBrowser(queue, null);
    }

    public QueueBrowser createBrowser(Queue queue, String messageSelector) throws JMSException
    {
        if (isStrictAMQP())
        {
            throw new UnsupportedOperationException();
        }

        checkNotClosed();
        checkValidQueue(queue);

        return new AMQQueueBrowser(this, (AMQQueue) queue, messageSelector);
    }

    public MessageConsumer createBrowserConsumer(Destination destination, String messageSelector, boolean noLocal)
            throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, _prefetchHighMark, _prefetchLowMark, noLocal, false,
                                  messageSelector, null, true, true);
    }

    public MessageConsumer createConsumer(Destination destination) throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, _prefetchHighMark, _prefetchLowMark, false, (destination instanceof Topic), null, null,
                                  false, false);
    }

    public C createExclusiveConsumer(Destination destination) throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, _prefetchHighMark, _prefetchLowMark, false, true, null, null,
                                  false, false);
    }

    public MessageConsumer createConsumer(Destination destination, String messageSelector) throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, _prefetchHighMark, _prefetchLowMark, false, (destination instanceof Topic),
                                  messageSelector, null, false, false);
    }

    public MessageConsumer createConsumer(Destination destination, String messageSelector, boolean noLocal)
            throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, _prefetchHighMark, _prefetchLowMark, noLocal, (destination instanceof Topic),
                                  messageSelector, null, false, false);
    }

    public MessageConsumer createExclusiveConsumer(Destination destination, String messageSelector, boolean noLocal)
            throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, _prefetchHighMark, _prefetchLowMark, noLocal, true,
                                  messageSelector, null, false, false);
    }

    public MessageConsumer createConsumer(Destination destination, int prefetch, boolean noLocal, boolean exclusive,
                                          String selector) throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, prefetch, prefetch / 2, noLocal, exclusive, selector, null, false, false);
    }

    public MessageConsumer createConsumer(Destination destination, int prefetchHigh, int prefetchLow, boolean noLocal,
                                          boolean exclusive, String selector) throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, prefetchHigh, prefetchLow, noLocal, exclusive, selector, null, false, false);
    }

    public MessageConsumer createConsumer(Destination destination, int prefetch, boolean noLocal, boolean exclusive,
                                          String selector, FieldTable rawSelector) throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, prefetch, prefetch / 2, noLocal, exclusive, selector, rawSelector, false, false);
    }

    public MessageConsumer createConsumer(Destination destination, int prefetchHigh, int prefetchLow, boolean noLocal,
                                          boolean exclusive, String selector, FieldTable rawSelector) throws JMSException
    {
        checkValidDestination(destination);

        return createConsumerImpl(destination, prefetchHigh, prefetchLow, noLocal, exclusive, selector, rawSelector, false,
                                  false);
    }

    public abstract TopicSubscriber createDurableSubscriber(Topic topic, String name) throws JMSException;

    public TopicSubscriber createDurableSubscriber(Topic topic, String name, String messageSelector, boolean noLocal)
            throws JMSException
    {
        checkNotClosed();
        checkValidTopic(topic, true);
        if (_subscriptions.containsKey(name))
        {
            _subscriptions.get(name).close();
        }
        AMQTopic dest = AMQTopic.createDurableTopic((AMQTopic) topic, name, _connection);
        C consumer = (C) createConsumer(dest, messageSelector, noLocal);
        TopicSubscriberAdaptor<C> subscriber = new TopicSubscriberAdaptor(dest, consumer);
        _subscriptions.put(name, subscriber);
        _reverseSubscriptionMap.put(subscriber.getMessageConsumer(), name);

        return subscriber;
    }

    public MapMessage createMapMessage() throws JMSException
    {
        checkNotClosed();
        return new JMSMapMessage(getMessageDelegateFactory());
    }

    public javax.jms.Message createMessage() throws JMSException
    {
        return createBytesMessage();
    }

    public ObjectMessage createObjectMessage() throws JMSException
    {
        checkNotClosed();
        return (ObjectMessage) new JMSObjectMessage(getMessageDelegateFactory());
    }

    public ObjectMessage createObjectMessage(Serializable object) throws JMSException
    {
        ObjectMessage msg = createObjectMessage();
        msg.setObject(object);

        return msg;
    }

    public P createProducer(Destination destination) throws JMSException
    {
        return createProducerImpl(destination, DEFAULT_MANDATORY, DEFAULT_IMMEDIATE);
    }

    public P createProducer(Destination destination, boolean immediate) throws JMSException
    {
        return createProducerImpl(destination, DEFAULT_MANDATORY, immediate);
    }

    public P createProducer(Destination destination, boolean mandatory, boolean immediate)
            throws JMSException
    {
        return createProducerImpl(destination, mandatory, immediate);
    }

    public P createProducer(Destination destination, boolean mandatory, boolean immediate,
                                               boolean waitUntilSent) throws JMSException
    {
        return createProducerImpl(destination, mandatory, immediate, waitUntilSent);
    }

    public TopicPublisher createPublisher(Topic topic) throws JMSException
    {
        checkNotClosed();

        return new TopicPublisherAdapter((P) createProducer(topic, false, false), topic);
    }

    public Queue createQueue(String queueName) throws JMSException
    {
        checkNotClosed();
        if (queueName.indexOf('/') == -1)
        {
            return new AMQQueue(getDefaultQueueExchangeName(), new AMQShortString(queueName));
        }
        else
        {
            try
            {
                return new AMQQueue(new AMQBindingURL(queueName));
            }
            catch (URISyntaxException urlse)
            {
                _logger.error("", urlse);
                JMSException jmse = new JMSException(urlse.getReason());
                jmse.setLinkedException(urlse);

                throw jmse;
            }
        }
    }

    /**
     * Declares the named queue.
     *
     * <p/>Note that this operation automatically retries in the event of fail-over.
     *
     * @param name       The name of the queue to declare.
     * @param autoDelete
     * @param durable    Flag to indicate that the queue is durable.
     * @param exclusive  Flag to indicate that the queue is exclusive to this client.
     *
     * @throws AMQException If the queue cannot be declared for any reason.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    public void createQueue(final AMQShortString name, final boolean autoDelete, final boolean durable,
                            final boolean exclusive) throws AMQException
    {
        createQueue(name, autoDelete, durable, exclusive, null);
    }

    /**
     * Declares the named queue.
     *
     * <p/>Note that this operation automatically retries in the event of fail-over.
     *
     * @param name       The name of the queue to declare.
     * @param autoDelete
     * @param durable    Flag to indicate that the queue is durable.
     * @param exclusive  Flag to indicate that the queue is exclusive to this client.
     * @param arguments  Arguments used to set special properties of the queue
     *
     * @throws AMQException If the queue cannot be declared for any reason.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    public void createQueue(final AMQShortString name, final boolean autoDelete, final boolean durable,
                            final boolean exclusive, final Map<String, Object> arguments) throws AMQException
    {
        new FailoverRetrySupport<Object, AMQException>(new FailoverProtectedOperation<Object, AMQException>()
        {
            public Object execute() throws AMQException, FailoverException
            {
                sendCreateQueue(name, autoDelete, durable, exclusive, arguments);
                return null;
            }
        }, _connection).execute();
    }

    public abstract void sendCreateQueue(AMQShortString name, final boolean autoDelete, final boolean durable,
                                         final boolean exclusive, final Map<String, Object> arguments) throws AMQException, FailoverException;

    /**
     * Creates a QueueReceiver
     *
     * @param destination
     *
     * @return QueueReceiver - a wrapper around our MessageConsumer
     *
     * @throws JMSException
     */
    public QueueReceiver createQueueReceiver(Destination destination) throws JMSException
    {
        checkValidDestination(destination);
        AMQQueue dest = (AMQQueue) destination;
        C consumer = (C) createConsumer(destination);

        return new QueueReceiverAdaptor(dest, consumer);
    }

    /**
     * Creates a QueueReceiver using a message selector
     *
     * @param destination
     * @param messageSelector
     *
     * @return QueueReceiver - a wrapper around our MessageConsumer
     *
     * @throws JMSException
     */
    public QueueReceiver createQueueReceiver(Destination destination, String messageSelector) throws JMSException
    {
        checkValidDestination(destination);
        AMQQueue dest = (AMQQueue) destination;
        C consumer = (C) createConsumer(destination, messageSelector);

        return new QueueReceiverAdaptor(dest, consumer);
    }

    /**
     * Creates a QueueReceiver wrapping a MessageConsumer
     *
     * @param queue
     *
     * @return QueueReceiver
     *
     * @throws JMSException
     */
    public QueueReceiver createReceiver(Queue queue) throws JMSException
    {
        checkNotClosed();
        AMQQueue dest = (AMQQueue) queue;
        C consumer = (C) createConsumer(dest);

        return new QueueReceiverAdaptor(dest, consumer);
    }

    /**
     * Creates a QueueReceiver wrapping a MessageConsumer using a message selector
     *
     * @param queue
     * @param messageSelector
     *
     * @return QueueReceiver
     *
     * @throws JMSException
     */
    public QueueReceiver createReceiver(Queue queue, String messageSelector) throws JMSException
    {
        checkNotClosed();
        AMQQueue dest = (AMQQueue) queue;
        C consumer = (C) createConsumer(dest, messageSelector);

        return new QueueReceiverAdaptor(dest, consumer);
    }

    public QueueSender createSender(Queue queue) throws JMSException
    {
        checkNotClosed();

        // return (QueueSender) createProducer(queue);
        return new QueueSenderAdapter(createProducer(queue), queue);
    }

    public StreamMessage createStreamMessage() throws JMSException
    {
        // This method needs to be improved. Throwables only arrive here from the mina : exceptionRecived
        // calls through connection.closeAllSessions which is also called by the public connection.close()
        // with a null cause
        // When we are closing the Session due to a protocol session error we simply create a new AMQException
        // with the correct error code and text this is cleary WRONG as the instanceof check below will fail.
        // We need to determin here if the connection should be

        synchronized (getFailoverMutex())
        {
            checkNotClosed();

            return new JMSStreamMessage(getMessageDelegateFactory());
        }
    }

    /**
     * Creates a non-durable subscriber
     *
     * @param topic
     *
     * @return TopicSubscriber - a wrapper round our MessageConsumer
     *
     * @throws JMSException
     */
    public TopicSubscriber createSubscriber(Topic topic) throws JMSException
    {
        checkNotClosed();
        AMQTopic dest = checkValidTopic(topic);

        // AMQTopic dest = new AMQTopic(topic.getTopicName());
        return new TopicSubscriberAdaptor(dest, (C) createExclusiveConsumer(dest));
    }

    /**
     * Creates a non-durable subscriber with a message selector
     *
     * @param topic
     * @param messageSelector
     * @param noLocal
     *
     * @return TopicSubscriber - a wrapper round our MessageConsumer
     *
     * @throws JMSException
     */
    public TopicSubscriber createSubscriber(Topic topic, String messageSelector, boolean noLocal) throws JMSException
    {
        checkNotClosed();
        AMQTopic dest = checkValidTopic(topic);

        // AMQTopic dest = new AMQTopic(topic.getTopicName());
        return new TopicSubscriberAdaptor(dest, (C) createExclusiveConsumer(dest, messageSelector, noLocal));
    }

    public TemporaryQueue createTemporaryQueue() throws JMSException
    {
        checkNotClosed();
        try
        {
            AMQTemporaryQueue result = new AMQTemporaryQueue(this);

            // this is done so that we can produce to a temporary queue before we create a consumer
            result.setQueueName(result.getRoutingKey());
            createQueue(result.getAMQQueueName(), result.isAutoDelete(),
                        result.isDurable(), result.isExclusive());
            bindQueue(result.getAMQQueueName(), result.getRoutingKey(),
                    new FieldTable(), result.getExchangeName(), result);
            return result;
        }
        catch (Exception e)
        {
           JMSException ex = new JMSException("Cannot create temporary queue");
           ex.setLinkedException(e);
           throw ex;
        }
    }

    public TemporaryTopic createTemporaryTopic() throws JMSException
    {
        checkNotClosed();

        return new AMQTemporaryTopic(this);
    }

    public TextMessage createTextMessage() throws JMSException
    {
        synchronized (getFailoverMutex())
        {
            checkNotClosed();

            return new JMSTextMessage(getMessageDelegateFactory());
        }
    }

    protected Object getFailoverMutex()
    {
        return _connection.getFailoverMutex();
    }

    public TextMessage createTextMessage(String text) throws JMSException
    {

        TextMessage msg = createTextMessage();
        msg.setText(text);

        return msg;
    }

    public Topic createTopic(String topicName) throws JMSException
    {
        checkNotClosed();

        if (topicName.indexOf('/') == -1)
        {
            return new AMQTopic(getDefaultTopicExchangeName(), new AMQShortString(topicName));
        }
        else
        {
            try
            {
                return new AMQTopic(new AMQBindingURL(topicName));
            }
            catch (URISyntaxException urlse)
            {
                JMSException jmse = new JMSException(urlse.getReason());
                jmse.setLinkedException(urlse);

                throw jmse;
            }
        }
    }

    public void declareExchange(AMQShortString name, AMQShortString type, boolean nowait) throws AMQException
    {
        declareExchange(name, type, getProtocolHandler(), nowait);
    }

    public int getAcknowledgeMode() throws JMSException
    {
        checkNotClosed();

        return _acknowledgeMode;
    }

    public AMQConnection getAMQConnection()
    {
        return _connection;
    }

    public int getChannelId()
    {
        return _channelId;
    }

    public int getDefaultPrefetch()
    {
        return _prefetchHighMark;
    }

    public int getDefaultPrefetchHigh()
    {
        return _prefetchHighMark;
    }

    public int getDefaultPrefetchLow()
    {
        return _prefetchLowMark;
    }

    public AMQShortString getDefaultQueueExchangeName()
    {
        return _connection.getDefaultQueueExchangeName();
    }

    public AMQShortString getDefaultTopicExchangeName()
    {
        return _connection.getDefaultTopicExchangeName();
    }

    public MessageListener getMessageListener() throws JMSException
    {
        // checkNotClosed();
        return _messageListener;
    }

    public AMQShortString getTemporaryQueueExchangeName()
    {
        return _connection.getTemporaryQueueExchangeName();
    }

    public AMQShortString getTemporaryTopicExchangeName()
    {
        return _connection.getTemporaryTopicExchangeName();
    }

    public int getTicket()
    {
        return _ticket;
    }

    public boolean getTransacted() throws JMSException
    {
        checkNotClosed();

        return _transacted;
    }

    public boolean hasConsumer(Destination destination)
    {
        AtomicInteger counter = _destinationConsumerCount.get(destination);

        return (counter != null) && (counter.get() != 0);
    }

    public boolean isStrictAMQP()
    {
        return _strictAMQP;
    }

    public boolean isSuspended()
    {
        return _suspended;
    }

    protected void addUnacknowledgedMessage(long id)
    {
        _unacknowledgedMessageTags.add(id);
    }

    protected void addDeliveredMessage(long id)
    {
        _deliveredMessageTags.add(id);
    }

    /**
     * Invoked by the MINA IO thread (indirectly) when a message is received from the transport. Puts the message onto
     * the queue read by the dispatcher.
     *
     * @param message the message that has been received
     */
    public void messageReceived(UnprocessedMessage message)
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Message[" + message.toString() + "] received in session");
        }
        _highestDeliveryTag.set(message.getDeliveryTag());
        _queue.add(message);
    }

    public void declareAndBind(AMQDestination amqd)
            throws
            AMQException
    {
        AMQProtocolHandler protocolHandler = getProtocolHandler();
        declareExchange(amqd, protocolHandler, false);
        AMQShortString queueName = declareQueue(amqd, protocolHandler, false);
        bindQueue(queueName, amqd.getRoutingKey(), new FieldTable(), amqd.getExchangeName(), amqd);
    }

    /**
     * Stops message delivery in this session, and restarts message delivery with the oldest unacknowledged message.
     *
     * <p/>All consumers deliver messages in a serial order. Acknowledging a received message automatically acknowledges
     * all messages that have been delivered to the client.
     *
     * <p/>Restarting a session causes it to take the following actions:
     *
     * <ul>
     * <li>Stop message delivery.</li>
     * <li>Mark all messages that might have been delivered but not acknowledged as "redelivered".
     * <li>Restart the delivery sequence including all unacknowledged messages that had been previously delivered.
     * Redelivered messages do not have to be delivered in exactly their original delivery order.</li>
     * </ul>
     *
     * <p/>If the recover operation is interrupted by a fail-over, between asking that the broker begin recovery and
     * receiving acknolwedgement that it hasm then a JMSException will be thrown. In this case it will not be possible
     * for the client to determine whether the broker is going to recover the session or not.
     *
     * @throws JMSException If the JMS provider fails to stop and restart message delivery due to some internal error.
     *                      Not that this does not necessarily mean that the recovery has failed, but simply that it is
     *                      not possible to tell if it has or not.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    public void recover() throws JMSException
    {
        // Ensure that the session is open.
        checkNotClosed();

        // Ensure that the session is not transacted.
        checkNotTransacted();

        // this is set only here, and the before the consumer's onMessage is called it is set to false
        _inRecovery = true;
        try
        {

            boolean isSuspended = isSuspended();

            if (!isSuspended)
            {
                suspendChannel(true);
            }

            if (_dispatcher != null)
            {
                _dispatcher.rollback();
            }

            sendRecover();

            markClean();
            
            if (!isSuspended)
            {
                suspendChannel(false);
            }
        }
        catch (AMQException e)
        {
            throw new JMSAMQException("Recover failed: " + e.getMessage(), e);
        }
        catch (FailoverException e)
        {
            throw new JMSAMQException("Recovery was interrupted by fail-over. Recovery status is not known.", e);
        }
    }

    protected abstract void sendRecover() throws AMQException, FailoverException;

    public void rejectMessage(UnprocessedMessage message, boolean requeue)
    {

        if (_logger.isDebugEnabled())
        {
            _logger.debug("Rejecting Unacked message:" + message.getDeliveryTag());
        }

        rejectMessage(message.getDeliveryTag(), requeue);
    }

    public void rejectMessage(AbstractJMSMessage message, boolean requeue)
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Rejecting Abstract message:" + message.getDeliveryTag());
        }

        rejectMessage(message.getDeliveryTag(), requeue);

    }

    public abstract void rejectMessage(long deliveryTag, boolean requeue);

    /**
     * Commits all messages done in this transaction and releases any locks currently held.
     *
     * <p/>If the rollback fails, because the rollback itself is interrupted by a fail-over between requesting that the
     * rollback be done, and receiving an acknowledgement that it has been done, then a JMSException will be thrown.
     * The client will be unable to determine whether or not the rollback actually happened on the broker in this case.
     *
     * @throws JMSException If the JMS provider fails to rollback the transaction due to some internal error. This does
     *                      not mean that the rollback is known to have failed, merely that it is not known whether it
     *                      failed or not.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    public void rollback() throws JMSException
    {
        synchronized (_suspensionLock)
        {
            checkTransacted();

            try
            {
                boolean isSuspended = isSuspended();

                if (!isSuspended)
                {
                    suspendChannel(true);
                }

                // Let the dispatcher know that all the incomming messages
                // should be rolled back(reject/release)
                _rollbackMark.set(_highestDeliveryTag.get());

                syncDispatchQueue();      

                _dispatcher.rollback();

                releaseForRollback();

                sendRollback();

                markClean();

                if (!isSuspended)
                {
                    suspendChannel(false);
                }
            }
            catch (AMQException e)
            {
                throw new JMSAMQException("Failed to rollback: " + e, e);
            }
            catch (FailoverException e)
            {
                throw new JMSAMQException("Fail-over interrupted rollback. Status of the rollback is uncertain.", e);
            }
        }
    }

    public abstract void releaseForRollback();

    public abstract void sendRollback() throws AMQException, FailoverException;

    public void run()
    {
        throw new java.lang.UnsupportedOperationException();
    }

    public void setMessageListener(MessageListener listener) throws JMSException
    {
        // checkNotClosed();
        //
        // if (_dispatcher != null && !_dispatcher.connectionStopped())
        // {
        // throw new javax.njms.IllegalStateException("Attempt to set listener while session is started.");
        // }
        //
        // // We are stopped
        // for (Iterator<BasicMessageConsumer> i = _consumers.values().iterator(); i.hasNext();)
        // {
        // BasicMessageConsumer consumer = i.next();
        //
        // if (consumer.isReceiving())
        // {
        // throw new javax.njms.IllegalStateException("Another thread is already receiving synchronously.");
        // }
        // }
        //
        // _messageListener = listener;
        //
        // for (Iterator<BasicMessageConsumer> i = _consumers.values().iterator(); i.hasNext();)
        // {
        // i.next().setMessageListener(_messageListener);
        // }

    }

    /*public void setTicket(int ticket)
    {
        _ticket = ticket;
    }*/

    public void unsubscribe(String name) throws JMSException
    {
        checkNotClosed();
        TopicSubscriberAdaptor subscriber = _subscriptions.get(name);
        if (subscriber != null)
        {
            // send a queue.delete for the subscription
            deleteQueue(AMQTopic.getDurableTopicQueueName(name, _connection));
            _subscriptions.remove(name);
            _reverseSubscriptionMap.remove(subscriber);
        }
        else
        {
            if (_strictAMQP)
            {
                if (_strictAMQPFATAL)
                {
                    throw new UnsupportedOperationException("JMS Durable not currently supported by AMQP.");
                }
                else
                {
                    _logger.warn("Unable to determine if subscription already exists for '" + name + "' for unsubscribe."
                                 + " Requesting queue deletion regardless.");
                }

                deleteQueue(AMQTopic.getDurableTopicQueueName(name, _connection));
            }
            else // Queue Browser
            {

                if (isQueueBound(getDefaultTopicExchangeName(), AMQTopic.getDurableTopicQueueName(name, _connection)))
                {
                    deleteQueue(AMQTopic.getDurableTopicQueueName(name, _connection));
                }
                else
                {
                    throw new InvalidDestinationException("Unknown subscription exchange:" + name);
                }
            }
        }
    }

    protected C createConsumerImpl(final Destination destination, final int prefetchHigh,
                                                 final int prefetchLow, final boolean noLocal, final boolean exclusive, String selector, final FieldTable rawSelector,
                                                 final boolean noConsume, final boolean autoClose) throws JMSException
    {
        checkTemporaryDestination(destination);

        final String messageSelector;

        if (_strictAMQP && !((selector == null) || selector.equals("")))
        {
            if (_strictAMQPFATAL)
            {
                throw new UnsupportedOperationException("Selectors not currently supported by AMQP.");
            }
            else
            {
                messageSelector = null;
            }
        }
        else
        {
            messageSelector = selector;
        }

        return new FailoverRetrySupport<C, JMSException>(
                new FailoverProtectedOperation<C, JMSException>()
                {
                    public C execute() throws JMSException, FailoverException
                    {
                        checkNotClosed();

                        AMQDestination amqd = (AMQDestination) destination;

                        final AMQProtocolHandler protocolHandler = getProtocolHandler();
                        // TODO: Define selectors in AMQP
                        // TODO: construct the rawSelector from the selector string if rawSelector == null
                        final FieldTable ft = FieldTableFactory.newFieldTable();
                        // if (rawSelector != null)
                        // ft.put("headers", rawSelector.getDataAsBytes());
                        // rawSelector is used by HeadersExchange and is not a JMS Selector
                        if (rawSelector != null)
                        {
                            ft.addAll(rawSelector);
                        }

                        if (messageSelector != null)
                        {
                            ft.put(new AMQShortString("x-filter-jms-selector"), messageSelector);
                        }

                        C consumer = createMessageConsumer(amqd, prefetchHigh, prefetchLow,
                                                                              noLocal, exclusive, messageSelector, ft, noConsume, autoClose);

                        if (_messageListener != null)
                        {
                            consumer.setMessageListener(_messageListener);
                        }

                        try
                        {
                            registerConsumer(consumer, false);
                        }
                        catch (AMQInvalidArgumentException ise)
                        {
                            JMSException ex = new InvalidSelectorException(ise.getMessage());
                            ex.setLinkedException(ise);
                            throw ex;
                        }
                        catch (AMQInvalidRoutingKeyException e)
                        {
                            JMSException ide =
                                    new InvalidDestinationException("Invalid routing key:" + amqd.getRoutingKey().toString());
                            ide.setLinkedException(e);
                            throw ide;
                        }
                        catch (AMQException e)
                        {
                            if (e instanceof AMQChannelClosedException)
                            {
                                close(-1, false);
                            }

                            JMSException ex = new JMSException("Error registering consumer: " + e);

                            ex.setLinkedException(e);
                            throw ex;
                        }

                        synchronized (destination)
                        {
                            _destinationConsumerCount.putIfAbsent(destination, new AtomicInteger());
                            _destinationConsumerCount.get(destination).incrementAndGet();
                        }

                        return consumer;
                    }
                }, _connection).execute();
    }

    public abstract C createMessageConsumer(final AMQDestination destination, final int prefetchHigh,
                                                               final int prefetchLow, final boolean noLocal, final boolean exclusive, String selector, final FieldTable arguments,
                                                               final boolean noConsume, final boolean autoClose) throws JMSException;

    /**
     * Called by the MessageConsumer when closing, to deregister the consumer from the map from consumerTag to consumer
     * instance.
     *
     * @param consumer the consum
     */
    void deregisterConsumer(C consumer)
    {
        if (_consumers.remove(consumer.getConsumerTag()) != null)
        {
            String subscriptionName = _reverseSubscriptionMap.remove(consumer);
            if (subscriptionName != null)
            {
                _subscriptions.remove(subscriptionName);
            }

            Destination dest = consumer.getDestination();
            synchronized (dest)
            {
                if (_destinationConsumerCount.get(dest).decrementAndGet() == 0)
                {
                    _destinationConsumerCount.remove(dest);
                }
            }

            // Consumers that are closed in a transaction must be stored
            // so that messages they have received can be acknowledged on commit
            if (_transacted)
            {
                _removedConsumers.add(consumer);
            }
        }
    }

    void deregisterProducer(long producerId)
    {
        _producers.remove(new Long(producerId));
    }

    boolean isInRecovery()
    {
        return _inRecovery;
    }

    boolean isQueueBound(AMQShortString exchangeName, AMQShortString queueName) throws JMSException
    {
        return isQueueBound(exchangeName, queueName, null);
    }

    /**
     * Tests whether or not the specified queue is bound to the specified exchange under a particular routing key.
     *
     * <p/>Note that this operation automatically retries in the event of fail-over.
     *
     * @param exchangeName The exchange name to test for binding against.
     * @param queueName    The queue name to check if bound.
     * @param routingKey   The routing key to check if the queue is bound under.
     *
     * @return <tt>true</tt> if the queue is bound to the exchange and routing key, <tt>false</tt> if not.
     *
     * @throws JMSException If the query fails for any reason.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    public abstract boolean isQueueBound(final AMQShortString exchangeName, final AMQShortString queueName, final AMQShortString routingKey)
            throws JMSException;

    public abstract boolean isQueueBound(final AMQDestination destination) throws JMSException;

    /**
     * Called to mark the session as being closed. Useful when the session needs to be made invalid, e.g. after failover
     * when the client has veoted resubscription. <p/> The caller of this method must already hold the failover mutex.
     */
    void markClosed()
    {
        _closed.set(true);
        _connection.deregisterSession(_channelId);
        markClosedProducersAndConsumers();

    }

    void failoverPrep()
    {
        syncDispatchQueue();
    }

    void syncDispatchQueue()
    {
        if (Thread.currentThread() == _dispatcherThread)
        {
            while (!_closed.get() && !_queue.isEmpty())
            {
                Dispatchable disp;
                try
                {
                    disp = (Dispatchable) _queue.take();
                }
                catch (InterruptedException e)
                {
                    throw new RuntimeException(e);
                }

                // Check just in case _queue becomes empty, it shouldn't but
                // better than an NPE.
                if (disp == null)
                {
                    _logger.debug("_queue became empty during sync.");
                    break;
                }

                disp.dispatch(AMQSession.this);
            }
        }
        else
        {
            startDispatcherIfNecessary();

            final CountDownLatch signal = new CountDownLatch(1);

            _queue.add(new Dispatchable()
            {
                public void dispatch(AMQSession ssn)
                {
                    signal.countDown();
                }
            });

            try
            {
                signal.await();
            }
            catch (InterruptedException e)
            {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Resubscribes all producers and consumers. This is called when performing failover.
     *
     * @throws AMQException
     */
    void resubscribe() throws AMQException
    {
        if (_dirty)
        {
            _failedOverDirty = true;
        }

        _rollbackMark.set(-1);
        resubscribeProducers();
        resubscribeConsumers();
    }

    void setHasMessageListeners()
    {
        _hasMessageListeners = true;
    }

    void setInRecovery(boolean inRecovery)
    {
        _inRecovery = inRecovery;
    }

    boolean isStarted()
    {
        return _startedAtLeastOnce.get();
    }

    /**
     * Starts the session, which ensures that it is not suspended and that its event dispatcher is running.
     *
     * @throws AMQException If the session cannot be started for any reason.
     * @todo This should be controlled by _stopped as it pairs with the stop method fixme or check the
     * FlowControlledBlockingQueue _queue to see if we have flow controlled. will result in sending Flow messages
     * for each subsequent call to flow.. only need to do this if we have called stop.
     */
    void start() throws AMQException
    {
        // Check if the session has perviously been started and suspended, in which case it must be unsuspended.
        if (_startedAtLeastOnce.getAndSet(true))
        {
            suspendChannel(false);
        }

        // If the event dispatcher is not running then start it too.
        if (hasMessageListeners())
        {
            startDispatcherIfNecessary();
        }
    }

    void startDispatcherIfNecessary()
    {
        //If we are the dispatcher then we don't need to check we are started
        if (Thread.currentThread() == _dispatcherThread)
        {
            return;
        }

        // If IMMEDIATE_PREFETCH is not set then we need to start fetching
        // This is final per session so will be multi-thread safe.
        if (!_immediatePrefetch)
        {
            // We do this now if this is the first call on a started connection
            if (isSuspended() && _startedAtLeastOnce.get() && _firstDispatcher.getAndSet(false))
            {
                try
                {
                    suspendChannel(false);
                }
                catch (AMQException e)
                {
                    _logger.info("Unsuspending channel threw an exception:" + e);
                }
            }
        }

        startDispatcherIfNecessary(false);
    }

    synchronized void startDispatcherIfNecessary(boolean initiallyStopped)
    {
        if (_dispatcher == null)
        {
            _dispatcher = new Dispatcher();
            try
            {
                _dispatcherThread = Threading.getThreadFactory().createThread(_dispatcher);

            }
            catch(Exception e)
            {
                throw new Error("Error creating Dispatcher thread",e);
            }
            _dispatcherThread.setName("Dispatcher-Channel-" + _channelId);
            _dispatcherThread.setDaemon(true);
            _dispatcher.setConnectionStopped(initiallyStopped);
            _dispatcherThread.start();
            if (_dispatcherLogger.isInfoEnabled())
            {
                _dispatcherLogger.info(_dispatcherThread.getName() + " created");
            }
        }
        else
        {
            _dispatcher.setConnectionStopped(initiallyStopped);
        }
    }

    void stop() throws AMQException
    {
        // Stop the server delivering messages to this session.
        suspendChannel(true);

        if (_dispatcher != null)
        {
            _dispatcher.setConnectionStopped(true);
        }
    }

    /*
     * Binds the named queue, with the specified routing key, to the named exchange.
     *
     * <p/>Note that this operation automatically retries in the event of fail-over.
     *
     * @param queueName    The name of the queue to bind.
     * @param routingKey   The routing key to bind the queue with.
     * @param arguments    Additional arguments.
     * @param exchangeName The exchange to bind the queue on.
     *
     * @throws AMQException If the queue cannot be bound for any reason.
     */
    /*private void bindQueue(AMQDestination amqd, AMQShortString queueName, AMQProtocolHandler protocolHandler, FieldTable ft)
        throws AMQException, FailoverException
    {
        AMQFrame queueBind =
            QueueBindBody.createAMQFrame(_channelId, getProtocolMajorVersion(), getProtocolMinorVersion(), ft, // arguments
                amqd.getExchangeName(), // exchange
                false, // nowait
                queueName, // queue
                amqd.getRoutingKey(), // routingKey
                getTicket()); // ticket

        protocolHandler.syncWrite(queueBind, QueueBindOkBody.class);
    }*/

    private void checkNotTransacted() throws JMSException
    {
        if (getTransacted())
        {
            throw new IllegalStateException("Session is transacted");
        }
    }

    private void checkTemporaryDestination(Destination destination) throws JMSException
    {
        if ((destination instanceof TemporaryDestination))
        {
            _logger.debug("destination is temporary");
            final TemporaryDestination tempDest = (TemporaryDestination) destination;
            if (tempDest.getSession() != this)
            {
                _logger.debug("destination is on different session");
                throw new JMSException("Cannot consume from a temporary destination created onanother session");
            }

            if (tempDest.isDeleted())
            {
                _logger.debug("destination is deleted");
                throw new JMSException("Cannot consume from a deleted destination");
            }
        }
    }

    protected void checkTransacted() throws JMSException
    {
        if (!getTransacted())
        {
            throw new IllegalStateException("Session is not transacted");
        }
    }

    private void checkValidDestination(Destination destination) throws InvalidDestinationException
    {
        if (destination == null)
        {
            throw new javax.jms.InvalidDestinationException("Invalid Queue");
        }
    }

    private void checkValidQueue(Queue queue) throws InvalidDestinationException
    {
        if (queue == null)
        {
            throw new javax.jms.InvalidDestinationException("Invalid Queue");
        }
    }

    /*
     * I could have combined the last 3 methods, but this way it improves readability
     */
    protected AMQTopic checkValidTopic(Topic topic, boolean durable) throws JMSException
    {
        if (topic == null)
        {
            throw new javax.jms.InvalidDestinationException("Invalid Topic");
        }

        if ((topic instanceof TemporaryDestination) && (((TemporaryDestination) topic).getSession() != this))
        {
            throw new javax.jms.InvalidDestinationException(
                    "Cannot create a subscription on a temporary topic created in another session");
        }

        if ((topic instanceof TemporaryDestination) && durable)
        {
            throw new javax.jms.InvalidDestinationException
                ("Cannot create a durable subscription with a temporary topic: " + topic);
        }

        if (!(topic instanceof AMQTopic))
        {
            throw new javax.jms.InvalidDestinationException(
                    "Cannot create a subscription on topic created for another JMS Provider, class of topic provided is: "
                    + topic.getClass().getName());
        }

        return (AMQTopic) topic;
    }

    protected AMQTopic checkValidTopic(Topic topic) throws JMSException
    {
        return checkValidTopic(topic, false);
    }

    /**
     * Called to close message consumers cleanly. This may or may <b>not</b> be as a result of an error.
     *
     * @param error not null if this is a result of an error occurring at the connection level
     */
    private void closeConsumers(Throwable error) throws JMSException
    {
        // we need to clone the list of consumers since the close() method updates the _consumers collection
        // which would result in a concurrent modification exception
        final ArrayList<C> clonedConsumers = new ArrayList<C>(_consumers.values());

        final Iterator<C> it = clonedConsumers.iterator();
        while (it.hasNext())
        {
            final C con = it.next();
            if (error != null)
            {
                con.notifyError(error);
            }
            else
            {
                con.close(false);
            }
        }
        // at this point the _consumers map will be empty
        if (_dispatcher != null)
        {
            _dispatcher.close();
            _dispatcher = null;
        }
    }

    /**
     * Called to close message producers cleanly. This may or may <b>not</b> be as a result of an error. There is
     * currently no way of propagating errors to message producers (this is a JMS limitation).
     */
    private void closeProducers() throws JMSException
    {
        // we need to clone the list of producers since the close() method updates the _producers collection
        // which would result in a concurrent modification exception
        final ArrayList clonedProducers = new ArrayList(_producers.values());

        final Iterator it = clonedProducers.iterator();
        while (it.hasNext())
        {
            final P prod = (P) it.next();
            prod.close();
        }
        // at this point the _producers map is empty
    }

    /**
     * Close all producers or consumers. This is called either in the error case or when closing the session normally.
     *
     * @param amqe the exception, may be null to indicate no error has occurred
     */
    private void closeProducersAndConsumers(AMQException amqe) throws JMSException
    {
        JMSException jmse = null;
        try
        {
            closeProducers();
        }
        catch (JMSException e)
        {
            _logger.error("Error closing session: " + e, e);
            jmse = e;
        }

        try
        {
            closeConsumers(amqe);
        }
        catch (JMSException e)
        {
            _logger.error("Error closing session: " + e, e);
            if (jmse == null)
            {
                jmse = e;
            }
        }

        if (jmse != null)
        {
            throw jmse;
        }
    }

    /**
     * Register to consume from the queue.
     *
     * @param queueName
     */
    private void consumeFromQueue(C consumer, AMQShortString queueName,
                                  AMQProtocolHandler protocolHandler, boolean nowait, String messageSelector) throws AMQException, FailoverException
    {
        int tagId = _nextTag++;

        consumer.setConsumerTag(tagId);
        // we must register the consumer in the map before we actually start listening
        _consumers.put(tagId, consumer);

        try
        {
            sendConsume(consumer, queueName, protocolHandler, nowait, messageSelector, tagId);
        }
        catch (AMQException e)
        {
            // clean-up the map in the event of an error
            _consumers.remove(tagId);
            throw e;
        }
    }

    public abstract void sendConsume(C consumer, AMQShortString queueName,
                                     AMQProtocolHandler protocolHandler, boolean nowait, String messageSelector, int tag) throws AMQException, FailoverException;

    private P createProducerImpl(Destination destination, boolean mandatory, boolean immediate)
            throws JMSException
    {
        return createProducerImpl(destination, mandatory, immediate, DEFAULT_WAIT_ON_SEND);
    }

    private P createProducerImpl(final Destination destination, final boolean mandatory,
                                                    final boolean immediate, final boolean waitUntilSent) throws JMSException
    {
        return new FailoverRetrySupport<P, JMSException>(
                new FailoverProtectedOperation<P, JMSException>()
                {
                    public P execute() throws JMSException, FailoverException
                    {
                        checkNotClosed();
                        long producerId = getNextProducerId();
                        P producer = createMessageProducer(destination, mandatory,
                                                                              immediate, waitUntilSent, producerId);
                        registerProducer(producerId, producer);

                        return producer;
                    }
                }, _connection).execute();
    }

    public abstract P createMessageProducer(final Destination destination, final boolean mandatory,
                                                               final boolean immediate, final boolean waitUntilSent, long producerId);

    private void declareExchange(AMQDestination amqd, AMQProtocolHandler protocolHandler, boolean nowait) throws AMQException
    {
        declareExchange(amqd.getExchangeName(), amqd.getExchangeClass(), protocolHandler, nowait);
    }

    /**
     * Returns the number of messages currently queued for the given destination.
     *
     * <p/>Note that this operation automatically retries in the event of fail-over.
     *
     * @param amqd The destination to be checked
     *
     * @return the number of queued messages.
     *
     * @throws AMQException If the queue cannot be declared for any reason.
     */
    public long getQueueDepth(final AMQDestination amqd)
            throws AMQException
    {
        return new FailoverNoopSupport<Long, AMQException>(
                new FailoverProtectedOperation<Long, AMQException>()
                {
                    public Long execute() throws AMQException, FailoverException
                    {
                        return requestQueueDepth(amqd);
                    }
                }, _connection).execute();

    }

    protected abstract Long requestQueueDepth(AMQDestination amqd) throws AMQException, FailoverException;

    /**
     * Declares the named exchange and type of exchange.
     *
     * <p/>Note that this operation automatically retries in the event of fail-over.
     *
     * @param name            The name of the exchange to declare.
     * @param type            The type of the exchange to declare.
     * @param protocolHandler The protocol handler to process the communication through.
     * @param nowait
     *
     * @throws AMQException If the exchange cannot be declared for any reason.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    private void declareExchange(final AMQShortString name, final AMQShortString type,
                                 final AMQProtocolHandler protocolHandler, final boolean nowait) throws AMQException
    {
        new FailoverNoopSupport<Object, AMQException>(new FailoverProtectedOperation<Object, AMQException>()
        {
            public Object execute() throws AMQException, FailoverException
            {
                sendExchangeDeclare(name, type, protocolHandler, nowait);
                return null;
            }
        }, _connection).execute();
    }

    public abstract void sendExchangeDeclare(final AMQShortString name, final AMQShortString type, final AMQProtocolHandler protocolHandler,
                                             final boolean nowait) throws AMQException, FailoverException;

    /**
     * Declares a queue for a JMS destination.
     *
     * <p/>Note that for queues but not topics the name is generated in the client rather than the server. This allows
     * the name to be reused on failover if required. In general, the destination indicates whether it wants a name
     * generated or not.
     *
     * <p/>Note that this operation automatically retries in the event of fail-over.
     *
     * @param amqd            The destination to declare as a queue.
     * @param protocolHandler The protocol handler to communicate through.
     *
     * @return The name of the decalred queue. This is useful where the broker is generating a queue name on behalf of
     *         the client.
     *
     * @throws AMQException If the queue cannot be declared for any reason.
     * @todo Verify the destiation is valid or throw an exception.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    protected AMQShortString declareQueue(final AMQDestination amqd, final AMQProtocolHandler protocolHandler,
                                          final boolean noLocal) throws AMQException
    {
        return declareQueue(amqd, protocolHandler, noLocal, false);
    }

    protected AMQShortString declareQueue(final AMQDestination amqd, final AMQProtocolHandler protocolHandler,
                                          final boolean noLocal, final boolean nowait)
            throws AMQException
    {
        /*return new FailoverRetrySupport<AMQShortString, AMQException>(*/
        return new FailoverNoopSupport<AMQShortString, AMQException>(
                new FailoverProtectedOperation<AMQShortString, AMQException>()
                {
                    public AMQShortString execute() throws AMQException, FailoverException
                    {
                        // Generate the queue name if the destination indicates that a client generated name is to be used.
                        if (amqd.isNameRequired())
                        {
                            amqd.setQueueName(protocolHandler.generateQueueName());
                        }

                        sendQueueDeclare(amqd, protocolHandler, nowait);

                        return amqd.getAMQQueueName();
                    }
                }, _connection).execute();
    }

    public abstract void sendQueueDeclare(final AMQDestination amqd, final AMQProtocolHandler protocolHandler,
                                          final boolean nowait) throws AMQException, FailoverException;

    /**
     * Undeclares the specified queue.
     *
     * <p/>Note that this operation automatically retries in the event of fail-over.
     *
     * @param queueName The name of the queue to delete.
     *
     * @throws JMSException If the queue could not be deleted for any reason.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    protected void deleteQueue(final AMQShortString queueName) throws JMSException
    {
        try
        {
            new FailoverRetrySupport<Object, AMQException>(new FailoverProtectedOperation<Object, AMQException>()
            {
                public Object execute() throws AMQException, FailoverException
                {
                    sendQueueDelete(queueName);
                    return null;
                }
            }, _connection).execute();
        }
        catch (AMQException e)
        {
            throw new JMSAMQException("The queue deletion failed: " + e.getMessage(), e);
        }
    }

    public abstract void sendQueueDelete(final AMQShortString queueName) throws AMQException, FailoverException;

    private long getNextProducerId()
    {
        return ++_nextProducerId;
    }

    protected AMQProtocolHandler getProtocolHandler()
    {
        return _connection.getProtocolHandler();
    }

    public byte getProtocolMajorVersion()
    {
        return getProtocolHandler().getProtocolMajorVersion();
    }

    public byte getProtocolMinorVersion()
    {
        return getProtocolHandler().getProtocolMinorVersion();
    }

    protected boolean hasMessageListeners()
    {
        return _hasMessageListeners;
    }

    private void markClosedConsumers() throws JMSException
    {
        if (_dispatcher != null)
        {
            _dispatcher.close();
            _dispatcher = null;
        }
        // we need to clone the list of consumers since the close() method updates the _consumers collection
        // which would result in a concurrent modification exception
        final ArrayList<C> clonedConsumers = new ArrayList<C>(_consumers.values());

        final Iterator<C> it = clonedConsumers.iterator();
        while (it.hasNext())
        {
            final C con = it.next();
            con.markClosed();
        }
        // at this point the _consumers map will be empty
    }

    private void markClosedProducersAndConsumers()
    {
        try
        {
            // no need for a markClosed* method in this case since there is no protocol traffic closing a producer
            closeProducers();
        }
        catch (JMSException e)
        {
            _logger.error("Error closing session: " + e, e);
        }

        try
        {
            markClosedConsumers();
        }
        catch (JMSException e)
        {
            _logger.error("Error closing session: " + e, e);
        }
    }

    /**
     * Callers must hold the failover mutex before calling this method.
     *
     * @param consumer
     *
     * @throws AMQException
     */
    private void registerConsumer(C consumer, boolean nowait) throws AMQException // , FailoverException
    {
        AMQDestination amqd = consumer.getDestination();

        AMQProtocolHandler protocolHandler = getProtocolHandler();

        if (DECLARE_EXCHANGES)
        {
            declareExchange(amqd, protocolHandler, nowait);
        }

        if (DECLARE_QUEUES || amqd.isNameRequired())
        {
            declareQueue(amqd, protocolHandler, consumer.isNoLocal(), nowait);
        }
        AMQShortString queueName = amqd.getAMQQueueName();

        // store the consumer queue name
        consumer.setQueuename(queueName);

        bindQueue(queueName, amqd.getRoutingKey(), consumer.getArguments(), amqd.getExchangeName(), amqd, nowait);

        // If IMMEDIATE_PREFETCH is not required then suspsend the channel to delay prefetch
        if (!_immediatePrefetch)
        {
            // The dispatcher will be null if we have just created this session
            // so suspend the channel before we register our consumer so that we don't
            // start prefetching until a receive/mListener is set.
            if (_dispatcher == null)
            {
                if (!isSuspended())
                {
                    try
                    {
                        suspendChannel(true);
                        _logger.info(
                                "Prefetching delayed existing messages will not flow until requested via receive*() or setML().");
                    }
                    catch (AMQException e)
                    {
                        _logger.info("Suspending channel threw an exception:" + e);
                    }
                }
            }
        }
        else
        {
            _logger.info("Immediately prefetching existing messages to new consumer.");
        }

        try
        {
            consumeFromQueue(consumer, queueName, protocolHandler, nowait, consumer._messageSelector);
        }
        catch (FailoverException e)
        {
            throw new AMQException(null, "Fail-over exception interrupted basic consume.", e);
        }
    }

    private void registerProducer(long producerId, MessageProducer producer)
    {
        _producers.put(new Long(producerId), producer);
    }

    private void rejectAllMessages(boolean requeue)
    {
        rejectMessagesForConsumerTag(0, requeue, true);
    }

    /**
     * @param consumerTag The consumerTag to prune from queue or all if null
     * @param requeue     Should the removed messages be requeued (or discarded. Possibly to DLQ)
     * @param rejectAllConsumers
     */

    private void rejectMessagesForConsumerTag(int consumerTag, boolean requeue, boolean rejectAllConsumers)
    {
        Iterator messages = _queue.iterator();
        if (_logger.isInfoEnabled())
        {
            _logger.info("Rejecting messages from _queue for Consumer tag(" + consumerTag + ") (PDispatchQ) requeue:"
                         + requeue);

            if (messages.hasNext())
            {
                _logger.info("Checking all messages in _queue for Consumer tag(" + consumerTag + ")");
            }
            else
            {
                _logger.info("No messages in _queue to reject");
            }
        }
        while (messages.hasNext())
        {
            UnprocessedMessage message = (UnprocessedMessage) messages.next();

            if (rejectAllConsumers || (message.getConsumerTag() == consumerTag))
            {
                if (_logger.isDebugEnabled())
                {
                    _logger.debug("Removing message(" + System.identityHashCode(message) + ") from _queue DT:"
                                  + message.getDeliveryTag());
                }

                messages.remove();

                rejectMessage(message, requeue);

                if (_logger.isDebugEnabled())
                {
                    _logger.debug("Rejected the message(" + message.toString() + ") for consumer :" + consumerTag);
                }
            }
        }
    }

    private void resubscribeConsumers() throws AMQException
    {
        ArrayList<C> consumers = new ArrayList<C>(_consumers.values());
        _consumers.clear();

        for (C consumer : consumers)
        {
            consumer.failedOverPre();
            registerConsumer(consumer, true);
            consumer.failedOverPost();
        }
    }

    private void resubscribeProducers() throws AMQException
    {
        ArrayList producers = new ArrayList(_producers.values());
        _logger.info(MessageFormat.format("Resubscribing producers = {0} producers.size={1}", producers, producers.size())); // FIXME: removeKey
        for (Iterator it = producers.iterator(); it.hasNext();)
        {
            P producer = (P) it.next();
            producer.resubscribe();
        }
    }

    /**
     * Suspends or unsuspends this session.
     *
     * @param suspend <tt>true</tt> indicates that the session should be suspended, <tt>false<tt> indicates that it
     *                should be unsuspended.
     *
     * @throws AMQException If the session cannot be suspended for any reason.
     * @todo Be aware of possible changes to parameter order as versions change.
     */
    protected void suspendChannel(boolean suspend) throws AMQException // , FailoverException
    {
        synchronized (_suspensionLock)
        {
            try
            {
                if (_logger.isDebugEnabled())
                {
                    _logger.debug("Setting channel flow : " + (suspend ? "suspended" : "unsuspended"));
                }

                _suspended = suspend;
                sendSuspendChannel(suspend);
            }
            catch (FailoverException e)
            {
                throw new AMQException(null, "Fail-over interrupted suspend/unsuspend channel.", e);
            }
        }
    }

    public abstract void sendSuspendChannel(boolean suspend) throws AMQException, FailoverException;

    Object getMessageDeliveryLock()
    {
        return _messageDeliveryLock;
    }

    /**
     * Indicates whether this session consumers pre-fetche messages
     *
     * @return true if this session consumers pre-fetche messages false otherwise
     */
    public boolean prefetch()
    {
        return getAMQConnection().getMaxPrefetch() > 0;
    }

    /** Signifies that the session has pending sends to commit. */
    public void markDirty()
    {
        _dirty = true;
    }

    /** Signifies that the session has no pending sends to commit. */
    public void markClean()
    {
        _dirty = false;
        _failedOverDirty = false;
    }

    /**
     * Check to see if failover has occured since the last call to markClean(commit or rollback).
     *
     * @return boolean true if failover has occured.
     */
    public boolean hasFailedOver()
    {
        return _failedOverDirty;
    }

    /**
     * Check to see if any message have been sent in this transaction and have not been commited.
     *
     * @return boolean true if a message has been sent but not commited
     */
    public boolean isDirty()
    {
        return _dirty;
    }

    public void setTicket(int ticket)
    {
        _ticket = ticket;
    }

    public void setFlowControl(final boolean active)
    {
        _flowControl.setFlowControl(active);
        _logger.warn("Broker enforced flow control " + (active ? "no longer in effect" : "has been enforced"));
    }

    public void checkFlowControl() throws InterruptedException, JMSException
    {
        long expiryTime = 0L;
        synchronized (_flowControl)
        {
            while (!_flowControl.getFlowControl() &&
                   (expiryTime == 0L ? (expiryTime = System.currentTimeMillis() + FLOW_CONTROL_WAIT_FAILURE)
                                     : expiryTime) >= System.currentTimeMillis() )
            {

                _flowControl.wait(FLOW_CONTROL_WAIT_PERIOD);
                _logger.warn("Message send delayed by " + (System.currentTimeMillis() + FLOW_CONTROL_WAIT_FAILURE - expiryTime)/1000 + "s due to broker enforced flow control");
            }
            if(!_flowControl.getFlowControl())
            {
                _logger.error("Message send failed due to timeout waiting on broker enforced flow control");
                throw new JMSException("Unable to send message for " + FLOW_CONTROL_WAIT_FAILURE/1000 + " seconds due to broker enforced flow control");
            }
        }

    }

    public interface Dispatchable
    {
        void dispatch(AMQSession ssn);
    }

    public void dispatch(UnprocessedMessage message)
    {
        if (_dispatcher == null)
        {
            throw new java.lang.IllegalStateException("dispatcher is not started");
        }

        _dispatcher.dispatchMessage(message);
    }

    /** Used for debugging in the dispatcher. */
    private static final Logger _dispatcherLogger = LoggerFactory.getLogger("org.apache.qpid.client.AMQSession.Dispatcher");

    /** Responsible for decoding a message fragment and passing it to the appropriate message consumer. */
    class Dispatcher implements Runnable
    {

        /** Track the 'stopped' state of the dispatcher, a session starts in the stopped state. */
        private final AtomicBoolean _closed = new AtomicBoolean(false);

        private final Object _lock = new Object();
        private String dispatcherID = "" + System.identityHashCode(this);

        public Dispatcher()
        {
        }

        public void close()
        {
            _closed.set(true);
            _dispatcherThread.interrupt();

            // fixme awaitTermination

        }

        public void rejectPending(C consumer)
        {
            synchronized (_lock)
            {
                boolean stopped = _dispatcher.connectionStopped();

                if (!stopped)
                {
                    _dispatcher.setConnectionStopped(true);
                }

                // Reject messages on pre-receive queue
                consumer.rollbackPendingMessages();

                // Reject messages on pre-dispatch queue
                rejectMessagesForConsumerTag(consumer.getConsumerTag(), true, false);
                //Let the dispatcher deal with this when it gets to them.

                // closeConsumer
                consumer.markClosed();

                _dispatcher.setConnectionStopped(stopped);

            }
        }

        public void rollback()
        {

            synchronized (_lock)
            {
                boolean isStopped = connectionStopped();

                if (!isStopped)
                {
                    setConnectionStopped(true);
                }

                _rollbackMark.set(_highestDeliveryTag.get());

                _dispatcherLogger.debug("Session Pre Dispatch Queue cleared");

                for (C consumer : _consumers.values())
                {
                    if (!consumer.isNoConsume())
                    {
                        consumer.rollback();
                    }
                    else
                    {
                        // should perhaps clear the _SQ here.
                        // consumer._synchronousQueue.clear();
                        consumer.clearReceiveQueue();
                    }

                }

                for (int i = 0; i < _removedConsumers.size(); i++)
                {
                    // Sends acknowledgement to server
                    _removedConsumers.get(i).rollback();
                    _removedConsumers.remove(i);
                }

                setConnectionStopped(isStopped);
            }

        }

        public void run()
        {
            if (_dispatcherLogger.isInfoEnabled())
            {
                _dispatcherLogger.info(_dispatcherThread.getName() + " started");
            }

            UnprocessedMessage message;

            // Allow disptacher to start stopped
            synchronized (_lock)
            {
                while (!_closed.get() && connectionStopped())
                {
                    try
                    {
                        _lock.wait();
                    }
                    catch (InterruptedException e)
                    {
                        // ignore
                    }
                }
            }

            try
            {
                Dispatchable disp;
                while (!_closed.get() && ((disp = (Dispatchable) _queue.take()) != null))
                {
                    disp.dispatch(AMQSession.this);
                }
            }
            catch (InterruptedException e)
            {
                // ignore
            }

            if (_dispatcherLogger.isInfoEnabled())
            {
                _dispatcherLogger.info(_dispatcherThread.getName() + " thread terminating for channel " + _channelId);
            }
        }

        // only call while holding lock
        final boolean connectionStopped()
        {
            return _connectionStopped;
        }

        boolean setConnectionStopped(boolean connectionStopped)
        {
            boolean currently;
            synchronized (_lock)
            {
                currently = _connectionStopped;
                _connectionStopped = connectionStopped;
                _lock.notify();

                if (_dispatcherLogger.isDebugEnabled())
                {
                    _dispatcherLogger.debug("Set Dispatcher Connection " + (connectionStopped ? "Stopped" : "Started")
                                            + ": Currently " + (currently ? "Stopped" : "Started"));
                }
            }

            return currently;
        }

        private void dispatchMessage(UnprocessedMessage message)
        {
            long deliveryTag = message.getDeliveryTag();

            synchronized (_lock)
            {

                try
                {
                    while (connectionStopped())
                    {
                        _lock.wait();
                    }
                }
                catch (InterruptedException e)
                {
                    // pass
                }

                if (!(message instanceof CloseConsumerMessage)
                    && tagLE(deliveryTag, _rollbackMark.get()))
                {
                    rejectMessage(message, true);
                }
                else
                {
                    synchronized (_messageDeliveryLock)
                    {
                        notifyConsumer(message);
                    }
                }
            }

            long current = _rollbackMark.get();
            if (updateRollbackMark(current, deliveryTag))
            {
                _rollbackMark.compareAndSet(current, deliveryTag);
            }
        }

        private void notifyConsumer(UnprocessedMessage message)
        {
            final C consumer = _consumers.get(message.getConsumerTag());

            if ((consumer == null) || consumer.isClosed())
            {
                if (_dispatcherLogger.isInfoEnabled())
                {
                    if (consumer == null)
                    {
                        _dispatcherLogger.info("Dispatcher(" + dispatcherID + ")Received a message("
                                               + System.identityHashCode(message) + ")" + "["
                                               + message.getDeliveryTag() + "] from queue "
                                               + message.getConsumerTag() + " )without a handler - rejecting(requeue)...");
                    }
                    else
                    {
                        if (consumer.isNoConsume())
                        {
                            _dispatcherLogger.info("Received a message("
                                                   + System.identityHashCode(message) + ")" + "["
                                                   + message.getDeliveryTag() + "] from queue " + " consumer("
                                                   + message.getConsumerTag() + ") is closed and a browser so dropping...");
                            //DROP MESSAGE
                            return;

                        }
                        else
                        {
                            _dispatcherLogger.info("Received a message("
                                                   + System.identityHashCode(message) + ")" + "["
                                                   + message.getDeliveryTag() + "] from queue " + " consumer("
                                                   + message.getConsumerTag() + ") is closed rejecting(requeue)...");
                        }
                    }
                }
                // Don't reject if we're already closing
                if (!_closed.get())
                {
                    rejectMessage(message, true);
                }
            }
            else
            {
                consumer.notifyMessage(message);
            }
        }
    }

    protected abstract boolean tagLE(long tag1, long tag2);

    protected abstract boolean updateRollbackMark(long current, long deliveryTag);

    public abstract AMQMessageDelegateFactory getMessageDelegateFactory();

    /*public void requestAccess(AMQShortString realm, boolean exclusive, boolean passive, boolean active, boolean write,
        boolean read) throws AMQException
    {
        getProtocolHandler().writeCommandFrameAndWaitForReply(AccessRequestBody.createAMQFrame(getChannelId(),
                getProtocolMajorVersion(), getProtocolMinorVersion(), active, exclusive, passive, read, realm, write),
            new BlockingMethodFrameListener(_channelId)
            {

                public boolean processMethod(int channelId, AMQMethodBody frame) // throws AMQException
                {
                    if (frame instanceof AccessRequestOkBody)
                    {
                        setTicket(((AccessRequestOkBody) frame).getTicket());

                        return true;
                    }
                    else
                    {
                        return false;
                    }
                }
            });
    }*/

    private class SuspenderRunner implements Runnable
    {
        private AtomicBoolean _suspend;

        public SuspenderRunner(AtomicBoolean suspend)
        {
            _suspend = suspend;
        }

        public void run()
        {
            try
            {
                synchronized (_suspensionLock)
                {
                    suspendChannel(_suspend.get());
                }
            }
            catch (AMQException e)
            {
                _logger.warn("Unable to suspend channel");
            }
        }
    }

    /**
     * Checks if the Session and its parent connection are closed
     *
     * @return <tt>true</tt> if this is closed, <tt>false</tt> otherwise.
     */
    @Override
    public boolean isClosed()
    {
        return _closed.get() || _connection.isClosed();
    }

    /**
     * Checks if the Session and its parent connection are capable of performing
     * closing operations
     *
     * @return <tt>true</tt> if we are closing, <tt>false</tt> otherwise.
     */
    @Override
    public boolean isClosing()
    {
        return _closing.get()|| _connection.isClosing();
    }
}
