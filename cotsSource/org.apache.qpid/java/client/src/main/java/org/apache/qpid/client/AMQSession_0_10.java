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

import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.client.failover.FailoverNoopSupport;
import org.apache.qpid.client.failover.FailoverProtectedOperation;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.client.message.MessageFactoryRegistry;
import org.apache.qpid.client.message.FiledTableSupport;
import org.apache.qpid.client.message.AMQMessageDelegateFactory;
import org.apache.qpid.client.message.UnprocessedMessage_0_10;
import org.apache.qpid.util.Serial;
import org.apache.qpid.transport.ExecutionException;
import org.apache.qpid.transport.MessageAcceptMode;
import org.apache.qpid.transport.MessageAcquireMode;
import org.apache.qpid.transport.MessageCreditUnit;
import org.apache.qpid.transport.MessageFlowMode;
import org.apache.qpid.transport.MessageTransfer;
import org.apache.qpid.transport.RangeSet;
import org.apache.qpid.transport.Option;
import org.apache.qpid.transport.ExchangeBoundResult;
import org.apache.qpid.transport.Future;
import org.apache.qpid.transport.Range;
import org.apache.qpid.transport.Session;
import org.apache.qpid.transport.SessionException;
import org.apache.qpid.transport.SessionListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.apache.qpid.transport.Option.*;

import javax.jms.*;
import javax.jms.IllegalStateException;

import java.lang.ref.WeakReference;

import java.util.Date;
import java.util.HashMap;
import java.util.UUID;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.UUID;

/**
 * This is a 0.10 Session
 */
public class AMQSession_0_10 extends AMQSession<BasicMessageConsumer_0_10, BasicMessageProducer_0_10>
    implements SessionListener
{

    /**
     * This class logger
     */
    private static final Logger _logger = LoggerFactory.getLogger(AMQSession_0_10.class);

    private static Timer timer = new Timer("ack-flusher", true);
    private static class Flusher extends TimerTask
    {

        private WeakReference<AMQSession_0_10> session;
        public Flusher(AMQSession_0_10 session)
        {
            this.session = new WeakReference<AMQSession_0_10>(session);
        }

        public void run() {
            AMQSession_0_10 ssn = session.get();
            if (ssn == null)
            {
                cancel();
            }
            else
            {
                try
                {
                    ssn.flushAcknowledgments(true);
                }
                catch (Throwable t)
                {
                    _logger.error("error flushing acks", t);
                }
            }
        }
    }


    /**
     * The underlying QpidSession
     */
    private Session _qpidSession;

    /**
     * The latest qpid Exception that has been reaised.
     */
    private Object _currentExceptionLock = new Object();
    private SessionException _currentException;

    // a ref on the qpid connection
    protected org.apache.qpid.transport.Connection _qpidConnection;

    private long maxAckDelay = Long.getLong("qpid.session.max_ack_delay", 1000);
    private TimerTask flushTask = null;
    private RangeSet unacked = new RangeSet();
    private int unackedCount = 0;

    /**
     * USed to store the range of in tx messages
     */
    private RangeSet _txRangeSet = new RangeSet();
    private int _txSize = 0;
    //--- constructors

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
     * @param qpidConnection          The qpid connection
     */
    AMQSession_0_10(org.apache.qpid.transport.Connection qpidConnection, AMQConnection con, int channelId,
                    boolean transacted, int acknowledgeMode, MessageFactoryRegistry messageFactoryRegistry,
                    int defaultPrefetchHighMark, int defaultPrefetchLowMark)
    {

        super(con, channelId, transacted, acknowledgeMode, messageFactoryRegistry, defaultPrefetchHighMark,
              defaultPrefetchLowMark);
        _qpidConnection = qpidConnection;
        try {
            String clientStr = con.getClientID();
            if (clientStr != null && clientStr.length() > 0) {
                _qpidSession = _qpidConnection.createSession(clientStr + "-" + UUID.randomUUID().toString(), 1);
            } else {
                _qpidSession = _qpidConnection.createSession(1);
            }
        } catch ( JMSException e ) {
            // con.getClientID() throws JMSException on any connection problem
            _qpidSession = _qpidConnection.createSession(1);
        }

        _qpidSession.setSessionListener(this);
        if (_transacted)
        {
            _qpidSession.txSelect();
        }

        if (maxAckDelay > 0)
        {
            flushTask = new Flusher(this);
            timer.schedule(flushTask, new Date(), maxAckDelay);
        }
    }

    /**
     * Creates a new session on a connection with the default 0-10 message factory.
     *
     * @param con                 The connection on which to create the session.
     * @param channelId           The unique identifier for the session.
     * @param transacted          Indicates whether or not the session is transactional.
     * @param acknowledgeMode     The acknoledgement mode for the session.
     * @param defaultPrefetchHigh The maximum number of messages to prefetched before suspending the session.
     * @param defaultPrefetchLow  The number of prefetched messages at which to resume the session.
     * @param qpidConnection      The connection
     */
    AMQSession_0_10(org.apache.qpid.transport.Connection qpidConnection, AMQConnection con, int channelId,
                    boolean transacted, int acknowledgeMode, int defaultPrefetchHigh, int defaultPrefetchLow)
    {

        this(qpidConnection, con, channelId, transacted, acknowledgeMode, MessageFactoryRegistry.newDefaultRegistry(),
             defaultPrefetchHigh, defaultPrefetchLow);
    }

    private void addUnacked(int id)
    {
        synchronized (unacked)
        {
            unacked.add(id);
            unackedCount++;
        }
    }

    private void clearUnacked()
    {
        synchronized (unacked)
        {
            unacked.clear();
            unackedCount = 0;
        }
    }

    //------- overwritten methods of class AMQSession

    void failoverPrep()
    {
        super.failoverPrep();
        clearUnacked();
    }

    /**
     * Acknowledge one or many messages.
     *
     * @param deliveryTag The tag of the last message to be acknowledged.
     * @param multiple    <tt>true</tt> to acknowledge all messages up to and including the one specified by the
     *                    delivery tag, <tt>false</tt> to just acknowledge that message.
     */

    public void acknowledgeMessage(long deliveryTag, boolean multiple)
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Sending ack for delivery tag " + deliveryTag + " on session " + _channelId);
        }
        // acknowledge this message
        if (multiple)
        {
            for (Long messageTag : _unacknowledgedMessageTags)
            {
                if( messageTag <= deliveryTag )
                {
                    addUnacked(messageTag.intValue());
                    _unacknowledgedMessageTags.remove(messageTag);
                }
            }
            //empty the list of unack messages

        }
        else
        {
            addUnacked((int) deliveryTag);
            _unacknowledgedMessageTags.remove(deliveryTag);
        }

        long prefetch = getAMQConnection().getMaxPrefetch();

        if (unackedCount >= prefetch/2 || maxAckDelay <= 0)
        {
            flushAcknowledgments();
        }
    }

    void flushAcknowledgments()
    {
        flushAcknowledgments(false);
    }
    
    void flushAcknowledgments(boolean setSyncBit)
    {
        synchronized (unacked)
        {
            if (unackedCount > 0)
            {
                messageAcknowledge
                    (unacked, _acknowledgeMode != org.apache.qpid.jms.Session.NO_ACKNOWLEDGE,setSyncBit);
                clearUnacked();
            }
        }
    }

    void messageAcknowledge(RangeSet ranges, boolean accept)
    {
        messageAcknowledge(ranges,accept,false);
    }
    
    void messageAcknowledge(RangeSet ranges, boolean accept,boolean setSyncBit)
    {
        Session ssn = getQpidSession();
        for (Range range : ranges)
        {
            ssn.processed(range);
        }
        ssn.flushProcessed(accept ? BATCH : NONE);
        if (accept)
        {
            ssn.messageAccept(ranges, UNRELIABLE,setSyncBit? SYNC : NONE);
        }
    }

    /**
     * Bind a queue with an exchange.
     *
     * @param queueName    Specifies the name of the queue to bind. If the queue name is empty,
     *                     refers to the current
     *                     queue for the session, which is the last declared queue.
     * @param exchangeName The exchange name.
     * @param routingKey   Specifies the routing key for the binding.
     * @param arguments    0_8 specific
     */
    public void sendQueueBind(final AMQShortString queueName, final AMQShortString routingKey,
                              final FieldTable arguments, final AMQShortString exchangeName,
                              final AMQDestination destination, final boolean nowait)
            throws AMQException, FailoverException
    {
        Map args = FiledTableSupport.convertToMap(arguments);
        // this is there only becasue the broker may expect a value for x-match
        if( ! args.containsKey("x-match") )
        {
            args.put("x-match", "any");
        }

        for (AMQShortString rk: destination.getBindingKeys())
        {
            _logger.debug("Binding queue : " + queueName.toString() + " exchange: " + exchangeName.toString() + " using binding key " + rk.asString());
            getQpidSession().exchangeBind(queueName.toString(), exchangeName.toString(), rk.toString(), args);
        }
        if (!nowait)
        {
            // We need to sync so that we get notify of an error.
            getQpidSession().sync();
            getCurrentException();
        }
    }


    /**
     * Close this session.
     *
     * @param timeout no used / 0_8 specific
     * @throws AMQException
     * @throws FailoverException
     */
    public void sendClose(long timeout) throws AMQException, FailoverException
    {
        if (flushTask != null)
        {
            flushTask.cancel();
            flushTask = null;
        }
        flushAcknowledgments();
        getQpidSession().sync();
        getQpidSession().close();
        getCurrentException();
    }


    /**
     * Commit the receipt and the delivery of all messages exchanged by this session resources.
     */
    public void sendCommit() throws AMQException, FailoverException
    {
        getQpidSession().setAutoSync(true);
        try
        {
            getQpidSession().txCommit();
        }
        finally
        {
            getQpidSession().setAutoSync(false);
        }
        // We need to sync so that we get notify of an error.
        getCurrentException();
    }

    /**
     * Create a queue with a given name.
     *
     * @param name       The queue name
     * @param autoDelete If this field is set and the exclusive field is also set,
     *                   then the queue is deleted when the connection closes.
     * @param durable    If set when creating a new queue,
     *                   the queue will be marked as durable.
     * @param exclusive  Exclusive queues can only be used from one connection at a time.
     * @param arguments  Exclusive queues can only be used from one connection at a time.
     * @throws AMQException
     * @throws FailoverException
     */
    public void sendCreateQueue(AMQShortString name, final boolean autoDelete, final boolean durable,
                                final boolean exclusive, Map<String, Object> arguments) throws AMQException, FailoverException
    {
        getQpidSession().queueDeclare(name.toString(), null, arguments, durable ? Option.DURABLE : Option.NONE,
                                      autoDelete ? Option.AUTO_DELETE : Option.NONE,
                                      exclusive ? Option.EXCLUSIVE : Option.NONE);
        // We need to sync so that we get notify of an error.
        getQpidSession().sync();
        getCurrentException();
    }

    /**
     * This method asks the broker to redeliver all unacknowledged messages
     *
     * @throws AMQException
     * @throws FailoverException
     */
    public void sendRecover() throws AMQException, FailoverException
    {
        // release all unack messages
        RangeSet ranges = new RangeSet();
        while (true)
        {
            Long tag = _unacknowledgedMessageTags.poll();
            if (tag == null)
            {
                break;
            }
            ranges.add((int) (long) tag);
        }
        getQpidSession().messageRelease(ranges, Option.SET_REDELIVERED);
        // We need to sync so that we get notify of an error.
        getQpidSession().sync();
        getCurrentException();
    }

    public void releaseForRollback()
    {
        getQpidSession().messageRelease(_txRangeSet, Option.SET_REDELIVERED);
        _txRangeSet.clear();
        _txSize = 0;
    }

    /**
     * Release (0_8 notion of Reject) an acquired message
     *
     * @param deliveryTag the message ID
     * @param requeue     always true
     */
    public void rejectMessage(long deliveryTag, boolean requeue)
    {
        // The value of requeue is always true
        RangeSet ranges = new RangeSet();
        ranges.add((int) deliveryTag);
        getQpidSession().messageRelease(ranges, Option.SET_REDELIVERED);
        //I don't think we need to sync
    }

    /**
     * Create an 0_10 message consumer
     */
    public BasicMessageConsumer_0_10 createMessageConsumer(final AMQDestination destination, final int prefetchHigh,
                                                      final int prefetchLow, final boolean noLocal,
                                                      final boolean exclusive, String messageSelector,
                                                      final FieldTable ft, final boolean noConsume,
                                                      final boolean autoClose) throws JMSException
    {

        final AMQProtocolHandler protocolHandler = getProtocolHandler();
        return new BasicMessageConsumer_0_10(_channelId, _connection, destination, messageSelector, noLocal,
                                             _messageFactoryRegistry, this, protocolHandler, ft, prefetchHigh,
                                             prefetchLow, exclusive, _acknowledgeMode, noConsume, autoClose);
    }

    /**
     * Bind a queue with an exchange.
     */

    public boolean isQueueBound(final AMQShortString exchangeName, final AMQShortString queueName, final AMQShortString routingKey)
    throws JMSException
    {
        return isQueueBound(exchangeName,queueName,routingKey,null);
    }

    public boolean isQueueBound(final AMQDestination destination) throws JMSException
    {
        return isQueueBound(destination.getExchangeName(),destination.getAMQQueueName(),destination.getRoutingKey(),destination.getBindingKeys());
    }

    public boolean isQueueBound(final AMQShortString exchangeName, final AMQShortString queueName, final AMQShortString routingKey,AMQShortString[] bindingKeys)
    throws JMSException
    {
        String rk = null;
        boolean res;
        if (bindingKeys != null && bindingKeys.length>0)
        {
            rk = bindingKeys[0].toString();
        }
        else if (routingKey != null)
        {
            rk = routingKey.toString();
        }

        ExchangeBoundResult bindingQueryResult =
            getQpidSession().exchangeBound(exchangeName.toString(),queueName.toString(), rk, null).get();

        if (rk == null)
        {
            res = !(bindingQueryResult.getExchangeNotFound() || bindingQueryResult.getQueueNotFound());
        }
        else
        {
            res = !(bindingQueryResult.getKeyNotMatched() || bindingQueryResult.getQueueNotFound() || bindingQueryResult
                    .getQueueNotMatched());
        }
        return res;
    }

    /**
     * This method is invoked when a consumer is creted
     * Registers the consumer with the broker
     */
    public void sendConsume(BasicMessageConsumer_0_10 consumer, AMQShortString queueName, AMQProtocolHandler protocolHandler,
                            boolean nowait, String messageSelector, int tag)
            throws AMQException, FailoverException
    {
        boolean preAcquire;
        try
        {
            preAcquire = ( ! consumer.isNoConsume()  &&
                    (consumer.getMessageSelector() == null || consumer.getMessageSelector().equals("")) )
                    || !(consumer.getDestination() instanceof AMQQueue);
            getQpidSession().messageSubscribe
                (queueName.toString(), String.valueOf(tag),
                 getAcknowledgeMode() == NO_ACKNOWLEDGE ? MessageAcceptMode.NONE : MessageAcceptMode.EXPLICIT,
                 preAcquire ? MessageAcquireMode.PRE_ACQUIRED : MessageAcquireMode.NOT_ACQUIRED, null, 0, null,
                 consumer.isExclusive() ? Option.EXCLUSIVE : Option.NONE);
        }
        catch (JMSException e)
        {
            throw new AMQException(AMQConstant.INTERNAL_ERROR, "problem when registering consumer", e);
        }

        String consumerTag = ((BasicMessageConsumer_0_10)consumer).getConsumerTagString();

        if (! prefetch())
        {
            getQpidSession().messageSetFlowMode(consumerTag, MessageFlowMode.CREDIT);
        }
        else
        {
            getQpidSession().messageSetFlowMode(consumerTag, MessageFlowMode.WINDOW);
        }
        getQpidSession().messageFlow(consumerTag, MessageCreditUnit.BYTE, 0xFFFFFFFF,
                                     Option.UNRELIABLE);
        // We need to sync so that we get notify of an error.
        // only if not immediat prefetch
        if(prefetch() && (isStarted() || _immediatePrefetch))
        {
            // set the flow
            getQpidSession().messageFlow(consumerTag,
                                         MessageCreditUnit.MESSAGE,
                                         getAMQConnection().getMaxPrefetch(),
                                         Option.UNRELIABLE);
        }

        if (!nowait)
        {
            getQpidSession().sync();
            getCurrentException();
        }
    }

    /**
     * Create an 0_10 message producer
     */
    public BasicMessageProducer_0_10 createMessageProducer(final Destination destination, final boolean mandatory,
                                                      final boolean immediate, final boolean waitUntilSent,
                                                      long producerId)
    {
        return new BasicMessageProducer_0_10(_connection, (AMQDestination) destination, _transacted, _channelId, this,
                                             getProtocolHandler(), producerId, immediate, mandatory, waitUntilSent);

    }

    /**
     * creates an exchange if it does not already exist
     */
    public void sendExchangeDeclare(final AMQShortString name, final AMQShortString type,
                                    final AMQProtocolHandler protocolHandler, final boolean nowait)
            throws AMQException, FailoverException
    {
        getQpidSession().exchangeDeclare(name.toString(),
                                        type.toString(),
                                        null,
                                        null,
                                        name.toString().startsWith("amq.")? Option.PASSIVE:Option.NONE);
        // We need to sync so that we get notify of an error.
        if (!nowait)
        {
            getQpidSession().sync();
            getCurrentException();
        }
    }

    /**
     * Declare a queue with the given queueName
     */
    public void sendQueueDeclare(final AMQDestination amqd, final AMQProtocolHandler protocolHandler,
                                 final boolean nowait)
            throws AMQException, FailoverException
    {
        // do nothing this is only used by 0_8
    }

    /**
     * Declare a queue with the given queueName
     */
    public AMQShortString send0_10QueueDeclare(final AMQDestination amqd, final AMQProtocolHandler protocolHandler,
                                               final boolean noLocal, final boolean nowait)
            throws AMQException, FailoverException
    {
        AMQShortString res;
        if (amqd.getAMQQueueName() == null)
        {
            // generate a name for this queue
            res = new AMQShortString("TempQueue" + UUID.randomUUID());
        }
        else
        {
            res = amqd.getAMQQueueName();
        }
        Map<String,Object> arguments = null;
        if (noLocal)
        {
            arguments = new HashMap<String,Object>();
            arguments.put("no-local", true);
        }
        getQpidSession().queueDeclare(res.toString(), null, arguments,
                                      amqd.isAutoDelete() ? Option.AUTO_DELETE : Option.NONE,
                                      amqd.isDurable() ? Option.DURABLE : Option.NONE,
                                      !amqd.isDurable() && amqd.isExclusive() ? Option.EXCLUSIVE : Option.NONE);
        // passive --> false
        if (!nowait)
        {
            // We need to sync so that we get notify of an error.
            getQpidSession().sync();
            getCurrentException();
        }
        return res;
    }

    /**
     * deletes a queue
     */
    public void sendQueueDelete(final AMQShortString queueName) throws AMQException, FailoverException
    {
        getQpidSession().queueDelete(queueName.toString());
        // ifEmpty --> false
        // ifUnused --> false
        // We need to sync so that we get notify of an error.
        getQpidSession().sync();
        getCurrentException();
    }

    /**
     * Activate/deactivate the message flow for all the consumers of this session.
     */
    public void sendSuspendChannel(boolean suspend) throws AMQException, FailoverException
    {
        if (suspend)
        {
            for (BasicMessageConsumer consumer : _consumers.values())
            {
                getQpidSession().messageStop(String.valueOf(consumer.getConsumerTag()),
                                             Option.UNRELIABLE);
            }
        }
        else
        {
            for (BasicMessageConsumer_0_10 consumer : _consumers.values())
            {
                String consumerTag = String.valueOf(consumer.getConsumerTag());
                //only set if msg list is null
                try
                {
                    if (! prefetch())
                    {
                        if (consumer.getMessageListener() != null)
                        {
                            getQpidSession().messageFlow(consumerTag,
                                                         MessageCreditUnit.MESSAGE, 1,
                                                         Option.UNRELIABLE);
                        }
                    }
                    else
                    {
                        getQpidSession()
                            .messageFlow(consumerTag, MessageCreditUnit.MESSAGE,
                                         getAMQConnection().getMaxPrefetch(),
                                         Option.UNRELIABLE);
                    }
                    getQpidSession()
                        .messageFlow(consumerTag, MessageCreditUnit.BYTE, 0xFFFFFFFF,
                                     Option.UNRELIABLE);
                }
                catch (Exception e)
                {
                    throw new AMQException(AMQConstant.INTERNAL_ERROR, "Error while trying to get the listener", e);
                }
            }
        }
        // We need to sync so that we get notify of an error.
        getQpidSession().sync();
        getCurrentException();
    }


    public void sendRollback() throws AMQException, FailoverException
    {
        getQpidSession().txRollback();
        // We need to sync so that we get notify of an error.
        getQpidSession().sync();
        getCurrentException();
    }

    //------ Private methods
    /**
     * Access to the underlying Qpid Session
     *
     * @return The associated Qpid Session.
     */
    protected Session getQpidSession()
    {
        return _qpidSession;
    }


    /**
     * Get the latest thrown exception.
     *
     * @throws org.apache.qpid.AMQException get the latest thrown error.
     */
    public void getCurrentException() throws AMQException
    {
        synchronized (_currentExceptionLock)
        {
            if (_currentException != null)
            {
                SessionException se = _currentException;
                _currentException = null;
                ExecutionException ee = se.getException();
                int code;
                if (ee == null)
                {
                    code = 0;
                }
                else
                {
                    code = ee.getErrorCode().getValue();
                }
                throw new AMQException
                    (AMQConstant.getConstant(code), se.getMessage(), se);
            }
        }
    }

    public void opened(Session ssn) {}

    public void resumed(Session ssn)
    {
        _qpidConnection = ssn.getConnection();
        try
        {
            resubscribe();
        }
        catch (AMQException e)
        {
            throw new RuntimeException(e);
        }
    }

    public void message(Session ssn, MessageTransfer xfr)
    {
        messageReceived(new UnprocessedMessage_0_10(xfr));
    }

    public void exception(Session ssn, SessionException exc)
    {
        synchronized (_currentExceptionLock)
        {
            _currentException = exc;
        }
    }

    public void closed(Session ssn) {}

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
                            String binddingKey = "";
                            for(AMQShortString key : amqd.getBindingKeys())
                            {
                               binddingKey = binddingKey + "_" + key.toString();
                            }
                            amqd.setQueueName(new AMQShortString( binddingKey + "@"
                                    + amqd.getExchangeName().toString() + "_" + UUID.randomUUID()));
                        }
                        return send0_10QueueDeclare(amqd, protocolHandler, noLocal, nowait);
                    }
                }, _connection).execute();
    }

    public TopicSubscriber createDurableSubscriber(Topic topic, String name) throws JMSException
    {

        checkNotClosed();
        AMQTopic origTopic=checkValidTopic(topic, true);
        AMQTopic dest=AMQTopic.createDurable010Topic(origTopic, name, _connection);

        TopicSubscriberAdaptor<BasicMessageConsumer_0_10> subscriber=_subscriptions.get(name);
        if (subscriber != null)
        {
            if (subscriber.getTopic().equals(topic))
            {
                throw new IllegalStateException("Already subscribed to topic " + topic + " with subscription exchange "
                        + name);
            }
            else
            {
                unsubscribe(name);
            }
        }
        else
        {
            AMQShortString topicName;
            if (topic instanceof AMQTopic)
            {
                topicName=((AMQTopic) topic).getBindingKeys()[0];
            }
            else
            {
                topicName=new AMQShortString(topic.getTopicName());
            }

            if (_strictAMQP)
            {
                if (_strictAMQPFATAL)
                {
                    throw new UnsupportedOperationException("JMS Durable not currently supported by AMQP.");
                }
                else
                {
                    _logger.warn("Unable to determine if subscription already exists for '" + topicName + "' "
                            + "for creation durableSubscriber. Requesting queue deletion regardless.");
                }

                deleteQueue(dest.getAMQQueueName());
            }
            else
            {
                // if the queue is bound to the exchange but NOT for this topic, then the JMS spec
                // says we must trash the subscription.
                if (isQueueBound(dest.getExchangeName(), dest.getAMQQueueName())
                        && !isQueueBound(dest.getExchangeName(), dest.getAMQQueueName(), topicName))
                {
                    deleteQueue(dest.getAMQQueueName());
                }
            }
        }

        subscriber=new TopicSubscriberAdaptor(dest, createExclusiveConsumer(dest));

        _subscriptions.put(name, subscriber);
        _reverseSubscriptionMap.put(subscriber.getMessageConsumer(), name);

        return subscriber;
    }

    protected Long requestQueueDepth(AMQDestination amqd)
    {
        return getQpidSession().queueQuery(amqd.getQueueName()).get().getMessageCount();
    }


    /**
     * Store non committed messages for this session
     * With 0.10 messages are consumed with window mode, we must send a completion
     * before the window size is reached so credits don't dry up.
     * @param id
     */
    @Override protected void addDeliveredMessage(long id)
    {
        _txRangeSet.add((int) id);
        _txSize++;
        // this is a heuristic, we may want to have that configurable
        if (_connection.getMaxPrefetch() == 1 ||
                _connection.getMaxPrefetch() != 0 && _txSize % (_connection.getMaxPrefetch() / 2) == 0)
        {
            // send completed so consumer credits don't dry up
            messageAcknowledge(_txRangeSet, false);
        }
    }

    @Override public void commit() throws JMSException
    {
        checkTransacted();
        try
        {
            if( _txSize > 0 )
            {
                messageAcknowledge(_txRangeSet, true);
                _txRangeSet.clear();
                _txSize = 0;
            }
            sendCommit();
        }
        catch (AMQException e)
        {
            throw new JMSAMQException("Failed to commit: " + e.getMessage(), e);
        }
        catch (FailoverException e)
        {
            throw new JMSAMQException("Fail-over interrupted commit. Status of the commit is uncertain.", e);
        }
    }

    protected final boolean tagLE(long tag1, long tag2)
    {
        return Serial.le((int) tag1, (int) tag2);
    }

    protected final boolean updateRollbackMark(long currentMark, long deliveryTag)
    {
        return Serial.lt((int) currentMark, (int) deliveryTag);
    }

    public AMQMessageDelegateFactory getMessageDelegateFactory()
    {
        return AMQMessageDelegateFactory.FACTORY_0_10;
    }

}
