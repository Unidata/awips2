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


import javax.jms.*;
import javax.jms.IllegalStateException;

import org.apache.qpid.AMQException;
import org.apache.qpid.AMQUndeliveredException;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.client.failover.FailoverProtectedOperation;
import org.apache.qpid.client.failover.FailoverRetrySupport;
import org.apache.qpid.client.message.*;
import org.apache.qpid.client.message.AMQMessageDelegateFactory;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.client.state.listener.SpecificMethodFrameListener;
import org.apache.qpid.client.state.AMQState;
import org.apache.qpid.common.AMQPFilterTypes;
import org.apache.qpid.framing.*;
import org.apache.qpid.framing.amqp_0_91.MethodRegistry_0_91;
import org.apache.qpid.framing.amqp_0_9.MethodRegistry_0_9;
import org.apache.qpid.jms.Session;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.protocol.AMQMethodEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public final class AMQSession_0_8 extends AMQSession<BasicMessageConsumer_0_8, BasicMessageProducer_0_8>
{

    /** Used for debugging. */
    private static final Logger _logger = LoggerFactory.getLogger(AMQSession.class);

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
    AMQSession_0_8(AMQConnection con, int channelId, boolean transacted, int acknowledgeMode,
               MessageFactoryRegistry messageFactoryRegistry, int defaultPrefetchHighMark, int defaultPrefetchLowMark)
    {

         super(con,channelId,transacted,acknowledgeMode,messageFactoryRegistry,defaultPrefetchHighMark,defaultPrefetchLowMark);
    }

    /**
     * Creates a new session on a connection with the default message factory factory.
     *
     * @param con                     The connection on which to create the session.
     * @param channelId               The unique identifier for the session.
     * @param transacted              Indicates whether or not the session is transactional.
     * @param acknowledgeMode         The acknoledgement mode for the session.
     * @param defaultPrefetchHigh     The maximum number of messages to prefetched before suspending the session.
     * @param defaultPrefetchLow      The number of prefetched messages at which to resume the session.
     */
    AMQSession_0_8(AMQConnection con, int channelId, boolean transacted, int acknowledgeMode, int defaultPrefetchHigh,
               int defaultPrefetchLow)
    {
        this(con, channelId, transacted, acknowledgeMode, MessageFactoryRegistry.newDefaultRegistry(), defaultPrefetchHigh,
             defaultPrefetchLow);
    }

    private ProtocolVersion getProtocolVersion()
    {
        return getProtocolHandler().getProtocolVersion();
    }

    public void acknowledgeMessage(long deliveryTag, boolean multiple)
    {
        BasicAckBody body = getMethodRegistry().createBasicAckBody(deliveryTag, multiple);

        final AMQFrame ackFrame = body.generateFrame(_channelId);

        if (_logger.isDebugEnabled())
        {
            _logger.debug("Sending ack for delivery tag " + deliveryTag + " on channel " + _channelId);
        }

        getProtocolHandler().writeFrame(ackFrame);
        _unacknowledgedMessageTags.remove(deliveryTag);
    }

    public void sendQueueBind(final AMQShortString queueName, final AMQShortString routingKey, final FieldTable arguments,
                              final AMQShortString exchangeName, final AMQDestination dest,
                              final boolean nowait) throws AMQException, FailoverException
    {
        getProtocolHandler().syncWrite(getProtocolHandler().getMethodRegistry().createQueueBindBody
                                        (getTicket(),queueName,exchangeName,routingKey,false,arguments).
                                        generateFrame(_channelId), QueueBindOkBody.class);
    }

    public void sendClose(long timeout) throws AMQException, FailoverException
    {
        // we also need to check the state manager for 08/09 as the
        // _connection variable may not be updated in time by the error receiving
        // thread.
        // We can't close the session if we are alreadying in the process of
        // closing/closed the connection.
                
        if (!(getProtocolHandler().getStateManager().getCurrentState().equals(AMQState.CONNECTION_CLOSED)
            || getProtocolHandler().getStateManager().getCurrentState().equals(AMQState.CONNECTION_CLOSING)))
        {

            getProtocolHandler().closeSession(this);
            getProtocolHandler().syncWrite(getProtocolHandler().getMethodRegistry().createChannelCloseBody(AMQConstant.REPLY_SUCCESS.getCode(),
                                                                                                           new AMQShortString("JMS client closing channel"), 0, 0).generateFrame(_channelId),
                                           ChannelCloseOkBody.class, timeout);
            // When control resumes at this point, a reply will have been received that
            // indicates the broker has closed the channel successfully.
        }
    }

    public void sendCommit() throws AMQException, FailoverException
    {
        final AMQProtocolHandler handler = getProtocolHandler();

        handler.syncWrite(getProtocolHandler().getMethodRegistry().createTxCommitBody().generateFrame(_channelId), TxCommitOkBody.class);
    }

    public void sendCreateQueue(AMQShortString name, final boolean autoDelete, final boolean durable, final boolean exclusive, final Map<String, Object> arguments) throws AMQException,
            FailoverException
    {
        FieldTable table = null;
        if(arguments != null && !arguments.isEmpty())
        {
            table = new FieldTable();
            for(Map.Entry<String, Object> entry : arguments.entrySet())
            {
                table.setObject(entry.getKey(), entry.getValue());
            }
        }
        QueueDeclareBody body = getMethodRegistry().createQueueDeclareBody(getTicket(),name,false,durable,exclusive,autoDelete,false,table);
        AMQFrame queueDeclare = body.generateFrame(_channelId);
        getProtocolHandler().syncWrite(queueDeclare, QueueDeclareOkBody.class);
    }

    public void sendRecover() throws AMQException, FailoverException
    {
        _unacknowledgedMessageTags.clear();

        if (isStrictAMQP())
        {
            // We can't use the BasicRecoverBody-OK method as it isn't part of the spec.

            BasicRecoverBody body = getMethodRegistry().createBasicRecoverBody(false);
            _connection.getProtocolHandler().writeFrame(body.generateFrame(_channelId));
            _logger.warn("Session Recover cannot be guaranteed with STRICT_AMQP. Messages may arrive out of order.");
        }
        else
        {
            // in Qpid the 0-8 spec was hacked to have a recover-ok method... this is bad
            // in 0-9 we used the cleaner addition of a new sync recover method with its own ok
            if(getProtocolHandler().getProtocolVersion().equals(ProtocolVersion.v8_0))
            {
                BasicRecoverBody body = getMethodRegistry().createBasicRecoverBody(false);
                _connection.getProtocolHandler().syncWrite(body.generateFrame(_channelId), BasicRecoverOkBody.class);
            }
            else if(getProtocolVersion().equals(ProtocolVersion.v0_9))
            {
                BasicRecoverSyncBody body = ((MethodRegistry_0_9)getMethodRegistry()).createBasicRecoverSyncBody(false);
                _connection.getProtocolHandler().syncWrite(body.generateFrame(_channelId), BasicRecoverSyncOkBody.class);
            }
            else if(getProtocolVersion().equals(ProtocolVersion.v0_91))
            {
                BasicRecoverSyncBody body = ((MethodRegistry_0_91)getMethodRegistry()).createBasicRecoverSyncBody(false);
                _connection.getProtocolHandler().syncWrite(body.generateFrame(_channelId), BasicRecoverSyncOkBody.class);
            }
            else
            {
                throw new RuntimeException("Unsupported version of the AMQP Protocol: " + getProtocolVersion());
            }
        }
    }

    public void releaseForRollback()
    {
        // Reject all the messages that have been received in this session and
        // have not yet been acknowledged. Should look to remove
        // _deliveredMessageTags and use _txRangeSet as used by 0-10.
        // Otherwise messages will be able to arrive out of order to a second
        // consumer on the queue. Whilst this is within the JMS spec it is not
        // user friendly and avoidable.
        while (true)
        {
            Long tag = _deliveredMessageTags.poll();
            if (tag == null)
            {
                break;
            }

            rejectMessage(tag, true);
        }
    }

    public void rejectMessage(long deliveryTag, boolean requeue)
    {
        if ((_acknowledgeMode == CLIENT_ACKNOWLEDGE) || (_acknowledgeMode == SESSION_TRANSACTED))
        {
            if (_logger.isDebugEnabled())
            {
                _logger.debug("Rejecting delivery tag:" + deliveryTag + ":SessionHC:" + this.hashCode());
            }

            BasicRejectBody body = getMethodRegistry().createBasicRejectBody(deliveryTag, requeue);
            AMQFrame frame = body.generateFrame(_channelId);

            _connection.getProtocolHandler().writeFrame(frame);
        }
    }

    public boolean isQueueBound(final AMQDestination destination) throws JMSException
    {
        return isQueueBound(destination.getExchangeName(),destination.getAMQQueueName(),destination.getAMQQueueName());
    }


    public boolean isQueueBound(final AMQShortString exchangeName, final AMQShortString queueName, final AMQShortString routingKey)
            throws JMSException
    {
        try
        {
            AMQMethodEvent response = new FailoverRetrySupport<AMQMethodEvent, AMQException>(
                    new FailoverProtectedOperation<AMQMethodEvent, AMQException>()
                    {
                        public AMQMethodEvent execute() throws AMQException, FailoverException
                        {
                            AMQFrame boundFrame = getProtocolHandler().getMethodRegistry().createExchangeBoundBody
                                                    (exchangeName, routingKey, queueName).generateFrame(_channelId);

                            return getProtocolHandler().syncWrite(boundFrame, ExchangeBoundOkBody.class);

                        }
                    }, _connection).execute();

            // Extract and return the response code from the query.
            ExchangeBoundOkBody responseBody = (ExchangeBoundOkBody) response.getMethod();

            return (responseBody.getReplyCode() == 0);
        }
        catch (AMQException e)
        {
            throw new JMSAMQException("Queue bound query failed: " + e.getMessage(), e);
        }
    }    

    @Override public void sendConsume(BasicMessageConsumer_0_8 consumer,
                                      AMQShortString queueName,
                                      AMQProtocolHandler protocolHandler,
                                      boolean nowait,
                                      String messageSelector,
                                      int tag) throws AMQException, FailoverException
    {
        FieldTable arguments = FieldTableFactory.newFieldTable();
        if ((messageSelector != null) && !messageSelector.equals(""))
        {
            arguments.put(AMQPFilterTypes.JMS_SELECTOR.getValue(), messageSelector);
        }

        if (consumer.isAutoClose())
        {
            arguments.put(AMQPFilterTypes.AUTO_CLOSE.getValue(), Boolean.TRUE);
        }

        if (consumer.isNoConsume())
        {
            arguments.put(AMQPFilterTypes.NO_CONSUME.getValue(), Boolean.TRUE);
        }

        BasicConsumeBody body = getMethodRegistry().createBasicConsumeBody(getTicket(),
                                                                           queueName,
                                                                           new AMQShortString(String.valueOf(tag)),
                                                                           consumer.isNoLocal(),
                                                                           consumer.getAcknowledgeMode() == Session.NO_ACKNOWLEDGE,
                                                                           consumer.isExclusive(),
                                                                           nowait,
                                                                           arguments);


        AMQFrame jmsConsume = body.generateFrame(_channelId);

        if (nowait)
        {
            protocolHandler.writeFrame(jmsConsume);
        }
        else
        {
            protocolHandler.syncWrite(jmsConsume, BasicConsumeOkBody.class);
        }
    }

    public void sendExchangeDeclare(final AMQShortString name, final AMQShortString type, final AMQProtocolHandler protocolHandler,
            final boolean nowait) throws AMQException, FailoverException
    {
        ExchangeDeclareBody body = getMethodRegistry().createExchangeDeclareBody(getTicket(),name,type,
                                                                                 name.toString().startsWith("amq."),
                                                                                 false,false,false,false,null);
        AMQFrame exchangeDeclare = body.generateFrame(_channelId);

        protocolHandler.syncWrite(exchangeDeclare, ExchangeDeclareOkBody.class);
    }

    public void sendQueueDeclare(final AMQDestination amqd, final AMQProtocolHandler protocolHandler,
                                 final boolean nowait) throws AMQException, FailoverException
    {
        QueueDeclareBody body = getMethodRegistry().createQueueDeclareBody(getTicket(),amqd.getAMQQueueName(),false,amqd.isDurable(),amqd.isExclusive(),amqd.isAutoDelete(),false,null);

        AMQFrame queueDeclare = body.generateFrame(_channelId);

        protocolHandler.syncWrite(queueDeclare, QueueDeclareOkBody.class);
    }

    public void sendQueueDelete(final AMQShortString queueName) throws AMQException, FailoverException
    {
        QueueDeleteBody body = getMethodRegistry().createQueueDeleteBody(getTicket(),
                                                                         queueName,
                                                                         false,
                                                                         false,
                                                                         true);
        AMQFrame queueDeleteFrame = body.generateFrame(_channelId);

        getProtocolHandler().syncWrite(queueDeleteFrame, QueueDeleteOkBody.class);
    }

    public void sendSuspendChannel(boolean suspend) throws AMQException, FailoverException
    {
        ChannelFlowBody body = getMethodRegistry().createChannelFlowBody(!suspend);
        AMQFrame channelFlowFrame = body.generateFrame(_channelId);
        _connection.getProtocolHandler().syncWrite(channelFlowFrame, ChannelFlowOkBody.class);
    }

    public BasicMessageConsumer_0_8 createMessageConsumer(final AMQDestination destination, final int prefetchHigh,
            final int prefetchLow, final boolean noLocal, final boolean exclusive, String messageSelector, final FieldTable arguments,
            final boolean noConsume, final boolean autoClose)  throws JMSException
    {

        final AMQProtocolHandler protocolHandler = getProtocolHandler();
       return new BasicMessageConsumer_0_8(_channelId, _connection, destination, messageSelector, noLocal,
                                 _messageFactoryRegistry,this, protocolHandler, arguments, prefetchHigh, prefetchLow,
                                 exclusive, _acknowledgeMode, noConsume, autoClose);
    }


    public BasicMessageProducer_0_8 createMessageProducer(final Destination destination, final boolean mandatory,
            final boolean immediate, final boolean waitUntilSent, long producerId)
    {

       return new BasicMessageProducer_0_8(_connection, (AMQDestination) destination, _transacted, _channelId,
                                 this, getProtocolHandler(), producerId, immediate, mandatory, waitUntilSent);
    }


    @Override public void messageReceived(UnprocessedMessage message)
    {

        if (message instanceof ReturnMessage)
        {
            // Return of the bounced message.
            returnBouncedMessage((ReturnMessage) message);
        }
        else
        {
            super.messageReceived(message);
        }
    }

    private void returnBouncedMessage(final ReturnMessage msg)
    {
        _connection.performConnectionTask(new Runnable()
        {
            public void run()
            {
                try
                {
                    // Bounced message is processed here, away from the mina thread
                    AbstractJMSMessage bouncedMessage =
                            _messageFactoryRegistry.createMessage(0, false, msg.getExchange(),
                                                                  msg.getRoutingKey(), msg.getContentHeader(), msg.getBodies());
                    AMQConstant errorCode = AMQConstant.getConstant(msg.getReplyCode());
                    AMQShortString reason = msg.getReplyText();
                    _logger.debug("Message returned with error code " + errorCode + " (" + reason + ")");

                    // @TODO should this be moved to an exception handler of sorts. Somewhere errors are converted to correct execeptions.
                    if (errorCode == AMQConstant.NO_CONSUMERS)
                    {
                        _connection.exceptionReceived(new AMQNoConsumersException("Error: " + reason, bouncedMessage, null));
                    }
                    else if (errorCode == AMQConstant.NO_ROUTE)
                    {
                        _connection.exceptionReceived(new AMQNoRouteException("Error: " + reason, bouncedMessage, null));
                    }
                    else
                    {
                        _connection.exceptionReceived(
                                new AMQUndeliveredException(errorCode, "Error: " + reason, bouncedMessage, null));
                    }

                }
                catch (Exception e)
                {
                    _logger.error(
                            "Caught exception trying to raise undelivered message exception (dump follows) - ignoring...",
                            e);
                }
            }
        });
    }




    public void sendRollback() throws AMQException, FailoverException
    {
        TxRollbackBody body = getMethodRegistry().createTxRollbackBody();
        AMQFrame frame = body.generateFrame(getChannelId());
        getProtocolHandler().syncWrite(frame, TxRollbackOkBody.class);
    }

    public  TopicSubscriber createDurableSubscriber(Topic topic, String name) throws JMSException
    {

        checkNotClosed();
        AMQTopic origTopic = checkValidTopic(topic, true);
        AMQTopic dest = AMQTopic.createDurableTopic(origTopic, name, _connection);
        TopicSubscriberAdaptor<BasicMessageConsumer_0_8> subscriber = _subscriptions.get(name);
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
                topicName = ((AMQTopic) topic).getRoutingKey();
            }
            else
            {
                topicName = new AMQShortString(topic.getTopicName());
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

        subscriber = new TopicSubscriberAdaptor(dest, (BasicMessageConsumer) createConsumer(dest));

        _subscriptions.put(name, subscriber);
        _reverseSubscriptionMap.put(subscriber.getMessageConsumer(), name);

        return subscriber;
    }




    public void setPrefetchLimits(final int messagePrefetch, final long sizePrefetch) throws AMQException
    {
        new FailoverRetrySupport<Object, AMQException>(
                new FailoverProtectedOperation<Object, AMQException>()
                {
                    public Object execute() throws AMQException, FailoverException
                    {

                        BasicQosBody basicQosBody = getProtocolHandler().getMethodRegistry().createBasicQosBody(sizePrefetch, messagePrefetch, false);

                        // todo send low water mark when protocol allows.
                        // todo Be aware of possible changes to parameter order as versions change.
                        getProtocolHandler().syncWrite(basicQosBody.generateFrame(getChannelId()), BasicQosOkBody.class);
                  
                        return null;
                    }
                 }, _connection).execute();
    }

    class QueueDeclareOkHandler extends SpecificMethodFrameListener
    {

        private long _messageCount;
        private long _consumerCount;

        public QueueDeclareOkHandler()
        {
            super(getChannelId(), QueueDeclareOkBody.class);
        }

        public boolean processMethod(int channelId, AMQMethodBody frame) //throws AMQException
        {
            boolean matches = super.processMethod(channelId, frame);
            if (matches)
            {
                QueueDeclareOkBody declareOk = (QueueDeclareOkBody) frame;
                _messageCount = declareOk.getMessageCount();
                _consumerCount = declareOk.getConsumerCount();
            }
            return matches;
        }

    }

    protected Long requestQueueDepth(AMQDestination amqd) throws AMQException, FailoverException
    {
        AMQFrame queueDeclare =
            getMethodRegistry().createQueueDeclareBody(getTicket(),
                                                       amqd.getAMQQueueName(),
                                                       true,
                                                       amqd.isDurable(),
                                                       amqd.isExclusive(),
                                                       amqd.isAutoDelete(),
                                                       false,
                                                       null).generateFrame(_channelId);
        QueueDeclareOkHandler okHandler = new QueueDeclareOkHandler();
        getProtocolHandler().writeCommandFrameAndWaitForReply(queueDeclare, okHandler);        
        return okHandler._messageCount;
    }

    protected final boolean tagLE(long tag1, long tag2)
    {
        return tag1 <= tag2;
    }

    protected final boolean updateRollbackMark(long currentMark, long deliveryTag)
    {
        return false;
    }

    public AMQMessageDelegateFactory getMessageDelegateFactory()
    {
        return AMQMessageDelegateFactory.FACTORY_0_8;
    }

}
