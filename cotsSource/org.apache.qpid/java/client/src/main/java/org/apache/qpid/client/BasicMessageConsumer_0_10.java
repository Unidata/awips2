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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.qpid.client.message.*;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.transport.*;
import org.apache.qpid.QpidException;
import org.apache.qpid.filter.MessageFilter;
import org.apache.qpid.filter.JMSSelectorFilter;

import javax.jms.InvalidSelectorException;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * This is a 0.10 message consumer.
 */
public class BasicMessageConsumer_0_10 extends BasicMessageConsumer<UnprocessedMessage_0_10>
{

    /**
     * This class logger
     */
    protected final Logger _logger = LoggerFactory.getLogger(getClass());

    /**
     * The message selector filter associated with this consumer message selector
     */
    private MessageFilter _filter = null;

    /**
     * The underlying QpidSession
     */
    private AMQSession_0_10 _0_10session;

    /**
     * Indicates whether this consumer receives pre-acquired messages
     */
    private boolean _preAcquire = true;

    /**
     * Indicate whether this consumer is started.
     */
    private boolean _isStarted = false;

    /**
     * Specify whether this consumer is performing a sync receive
     */
    private final AtomicBoolean _syncReceive = new AtomicBoolean(false);
    private String _consumerTagString;

    //--- constructor
    protected BasicMessageConsumer_0_10(int channelId, AMQConnection connection, AMQDestination destination,
                                        String messageSelector, boolean noLocal, MessageFactoryRegistry messageFactory,
                                        AMQSession session, AMQProtocolHandler protocolHandler,
                                        FieldTable arguments, int prefetchHigh, int prefetchLow,
                                        boolean exclusive, int acknowledgeMode, boolean noConsume, boolean autoClose)
            throws JMSException
    {
        super(channelId, connection, destination, messageSelector, noLocal, messageFactory, session, protocolHandler,
                arguments, prefetchHigh, prefetchLow, exclusive, acknowledgeMode, noConsume, autoClose);
        _0_10session = (AMQSession_0_10) session;
        if (messageSelector != null && !messageSelector.equals(""))
        {
            try
            {
                _filter = new JMSSelectorFilter(messageSelector);
            }
            catch (QpidException e)
            {
                throw new InvalidSelectorException("cannot create consumer because of selector issue");
            }
            if (destination instanceof AMQQueue)
            {
                _preAcquire = false;
            }
        }
        _isStarted = connection.started();
    }


    @Override public void setConsumerTag(int consumerTag)
    {
        super.setConsumerTag(consumerTag);
        _consumerTagString = String.valueOf(consumerTag);
    }

    public String getConsumerTagString()
    {
        return _consumerTagString;
    }

    /**
     *
     * This is invoked by the session thread when emptying the session message queue.
     * We first check if the message is valid (match the selector) and then deliver it to the
     * message listener or to the sync consumer queue.
     *
     * @param jmsMessage this message has already been processed so can't redo preDeliver
     */
    @Override public void notifyMessage(AbstractJMSMessage jmsMessage)
    {
        boolean messageOk = false;
        try
        {
            messageOk = checkPreConditions(jmsMessage);
        }
        catch (AMQException e)
        {
            _logger.error("Receivecd an Exception when receiving message",e);
            try
            {

                getSession().getAMQConnection().getExceptionListener()
                        .onException(new JMSAMQException("Error when receiving message", e));
            }
            catch (Exception e1)
            {
                // we should silently log thie exception as it only hanppens when the connection is closed
                _logger.error("Exception when receiving message", e1);
            }
        }
        if (messageOk)
        {
            if (isMessageListenerSet() && ! getSession().prefetch())
            {
                _0_10session.getQpidSession().messageFlow(getConsumerTagString(),
                                                          MessageCreditUnit.MESSAGE, 1,
                                                          Option.UNRELIABLE);
            }
            _logger.debug("messageOk, trying to notify");
            super.notifyMessage(jmsMessage);
        }
    }

    //----- overwritten methods

    /**
     * This method is invoked when this consumer is stopped.
     * It tells the broker to stop delivering messages to this consumer.
     */
    @Override void sendCancel() throws AMQException
    {
        ((AMQSession_0_10) getSession()).getQpidSession().messageCancel(getConsumerTagString());
        ((AMQSession_0_10) getSession()).getQpidSession().sync();
        // confirm cancel
        getSession().confirmConsumerCancelled(getConsumerTag());
        ((AMQSession_0_10) getSession()).getCurrentException();
    }

    @Override void notifyMessage(UnprocessedMessage_0_10 messageFrame)
    {

        super.notifyMessage(messageFrame);
    }

    @Override protected void preApplicationProcessing(AbstractJMSMessage jmsMsg) throws JMSException
    {
        super.preApplicationProcessing(jmsMsg);
        if (!_session.getTransacted() && _session.getAcknowledgeMode() != org.apache.qpid.jms.Session.CLIENT_ACKNOWLEDGE)
        {
            _session.addUnacknowledgedMessage(jmsMsg.getDeliveryTag());
        }
    }

    @Override public AbstractJMSMessage createJMSMessageFromUnprocessedMessage(
            AMQMessageDelegateFactory delegateFactory, UnprocessedMessage_0_10 msg) throws Exception
    {
        AMQMessageDelegate_0_10.updateExchangeTypeMapping(msg.getMessageTransfer().getHeader(), ((AMQSession_0_10)getSession()).getQpidSession());
        return _messageFactory.createMessage(msg.getMessageTransfer());
    }

    // private methods
    /**
     * Check whether a message can be delivered to this consumer.
     *
     * @param message The message to be checked.
     * @return true if the message matches the selector and can be acquired, false otherwise.
     * @throws AMQException If the message preConditions cannot be checked due to some internal error.
     */
    private boolean checkPreConditions(AbstractJMSMessage message) throws AMQException
    {
        boolean messageOk = true;
        // TODO Use a tag for fiding out if message filtering is done here or by the broker.
        try
        {
            if (_messageSelector != null && !_messageSelector.equals(""))
            {
                messageOk = _filter.matches(message);
            }
        }
        catch (Exception e)
        {
            throw new AMQException(AMQConstant.INTERNAL_ERROR, "Error when evaluating message selector", e);
        }

        if (_logger.isDebugEnabled())
        {
            _logger.debug("messageOk " + messageOk);
            _logger.debug("_preAcquire " + _preAcquire);
        }
        if (!messageOk)
        {
            if (_preAcquire)
            {
                // this is the case for topics
                // We need to ack this message
                if (_logger.isDebugEnabled())
                {
                    _logger.debug("filterMessage - trying to ack message");
                }
                acknowledgeMessage(message);
            }
            else
            {
                if (_logger.isDebugEnabled())
                {
                    _logger.debug("Message not OK, releasing");
                }
                releaseMessage(message);
            }
            // if we are syncrhonously waiting for a message
            // and messages are not prefetched we then need to request another one
            if(! getSession().prefetch())
            {
               _0_10session.getQpidSession().messageFlow(getConsumerTagString(),
                                                         MessageCreditUnit.MESSAGE, 1,
                                                         Option.UNRELIABLE);
            }
        }
        // now we need to acquire this message if needed
        // this is the case of queue with a message selector set
        if (!_preAcquire && messageOk && !isNoConsume())
        {
            if (_logger.isDebugEnabled())
            {
                _logger.debug("filterMessage - trying to acquire message");
            }
            messageOk = acquireMessage(message);
            _logger.debug("filterMessage - message acquire status : " + messageOk);
        }
        return messageOk;
    }


    /**
     * Acknowledge a message
     *
     * @param message The message to be acknowledged
     * @throws AMQException If the message cannot be acquired due to some internal error.
     */
    private void acknowledgeMessage(AbstractJMSMessage message) throws AMQException
    {
        if (!_preAcquire)
        {
            RangeSet ranges = new RangeSet();
            ranges.add((int) message.getDeliveryTag());
            _0_10session.messageAcknowledge
                (ranges,
                 _acknowledgeMode != org.apache.qpid.jms.Session.NO_ACKNOWLEDGE);
            _0_10session.getCurrentException();
        }
    }

    /**
     * Release a message
     *
     * @param message The message to be released
     * @throws AMQException If the message cannot be released due to some internal error.
     */
    private void releaseMessage(AbstractJMSMessage message) throws AMQException
    {
        if (_preAcquire)
        {
            RangeSet ranges = new RangeSet();
            ranges.add((int) message.getDeliveryTag());
            _0_10session.getQpidSession().messageRelease(ranges);
            _0_10session.getCurrentException();
        }
    }

    /**
     * Acquire a message
     *
     * @param message The message to be acquired
     * @return true if the message has been acquired, false otherwise.
     * @throws AMQException If the message cannot be acquired due to some internal error.
     */
    private boolean acquireMessage(AbstractJMSMessage message) throws AMQException
    {
        boolean result = false;
        if (!_preAcquire)
        {
            RangeSet ranges = new RangeSet();
            ranges.add((int) message.getDeliveryTag());

            Acquired acq = _0_10session.getQpidSession().messageAcquire(ranges).get();

            RangeSet acquired = acq.getTransfers();
            if (acquired != null && acquired.size() > 0)
            {
                result = true;
            }
        }
        return result;
    }


    public void setMessageListener(final MessageListener messageListener) throws JMSException
    {
        super.setMessageListener(messageListener);
        if (messageListener != null && ! getSession().prefetch())
        {
            _0_10session.getQpidSession().messageFlow(getConsumerTagString(),
                                                      MessageCreditUnit.MESSAGE, 1,
                                                      Option.UNRELIABLE);
        }
        if (messageListener != null && !_synchronousQueue.isEmpty())
        {
            Iterator messages=_synchronousQueue.iterator();
            while (messages.hasNext())
            {
                AbstractJMSMessage message=(AbstractJMSMessage) messages.next();
                messages.remove();
                _session.rejectMessage(message, true);
            }
        }
    }

    public void failedOverPost()
    {
        if (_0_10session.isStarted() && _syncReceive.get())
        {
            _0_10session.getQpidSession().messageFlow
                (getConsumerTagString(), MessageCreditUnit.MESSAGE, 1,
                 Option.UNRELIABLE);
        }
    }

    /**
     * When messages are not prefetched we need to request a message from the
     * broker.
     * Note that if the timeout is too short a message may be queued in _synchronousQueue until
     * this consumer closes or request it.
     * @param l
     * @return
     * @throws InterruptedException
     */
    public Object getMessageFromQueue(long l) throws InterruptedException
    {
        if (! getSession().prefetch())
        {
            _syncReceive.set(true);
        }
        if (_0_10session.isStarted() && ! getSession().prefetch() && _synchronousQueue.isEmpty())
        {
            _0_10session.getQpidSession().messageFlow(getConsumerTagString(),
                                                      MessageCreditUnit.MESSAGE, 1,
                                                      Option.UNRELIABLE);
        }
        Object o = super.getMessageFromQueue(l);
        if (o == null && _0_10session.isStarted())
        {
            _0_10session.getQpidSession().messageFlush
                (getConsumerTagString(), Option.UNRELIABLE, Option.SYNC);
            _0_10session.getQpidSession().sync();
            _0_10session.getQpidSession().messageFlow
                (getConsumerTagString(), MessageCreditUnit.BYTE,
                 0xFFFFFFFF, Option.UNRELIABLE);
            if (getSession().prefetch())
            {
                _0_10session.getQpidSession().messageFlow
                    (getConsumerTagString(), MessageCreditUnit.MESSAGE,
                     _0_10session.getAMQConnection().getMaxPrefetch(),
                     Option.UNRELIABLE);
            }
            _0_10session.syncDispatchQueue();
            o = super.getMessageFromQueue(-1);
        }
        if (! getSession().prefetch())
        {
            _syncReceive.set(false);
        }
        return o;
    }

    void postDeliver(AbstractJMSMessage msg) throws JMSException
    {
        super.postDeliver(msg);
        if (_acknowledgeMode == org.apache.qpid.jms.Session.NO_ACKNOWLEDGE && !_session.isInRecovery())
        {
          _session.acknowledgeMessage(msg.getDeliveryTag(), false);
        }
        
        if (_acknowledgeMode == org.apache.qpid.jms.Session.AUTO_ACKNOWLEDGE  &&
             !_session.isInRecovery() &&   
             _session.getAMQConnection().getSyncAck())
        {
            ((AMQSession_0_10) getSession()).flushAcknowledgments();
            ((AMQSession_0_10) getSession()).getQpidSession().sync();
        }
    }

    Message receiveBrowse() throws JMSException
    {
        return receiveNoWait();
    }

    @Override public void rollbackPendingMessages()
    {
        if (_synchronousQueue.size() > 0)
        {
            RangeSet ranges = new RangeSet();
            Iterator iterator = _synchronousQueue.iterator();
            while (iterator.hasNext())
            {

                Object o = iterator.next();
                if (o instanceof AbstractJMSMessage)
                {
                    ranges.add((int) ((AbstractJMSMessage) o).getDeliveryTag());
                    iterator.remove();
                }
                else
                {
                    _logger.error("Queue contained a :" + o.getClass()
                                  + " unable to reject as it is not an AbstractJMSMessage. Will be cleared");
                    iterator.remove();
                }
            }

            _0_10session.getQpidSession().messageRelease(ranges, Option.SET_REDELIVERED);
            clearReceiveQueue();
        }
    }
}
