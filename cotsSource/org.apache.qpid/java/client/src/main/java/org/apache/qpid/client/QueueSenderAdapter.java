/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */

package org.apache.qpid.client;

import javax.jms.Destination;
import javax.jms.InvalidDestinationException;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Queue;
import javax.jms.QueueSender;

public class QueueSenderAdapter implements QueueSender
{

    private BasicMessageProducer _delegate;
    private Queue _queue;
    private boolean closed = false;

    public QueueSenderAdapter(BasicMessageProducer msgProducer, Queue queue)
    {
        _delegate = msgProducer;
        _queue = queue;
    }

    public Queue getQueue() throws JMSException
    {
        checkPreConditions();

        return _queue;
    }

    public void send(Message msg) throws JMSException
    {
        checkPreConditions();
        _delegate.send(msg);
    }

    public void send(Queue queue, Message msg) throws JMSException
    {
        checkPreConditions(queue);
        _delegate.send(queue, msg);
    }

    public void publish(Message msg, int deliveryMode, int priority, long timeToLive) throws JMSException
    {
        checkPreConditions();
        _delegate.send(msg, deliveryMode, priority, timeToLive);
    }

    public void send(Queue queue, Message msg, int deliveryMode, int priority, long timeToLive) throws JMSException
    {
        checkPreConditions(queue);
        _delegate.send(queue, msg, deliveryMode, priority, timeToLive);
    }

    public void close() throws JMSException
    {
        _delegate.close();
        closed = true;
    }

    public int getDeliveryMode() throws JMSException
    {
        checkPreConditions();

        return _delegate.getDeliveryMode();
    }

    public Destination getDestination() throws JMSException
    {
        checkPreConditions();

        return _delegate.getDestination();
    }

    public boolean getDisableMessageID() throws JMSException
    {
        checkPreConditions();

        return _delegate.getDisableMessageID();
    }

    public boolean getDisableMessageTimestamp() throws JMSException
    {
        checkPreConditions();

        return _delegate.getDisableMessageTimestamp();
    }

    public int getPriority() throws JMSException
    {
        checkPreConditions();

        return _delegate.getPriority();
    }

    public long getTimeToLive() throws JMSException
    {
        checkPreConditions();

        return _delegate.getTimeToLive();
    }

    public void send(Destination dest, Message msg) throws JMSException
    {
        checkPreConditions((Queue) dest);
        _delegate.send(dest, msg);
    }

    public void send(Message msg, int deliveryMode, int priority, long timeToLive) throws JMSException
    {
        checkPreConditions();
        _delegate.send(msg, deliveryMode, priority, timeToLive);
    }

    public void send(Destination dest, Message msg, int deliveryMode, int priority, long timeToLive) throws JMSException
    {
        checkPreConditions((Queue) dest);
        _delegate.send(dest, msg, deliveryMode, priority, timeToLive);
    }

    public void setDeliveryMode(int deliveryMode) throws JMSException
    {
        checkPreConditions();
        _delegate.setDeliveryMode(deliveryMode);
    }

    public void setDisableMessageID(boolean disableMessageID) throws JMSException
    {
        checkPreConditions();
        _delegate.setDisableMessageID(disableMessageID);
    }

    public void setDisableMessageTimestamp(boolean disableMessageTimestamp) throws JMSException
    {
        checkPreConditions();
        _delegate.setDisableMessageTimestamp(disableMessageTimestamp);
    }

    public void setPriority(int priority) throws JMSException
    {
        checkPreConditions();
        _delegate.setPriority(priority);
    }

    public void setTimeToLive(long timeToLive) throws JMSException
    {
        checkPreConditions();
        _delegate.setTimeToLive(timeToLive);
    }

    private void checkPreConditions() throws JMSException
    {
        checkPreConditions(_queue);
    }

    private void checkPreConditions(Queue queue) throws JMSException
    {
        if (closed)
        {
            throw new javax.jms.IllegalStateException("Publisher is closed");
        }

        AMQSession session = ((BasicMessageProducer) _delegate).getSession();

        if ((session == null) || session.isClosed())
        {
            throw new javax.jms.IllegalStateException("Invalid Session");
        }

        if (queue == null)
        {
            throw new UnsupportedOperationException("Queue is null.");
        }

        if (!(queue instanceof AMQDestination))
        {
            throw new InvalidDestinationException("Queue: " + queue + " is not a valid Qpid queue");
        }

        AMQDestination destination = (AMQDestination) queue;
        if (!destination.isCheckedForQueueBinding() && checkQueueBeforePublish())
        {

            if (_delegate.getSession().isStrictAMQP())
            {
                _delegate._logger.warn("AMQP does not support destination validation before publish, ");
                destination.setCheckedForQueueBinding(true);
            }
            else
            {
                if (_delegate.isBound(destination))
                {
                    destination.setCheckedForQueueBinding(true);
                }
                else
                {
                    throw new InvalidDestinationException("Queue: " + queue
                        + " is not a valid destination (no bindings on server");
                }
            }
        }
    }

    private boolean checkQueueBeforePublish()
    {
        return "true".equalsIgnoreCase(System.getProperty("org.apache.qpid.client.verifyQueueBindingBeforePublish", "true"));
    }
}
