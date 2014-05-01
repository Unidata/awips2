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
import javax.jms.IllegalStateException;
import javax.jms.InvalidDestinationException;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Topic;
import javax.jms.TopicPublisher;

public class TopicPublisherAdapter implements TopicPublisher
{

    private BasicMessageProducer _delegate;
    private Topic _topic;

    public TopicPublisherAdapter(BasicMessageProducer msgProducer, Topic topic)
    {
        _delegate = msgProducer;
        _topic = topic;
    }

    public Topic getTopic() throws JMSException
    {
        checkPreConditions();
        return _topic;
    }

    public void publish(Message msg) throws JMSException
    {
        checkPreConditions();
        checkTopic(_topic);
        _delegate.send(msg);
    }

    public void publish(Topic topic, Message msg) throws JMSException
    {
        checkPreConditions();
        checkTopic(topic);
        _delegate.send(topic, msg);
    }

    public void publish(Message msg, int deliveryMode, int priority, long timeToLive)
            throws JMSException
    {
        checkPreConditions();
        checkTopic(_topic);
        _delegate.send(msg, deliveryMode, priority, timeToLive);
    }

	public int getDeliveryMode() throws JMSException {
		checkPreConditions();
		return _delegate.getDeliveryMode();
	}

	public void publish(Topic topic, Message msg, int deliveryMode, int priority, long timeToLive)
            throws JMSException
    {
        checkPreConditions();
        checkTopic(topic);
        _delegate.send(topic, msg, deliveryMode, priority, timeToLive);
    }

	public void close() throws JMSException
    {
        _delegate.close();
    }

	public boolean getDisableMessageID() throws JMSException {
		checkPreConditions();
		return _delegate.getDisableMessageID();
	}

	public boolean getDisableMessageTimestamp() throws JMSException {
		checkPreConditions();
		return _delegate.getDisableMessageTimestamp();
	}
	
    public Destination getDestination() throws JMSException
    {
		checkPreConditions();
        return _delegate.getDestination();
    }

    public int getPriority() throws JMSException {
		checkPreConditions();
		return _delegate.getPriority();
	}

    public long getTimeToLive() throws JMSException {
		checkPreConditions();
		return _delegate.getTimeToLive();
	}
    
    public void send(Message msg) throws JMSException
    {
        checkPreConditions();
        checkTopic(_topic);
        _delegate.send(msg);
    }

    public void send(Destination dest, Message msg) throws JMSException
    {
        checkPreConditions();
        checkTopic(_topic);
        _delegate.send(dest, msg);
    }

    public void send(Message msg, int deliveryMode, int priority, long timeToLive)
            throws JMSException
    {
        checkPreConditions();
        checkTopic(_topic);
        _delegate.send(msg, deliveryMode, priority, timeToLive);
    }

    public void send(Destination dest, Message msg, int deliveryMode, int priority, long timeToLive) throws JMSException
    {
        checkPreConditions();
        checkTopic(dest);
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

    private void checkPreConditions() throws IllegalStateException
    {
        if (_delegate.isClosed())
        {
            throw new javax.jms.IllegalStateException("Publisher is _closed");
        }

        AMQSession session = _delegate.getSession();
        if (session == null || session.isClosed())
        {
            throw new javax.jms.IllegalStateException("Invalid Session");
        }
    }

    private void checkTopic(Destination topic) throws InvalidDestinationException
    {
        if (topic == null)
        {
            throw new UnsupportedOperationException("Topic is null");
        }
        if (!(topic instanceof Topic))
        {
            throw new InvalidDestinationException("Destination " + topic + " is not a topic");
        }
        if(!(topic instanceof AMQDestination))
        {
            throw new InvalidDestinationException("Destination " + topic + " is not a Qpid topic");
        }

    }
}
