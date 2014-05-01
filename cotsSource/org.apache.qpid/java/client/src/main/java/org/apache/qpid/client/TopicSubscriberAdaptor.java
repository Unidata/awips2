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

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Topic;
import javax.jms.TopicSubscriber;

/**
 * Wraps a MessageConsumer to fulfill the extended TopicSubscriber contract
 *
 */
class TopicSubscriberAdaptor<C extends BasicMessageConsumer> implements TopicSubscriber
{
    private final Topic _topic;
    private final C _consumer;
    private final boolean _noLocal;

    TopicSubscriberAdaptor(Topic topic, C consumer, boolean noLocal)
    {
        _topic = topic;
        _consumer = consumer;
        _noLocal = noLocal;
    }
    
    TopicSubscriberAdaptor(Topic topic, C consumer)
    {
        this(topic, consumer, consumer.isNoLocal());
    }
    
    public Topic getTopic() throws JMSException
    {
    	checkPreConditions();
        return _topic;
    }

    public boolean getNoLocal() throws JMSException
    {
    	checkPreConditions();
        return _noLocal;
    }

    public String getMessageSelector() throws JMSException
    {
    	checkPreConditions();
        return _consumer.getMessageSelector();
    }

    public MessageListener getMessageListener() throws JMSException
    {
    	checkPreConditions();
        return _consumer.getMessageListener();
    }

    public void setMessageListener(MessageListener messageListener) throws JMSException
    {
    	checkPreConditions();
        _consumer.setMessageListener(messageListener);
    }

    public Message receive() throws JMSException
    {
    	checkPreConditions();
        return _consumer.receive();
    }

    public Message receive(long l) throws JMSException
    {
        return _consumer.receive(l);
    }

    public Message receiveNoWait() throws JMSException
    {
    	checkPreConditions();
        return _consumer.receiveNoWait();
    }

    public void close() throws JMSException
    {
        _consumer.close();
    }
    
    private void checkPreConditions() throws javax.jms.IllegalStateException{
    	C msgConsumer = _consumer;
    	
    	if (msgConsumer.isClosed() ){
			throw new javax.jms.IllegalStateException("Consumer is closed");
		}
		
		if(_topic == null){
			throw new UnsupportedOperationException("Topic is null");
		}
		
		AMQSession session = msgConsumer.getSession();
		
		if(session == null || session.isClosed()){
			throw new javax.jms.IllegalStateException("Invalid Session");
		}
	}

    C getMessageConsumer()
    {
        return _consumer;
    }

    public void addBindingKey(Topic topic, String bindingKey) throws AMQException
    {
        _consumer.addBindingKey((AMQDestination) topic, bindingKey);
    }
}
