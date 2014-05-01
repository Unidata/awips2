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

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.Queue;
import javax.jms.QueueReceiver;

/**
 * Class that wraps a MessageConsumer for backwards JMS compatibility
 * Returned by methods in AMQSession etc
 */
public class QueueReceiverAdaptor implements QueueReceiver {

    protected MessageConsumer _consumer;
    protected Queue _queue;

    protected QueueReceiverAdaptor(Queue queue, MessageConsumer consumer)
    {
        _consumer = consumer;
        _queue = queue;
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
    	checkPreConditions();
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

    /**
     * Return the queue associated with this receiver
     * @return
     * @throws JMSException
     */
    public Queue getQueue() throws JMSException
    {
    	checkPreConditions();
       return _queue;
    }

    private void checkPreConditions() throws javax.jms.IllegalStateException {
    	BasicMessageConsumer msgConsumer = (BasicMessageConsumer)_consumer;
    	
    	if (msgConsumer.isClosed() ){
			throw new javax.jms.IllegalStateException("Consumer is closed");
		}
		
		if(_queue == null){
			throw new UnsupportedOperationException("Queue is null");
		}
		
		AMQSession session = msgConsumer.getSession();
		
		if(session == null || session.isClosed()){
			throw new javax.jms.IllegalStateException("Invalid Session");
		}
	}

}
