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

import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.IllegalStateException;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.ObjectMessage;
import javax.jms.Queue;
import javax.jms.QueueBrowser;
import javax.jms.Session;
import javax.jms.StreamMessage;
import javax.jms.TemporaryQueue;
import javax.jms.TemporaryTopic;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;

public class AMQTopicSessionAdaptor implements TopicSession, AMQSessionAdapter
{
    protected final AMQSession _session;

    public AMQTopicSessionAdaptor(Session session)
    {
        _session = (AMQSession) session;
    }

    public Topic createTopic(String string) throws JMSException
    {
        return _session.createTopic(string);
    }

    public TopicSubscriber createSubscriber(Topic topic) throws JMSException
    {
        return _session.createSubscriber(topic);
    }

    public TopicSubscriber createSubscriber(Topic topic, String string, boolean b) throws JMSException
    {
        return _session.createSubscriber(topic, string, b);
    }

    public TopicSubscriber createDurableSubscriber(Topic topic, String string) throws JMSException
    {
        return _session.createDurableSubscriber(topic, string);
    }

    public TopicSubscriber createDurableSubscriber(Topic topic, String string, String string1, boolean b) throws JMSException
    {
        return _session.createDurableSubscriber(topic, string, string1, b);
    }

    public TopicPublisher createPublisher(Topic topic) throws JMSException
    {
        return _session.createPublisher(topic);
    }

    public TemporaryTopic createTemporaryTopic() throws JMSException
    {
        return _session.createTemporaryTopic();
    }

    public void unsubscribe(String string) throws JMSException
    {
        _session.unsubscribe(string);
    }

    public BytesMessage createBytesMessage() throws JMSException
    {
        return _session.createBytesMessage();
    }

    public MapMessage createMapMessage() throws JMSException
    {
        return _session.createMapMessage();
    }

    public Message createMessage() throws JMSException
    {
        return _session.createMessage();
    }

    public ObjectMessage createObjectMessage() throws JMSException
    {
        return _session.createObjectMessage();
    }

    public ObjectMessage createObjectMessage(Serializable serializable) throws JMSException
    {
        return _session.createObjectMessage(serializable);
    }

    public StreamMessage createStreamMessage() throws JMSException
    {
        return _session.createStreamMessage();
    }

    public TextMessage createTextMessage() throws JMSException
    {
        return _session.createTextMessage();
    }

    public TextMessage createTextMessage(String string) throws JMSException
    {
        return _session.createTextMessage(string);
    }

    public boolean getTransacted() throws JMSException
    {
        return _session.getTransacted();
    }

    public int getAcknowledgeMode() throws JMSException
    {
        return _session.getAcknowledgeMode();
    }

    public void commit() throws JMSException
    {
        _session.commit();
    }

    public void rollback() throws JMSException
    {
        _session.rollback();
    }

    public void close() throws JMSException
    {
        _session.close();
    }

    public void recover() throws JMSException
    {
        _session.recover();
    }

    public MessageListener getMessageListener() throws JMSException
    {
        return _session.getMessageListener();
    }

    public void setMessageListener(MessageListener messageListener) throws JMSException
    {
        _session.setMessageListener(messageListener);
    }

    public void run()
    {
        _session.run();
    }

    public MessageProducer createProducer(Destination destination) throws JMSException
    {
        return _session.createProducer(destination);
    }

    public MessageConsumer createConsumer(Destination destination) throws JMSException
    {
        return _session.createConsumer(destination);
    }

    public MessageConsumer createConsumer(Destination destination, String string) throws JMSException
    {
        return _session.createConsumer(destination, string);
    }

    public MessageConsumer createConsumer(Destination destination, String string, boolean b) throws JMSException
    {
        return _session.createConsumer(destination, string, b);
    }

    //The following methods cannot be called from a TopicSession as per JMS spec
    public Queue createQueue(String string) throws JMSException
    {
        throw new IllegalStateException("Cannot call createQueue from TopicSession");
    }

    public QueueBrowser createBrowser(Queue queue) throws JMSException
    {
        throw new IllegalStateException("Cannot call createBrowser from TopicSession");
    }

    public QueueBrowser createBrowser(Queue queue, String string) throws JMSException
    {
        throw new IllegalStateException("Cannot call createBrowser from TopicSession");
    }

    public TemporaryQueue createTemporaryQueue() throws JMSException
    {
        throw new IllegalStateException("Cannot call createTemporaryQueue from TopicSession");
    }

    public AMQSession getSession()
    {
        return _session;
    }
}
