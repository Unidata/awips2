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
package org.apache.qpid.test.unit.client;

import javax.jms.JMSException;
import javax.jms.QueueReceiver;
import javax.jms.TopicSubscriber;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.test.utils.QpidTestCase;

/**
 * Tests for QueueReceiver and TopicSubscriber creation methods on AMQSession
 */
public class AMQSessionTest extends QpidTestCase
{

    private static AMQSession _session;
    private static AMQTopic _topic;
    private static AMQQueue _queue;
    private static AMQConnection _connection;

    protected void setUp() throws Exception
    {
        super.setUp();
        _connection = (AMQConnection) getConnection("guest", "guest");
        _topic = new AMQTopic(_connection,"mytopic");
        _queue = new AMQQueue(_connection,"myqueue");
        _session = (AMQSession) _connection.createSession(false, AMQSession.NO_ACKNOWLEDGE);
    }

    protected void tearDown() throws Exception
    {
        try
        {
            _connection.close();
        }
        catch (JMSException e)
        {
            //just close
        }
        super.tearDown();
    }

    public void testCreateSubscriber() throws JMSException
    {
        TopicSubscriber subscriber = _session.createSubscriber(_topic);
        assertEquals("Topic names should match from TopicSubscriber", _topic.getTopicName(), subscriber.getTopic().getTopicName());

        subscriber = _session.createSubscriber(_topic, "abc", false);
        assertEquals("Topic names should match from TopicSubscriber with selector", _topic.getTopicName(), subscriber.getTopic().getTopicName());
    }

    public void testCreateDurableSubscriber() throws JMSException
    {
       TopicSubscriber subscriber = _session.createDurableSubscriber(_topic, "mysubname");
        assertEquals("Topic names should match from durable TopicSubscriber", _topic.getTopicName(), subscriber.getTopic().getTopicName());

        subscriber = _session.createDurableSubscriber(_topic, "mysubname2", "abc", false);
        assertEquals("Topic names should match from durable TopicSubscriber with selector", _topic.getTopicName(), subscriber.getTopic().getTopicName());
        _session.unsubscribe("mysubname");
        _session.unsubscribe("mysubname2");
    }

    public void testCreateQueueReceiver() throws JMSException
    {
        QueueReceiver receiver = _session.createQueueReceiver(_queue);
        assertEquals("Queue names should match from QueueReceiver", _queue.getQueueName(), receiver.getQueue().getQueueName());

        receiver = _session.createQueueReceiver(_queue, "abc");
        assertEquals("Queue names should match from QueueReceiver with selector", _queue.getQueueName(), receiver.getQueue().getQueueName());
    }

    public void testCreateReceiver() throws JMSException
    {
        QueueReceiver receiver = _session.createReceiver(_queue);
        assertEquals("Queue names should match from QueueReceiver", _queue.getQueueName(), receiver.getQueue().getQueueName());

        receiver = _session.createReceiver(_queue, "abc");
        assertEquals("Queue names should match from QueueReceiver with selector", _queue.getQueueName(), receiver.getQueue().getQueueName());
    }

    public static void stopVmBrokers()
    {
        _queue = null;
        _topic = null;
        _session = null;
    }
}
