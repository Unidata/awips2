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
package org.apache.qpid.cluster;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.url.URLSyntaxException;

import javax.jms.MessageListener;
import javax.jms.Message;
import javax.jms.Session;
import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.TextMessage;
import java.util.Random;

public class Client
{
    private final Random random = new Random();
    private final String name;
    private final Session session;
    private final MessageProducer topicProducer;
    private final MessageProducer queueProducer;

    Client(AMQConnection connection, String name) throws JMSException, InterruptedException
    {
        this.name = name;
        session = connection.createSession(false, AMQSession.NO_ACKNOWLEDGE);

        AMQTopic topic = new AMQTopic(((AMQSession)session).getDefaultTopicExchangeName(), new AMQShortString("cluster_test_topic"));
        AMQQueue queue = new AMQQueue(((AMQSession)session).getDefaultQueueExchangeName(), new AMQShortString("cluster_test_queue"));

        topicProducer = session.createProducer(topic);
        queueProducer = session.createProducer(queue);

        //subscribe to a known topic
        session.createConsumer(topic).setMessageListener(new TopicHandler());
        //subscribe to a known queue
        session.createConsumer(queue).setMessageListener(new QueueHandler());

        connection.start();

        while(true)
        {
            Thread.sleep(random.nextInt(60000));
            sendToQueue(name + ":" + randomString(5));
        }
    }

    private synchronized void sendToTopic(String message) throws JMSException
    {
        topicProducer.send(session.createTextMessage(message));
    }

    private synchronized void sendToQueue(String message) throws JMSException
    {
        queueProducer.send(session.createTextMessage(message));
    }

    private String randomString(int length){
        char[] c = new char[length];
        for(int i = 0; i < length; i++)
        {
            c[i] = (char) ('A' + random.nextInt(26));
        }
        return new String(c);
    }

    private class QueueHandler implements MessageListener
    {
        public void onMessage(Message message)
        {
            try
            {
                sendToTopic(((TextMessage) message).getText());
            }
            catch (JMSException e)
            {
                e.printStackTrace();
            }
        }
    }

    private class TopicHandler implements MessageListener
    {
        public void onMessage(Message message)
        {
            try
            {
                System.out.println(((TextMessage) message).getText());
            }
            catch (JMSException e)
            {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] argv) throws AMQException, JMSException, InterruptedException, URLSyntaxException
    {
        //assume args describe the set of brokers to try

        String clientName = argv.length > 1 ? argv[1] : "testClient";
        new Client(new AMQConnection(argv.length > 0 ? argv[0] : "vm://:1", "guest", "guest", clientName, "/test"), clientName);
    }
}
