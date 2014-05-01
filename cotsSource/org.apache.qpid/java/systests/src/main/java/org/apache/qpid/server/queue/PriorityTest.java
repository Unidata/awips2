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
package org.apache.qpid.server.queue;

import junit.framework.TestCase;
import junit.framework.Assert;
import org.apache.log4j.Logger;
import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.jndi.PropertiesFileInitialContextFactory;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.url.URLSyntaxException;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;

import javax.jms.*;
import javax.naming.NamingException;
import javax.naming.Context;
import javax.naming.spi.InitialContextFactory;
import java.util.Hashtable;
import java.util.HashMap;
import java.util.Map;

public class PriorityTest extends QpidTestCase
{
    private static final int TIMEOUT = 1500;


    private static final Logger _logger = Logger.getLogger(PriorityTest.class);

    protected final String QUEUE = "PriorityQueue";

    private static final int MSG_COUNT = 50;

    private Connection producerConnection;
    private MessageProducer producer;
    private Session producerSession;
    private Queue queue;
    private Connection consumerConnection;
    private Session consumerSession;


    private MessageConsumer consumer;
    
    protected void setUp() throws Exception
    {
        super.setUp();

        producerConnection = getConnection();
        producerSession = producerConnection.createSession(true, Session.AUTO_ACKNOWLEDGE);

        producerConnection.start();
        
        consumerConnection = getConnection();
        consumerSession = consumerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        
    }

    protected void tearDown() throws Exception
    {
        producerConnection.close();
        consumerConnection.close();
        super.tearDown();
    }

    public void testPriority() throws JMSException, NamingException, AMQException
    {
        final Map<String,Object> arguments = new HashMap<String, Object>();
        arguments.put("x-qpid-priorities",10);
        ((AMQSession) producerSession).createQueue(new AMQShortString(QUEUE), true, false, false, arguments);
        queue = (Queue) producerSession.createQueue("direct://amq.direct/"+QUEUE+"/"+QUEUE+"?durable='false'&autodelete='true'");

        ((AMQSession) producerSession).declareAndBind((AMQDestination)queue);
        producer = producerSession.createProducer(queue);

        for (int msg = 0; msg < MSG_COUNT; msg++)
        {
            producer.setPriority(msg % 10);
            producer.send(nextMessage(msg, false, producerSession, producer));
        }
        producerSession.commit();
        producer.close();
        producerSession.close();
        producerConnection.close();

        consumer = consumerSession.createConsumer(queue);
        consumerConnection.start();
        Message received;
        int receivedCount = 0;
        Message previous = null;
        int messageCount = 0;
        while((received = consumer.receive(1000))!=null)
        {   
            messageCount++;
            if(previous != null)
            {
                assertTrue("Messages arrived in unexpected order " + messageCount + " " + previous.getIntProperty("msg") + " " + received.getIntProperty("msg") + " " + previous.getJMSPriority() + " " + received.getJMSPriority(), (previous.getJMSPriority() > received.getJMSPriority()) || ((previous.getJMSPriority() == received.getJMSPriority()) && previous.getIntProperty("msg") < received.getIntProperty("msg")) );
            }

            previous = received;
            receivedCount++;
        }

        assertEquals("Incorrect number of message received", 50, receivedCount);
    }
    
    public void testOddOrdering() throws AMQException, JMSException
    {
        final Map<String,Object> arguments = new HashMap<String, Object>();
        arguments.put("x-qpid-priorities",3);
        ((AMQSession) producerSession).createQueue(new AMQShortString(QUEUE), true, false, false, arguments);
        queue = producerSession.createQueue("direct://amq.direct/"+QUEUE+"/"+QUEUE+"?durable='false'&autodelete='true'");
        
        ((AMQSession) producerSession).declareAndBind((AMQDestination)queue);
        producer = producerSession.createProducer(queue);
        
        // In order ABC
        producer.setPriority(9);
        producer.send(nextMessage(1, false, producerSession, producer));
        producer.setPriority(4);
        producer.send(nextMessage(2, false, producerSession, producer));
        producer.setPriority(1);
        producer.send(nextMessage(3, false, producerSession, producer));

        // Out of order BAC
        producer.setPriority(4);
        producer.send(nextMessage(4, false, producerSession, producer));
        producer.setPriority(9);
        producer.send(nextMessage(5, false, producerSession, producer));
        producer.setPriority(1);
        producer.send(nextMessage(6, false, producerSession, producer));

        // Out of order BCA 
        producer.setPriority(4);
        producer.send(nextMessage(7, false, producerSession, producer));
        producer.setPriority(1);
        producer.send(nextMessage(8, false, producerSession, producer));
        producer.setPriority(9);
        producer.send(nextMessage(9, false, producerSession, producer));
        
        // Reverse order CBA
        producer.setPriority(1);
        producer.send(nextMessage(10, false, producerSession, producer));
        producer.setPriority(4);
        producer.send(nextMessage(11, false, producerSession, producer));
        producer.setPriority(9);
        producer.send(nextMessage(12, false, producerSession, producer));
        producerSession.commit();
        
        consumer = consumerSession.createConsumer(queue);
        consumerConnection.start();
        
        Message msg = consumer.receive(TIMEOUT);
        assertEquals(1, msg.getIntProperty("msg"));
        msg = consumer.receive(TIMEOUT);
        assertEquals(5, msg.getIntProperty("msg"));
        msg = consumer.receive(TIMEOUT);
        assertEquals(9, msg.getIntProperty("msg"));
        msg = consumer.receive(TIMEOUT);
        assertEquals(12, msg.getIntProperty("msg"));
        
        msg = consumer.receive(TIMEOUT);
        assertEquals(2, msg.getIntProperty("msg"));
        msg = consumer.receive(TIMEOUT);
        assertEquals(4, msg.getIntProperty("msg"));
        msg = consumer.receive(TIMEOUT);
        assertEquals(7, msg.getIntProperty("msg"));
        msg = consumer.receive(TIMEOUT);
        assertEquals(11, msg.getIntProperty("msg"));
        
        msg = consumer.receive(TIMEOUT);
        assertEquals(3, msg.getIntProperty("msg"));
        msg = consumer.receive(TIMEOUT);
        assertEquals(6, msg.getIntProperty("msg"));
        msg = consumer.receive(TIMEOUT);
        assertEquals(8, msg.getIntProperty("msg"));
        msg = consumer.receive(TIMEOUT);
        assertEquals(10, msg.getIntProperty("msg"));
    }

    private Message nextMessage(int msg, boolean first, Session producerSession, MessageProducer producer) throws JMSException
    {
        Message send = producerSession.createTextMessage("Message: " + msg);
        send.setIntProperty("msg", msg);

        return send;
    }


}
