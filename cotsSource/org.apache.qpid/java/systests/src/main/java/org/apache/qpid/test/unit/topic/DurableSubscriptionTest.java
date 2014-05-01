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
package org.apache.qpid.test.unit.topic;

import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQTopic;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.Connection;
import javax.jms.InvalidDestinationException;
import javax.jms.InvalidSelectorException;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicSubscriber;

/**
 * @todo Code to check that a consumer gets only one particular method could be factored into a re-usable method (as
 *       a static on a base test helper class, e.g. TestUtils.
 *
 * @todo Code to create test end-points using session per connection, or all sessions on one connection, to be factored
 *       out to make creating this test variation simpler. Want to make this variation available through LocalCircuit,
 *       driven by the test model.
 */
public class DurableSubscriptionTest extends QpidTestCase
{
    private static final Logger _logger = LoggerFactory.getLogger(DurableSubscriptionTest.class);
    
    /** Timeout for receive() if we are expecting a message */
    private static final long POSITIVE_RECEIVE_TIMEOUT = 2000;
    
    /** Timeout for receive() if we are not expecting a message */
    private static final long NEGATIVE_RECEIVE_TIMEOUT = 1000;
    
    public void testUnsubscribe() throws Exception
    {
        AMQConnection con = (AMQConnection) getConnection("guest", "guest");
        AMQTopic topic = new AMQTopic(con, "MyDurableSubscriptionTestTopic");
        _logger.info("Create Session 1");
        Session session1 = con.createSession(false, AMQSession.NO_ACKNOWLEDGE);
        _logger.info("Create Consumer on Session 1");
        MessageConsumer consumer1 = session1.createConsumer(topic);
        _logger.info("Create Producer on Session 1");
        MessageProducer producer = session1.createProducer(topic);

        _logger.info("Create Session 2");
        Session session2 = con.createSession(false, AMQSession.NO_ACKNOWLEDGE);
        _logger.info("Create Durable Subscriber on Session 2");
        TopicSubscriber consumer2 = session2.createDurableSubscriber(topic, "MySubscription");

        _logger.info("Starting connection");
        con.start();

        _logger.info("Producer sending message A");
        producer.send(session1.createTextMessage("A"));

        Message msg;
        _logger.info("Receive message on consumer 1:expecting A");
        msg = consumer1.receive(POSITIVE_RECEIVE_TIMEOUT);
        assertNotNull("Message should have been received",msg);
        assertEquals("A", ((TextMessage) msg).getText());
        _logger.info("Receive message on consumer 1 :expecting null");
        msg = consumer1.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertEquals(null, msg);

        _logger.info("Receive message on consumer 2:expecting A");
        msg = consumer2.receive(POSITIVE_RECEIVE_TIMEOUT);
        assertNotNull("Message should have been received",msg);
        assertEquals("A", ((TextMessage) msg).getText());
        msg = consumer2.receive(NEGATIVE_RECEIVE_TIMEOUT);
        _logger.info("Receive message on consumer 1 :expecting null");
        assertEquals(null, msg);

        _logger.info("Unsubscribe session2/consumer2");
        session2.unsubscribe("MySubscription");

        _logger.info("Producer sending message B");
        producer.send(session1.createTextMessage("B"));

        _logger.info("Receive message on consumer 1 :expecting B");
        msg = consumer1.receive(POSITIVE_RECEIVE_TIMEOUT);
        assertNotNull("Message should have been received",msg);
        assertEquals("B", ((TextMessage) msg).getText());
        _logger.info("Receive message on consumer 1 :expecting null");
        msg = consumer1.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertEquals(null, msg);

        _logger.info("Receive message on consumer 2 :expecting null");
        msg = consumer2.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertEquals(null, msg);

        _logger.info("Close connection");
        con.close();
    }

    public void testDurabilityAUTOACK() throws Exception
    {
        durabilityImpl(Session.AUTO_ACKNOWLEDGE);
    }

    public void testDurabilityNOACKSessionPerConnection() throws Exception
    {
        durabilityImplSessionPerConnection(AMQSession.NO_ACKNOWLEDGE);
    }

    public void testDurabilityAUTOACKSessionPerConnection() throws Exception
    {
        durabilityImplSessionPerConnection(Session.AUTO_ACKNOWLEDGE);
    }

    private void durabilityImpl(int ackMode) throws Exception
    {
        AMQConnection con = (AMQConnection) getConnection("guest", "guest");
        AMQTopic topic = new AMQTopic(con, "MyTopic");
        Session session1 = con.createSession(false, ackMode);
        MessageConsumer consumer1 = session1.createConsumer(topic);

        Session sessionProd = con.createSession(false, AMQSession.NO_ACKNOWLEDGE);
        MessageProducer producer = sessionProd.createProducer(topic);

        Session session2 = con.createSession(false, AMQSession.NO_ACKNOWLEDGE);
        TopicSubscriber consumer2 = session2.createDurableSubscriber(topic, "MySubscription");

        con.start();

        producer.send(session1.createTextMessage("A"));

        Message msg;
        msg = consumer1.receive(POSITIVE_RECEIVE_TIMEOUT);
        assertNotNull("Message should have been received",msg);
        assertEquals("A", ((TextMessage) msg).getText());
        msg = consumer1.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertEquals(null, msg);

        msg = consumer2.receive(POSITIVE_RECEIVE_TIMEOUT);
        assertNotNull("Message should have been received",msg);
        assertEquals("A", ((TextMessage) msg).getText());
        msg = consumer2.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertEquals(null, msg);

        consumer2.close();

        producer.send(session1.createTextMessage("B"));

        _logger.info("Receive message on consumer 1 :expecting B");
        msg = consumer1.receive(500);
        assertNotNull("Consumer 1 should get message 'B'.", msg);
        assertEquals("Incorrect Message recevied on consumer1.", "B", ((TextMessage) msg).getText());
        _logger.info("Receive message on consumer 1 :expecting null");
        msg = consumer1.receive(500);
        assertNull("There should be no more messages for consumption on consumer1.", msg);

        Session session3 = con.createSession(false, ackMode);
        MessageConsumer consumer3 = session3.createDurableSubscriber(topic, "MySubscription");

        _logger.info("Receive message on consumer 3 :expecting B");
        msg = consumer3.receive(500);
        assertNotNull("Consumer 3 should get message 'B'.", msg);
        assertEquals("Incorrect Message recevied on consumer4.", "B", ((TextMessage) msg).getText());
        _logger.info("Receive message on consumer 3 :expecting null");
        msg = consumer3.receive(500);
        assertNull("There should be no more messages for consumption on consumer3.", msg);

        consumer1.close();
        consumer3.close();

        session3.unsubscribe("MySubscription");

        con.close();
    }

    private void durabilityImplSessionPerConnection(int ackMode) throws Exception
    {
        Message msg;
        // Create producer.
        AMQConnection con0 = (AMQConnection) getConnection("guest", "guest");
        con0.start();
        Session session0 = con0.createSession(false, ackMode);

        AMQTopic topic = new AMQTopic(con0, "MyTopic");

        Session sessionProd = con0.createSession(false, ackMode);
        MessageProducer producer = sessionProd.createProducer(topic);

        // Create consumer 1.
        AMQConnection con1 = (AMQConnection) getConnection("guest", "guest");
        con1.start();
        Session session1 = con1.createSession(false, ackMode);

        MessageConsumer consumer1 = session0.createConsumer(topic);

        // Create consumer 2.
        AMQConnection con2 = (AMQConnection) getConnection("guest", "guest");
        con2.start();
        Session session2 = con2.createSession(false, ackMode);

        TopicSubscriber consumer2 = session2.createDurableSubscriber(topic, "MySubscription");

        // Send message and check that both consumers get it and only it.
        producer.send(session0.createTextMessage("A"));

        msg = consumer1.receive(500);
        assertNotNull("Message should be available", msg);
        assertEquals("Message Text doesn't match", "A", ((TextMessage) msg).getText());
        msg = consumer1.receive(500);
        assertNull("There should be no more messages for consumption on consumer1.", msg);

        msg = consumer2.receive(POSITIVE_RECEIVE_TIMEOUT);
        assertNotNull("Message should have been received",msg);
        assertEquals("Consumer 2 should also received the first msg.", "A", ((TextMessage) msg).getText());
        msg = consumer2.receive(500);
        assertNull("There should be no more messages for consumption on consumer2.", msg);

        // Detach the durable subscriber.
        consumer2.close();
        session2.close();
        con2.close();

        // Send message and receive on open consumer.
        producer.send(session0.createTextMessage("B"));

        _logger.info("Receive message on consumer 1 :expecting B");
        msg = consumer1.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertEquals("B", ((TextMessage) msg).getText());
        _logger.info("Receive message on consumer 1 :expecting null");
        msg = consumer1.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertEquals(null, msg);

        // Re-attach a new consumer to the durable subscription, and check that it gets the message that it missed.
        AMQConnection con3 = (AMQConnection) getConnection("guest", "guest");
        con3.start();
        Session session3 = con3.createSession(false, ackMode);

        TopicSubscriber consumer3 = session3.createDurableSubscriber(topic, "MySubscription");

        _logger.info("Receive message on consumer 3 :expecting B");
        msg = consumer3.receive(500);
        assertNotNull("Consumer 3 should get message 'B'.", msg);
        assertEquals("Incorrect Message recevied on consumer4.", "B", ((TextMessage) msg).getText());
        _logger.info("Receive message on consumer 3 :expecting null");
        msg = consumer3.receive(500);
        assertNull("There should be no more messages for consumption on consumer3.", msg);

        consumer1.close();
        consumer3.close();

        session3.unsubscribe("MySubscription");

        con0.close();
        con1.close();
        con3.close();
    }

    /***
     * This tests the fix for QPID-1085
     * Creates a durable subscriber with an invalid selector, checks that the
     * exception is thrown correctly and that the subscription is not created. 
     * @throws Exception 
     */
    public void testDurableWithInvalidSelector() throws Exception
    {
    	Connection conn = getConnection();
    	conn.start();
    	Session session = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);
    	AMQTopic topic = new AMQTopic((AMQConnection) conn, "MyTestDurableWithInvalidSelectorTopic");
    	MessageProducer producer = session.createProducer(topic);
    	producer.send(session.createTextMessage("testDurableWithInvalidSelector1"));
    	try 
    	{
    		TopicSubscriber deadSubscriber = session.createDurableSubscriber(topic, "testDurableWithInvalidSelectorSub",
																	 		 "=TEST 'test", true);
    		assertNull("Subscriber should not have been created", deadSubscriber);
    	} 
    	catch (JMSException e)
    	{
    		assertTrue("Wrong type of exception thrown", e instanceof InvalidSelectorException);
    	}
    	
    	TopicSubscriber liveSubscriber = session.createDurableSubscriber(topic, "testDurableWithInvalidSelectorSub");
    	assertNotNull("Subscriber should have been created", liveSubscriber);

    	producer.send(session.createTextMessage("testDurableWithInvalidSelector2"));
    	
    	Message msg = liveSubscriber.receive(POSITIVE_RECEIVE_TIMEOUT);
    	assertNotNull ("Message should have been received", msg);
    	assertEquals ("testDurableWithInvalidSelector2", ((TextMessage) msg).getText());
    	assertNull("Should not receive subsequent message", liveSubscriber.receive(200));
        session.unsubscribe("testDurableWithInvalidSelectorSub");
    }
    
    /***
     * This tests the fix for QPID-1085
     * Creates a durable subscriber with an invalid destination, checks that the
     * exception is thrown correctly and that the subscription is not created. 
     * @throws Exception 
     */
    public void testDurableWithInvalidDestination() throws Exception
    {
    	Connection conn = getConnection();
    	conn.start();
    	Session session = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);
    	AMQTopic topic = new AMQTopic((AMQConnection) conn, "testDurableWithInvalidDestinationTopic");
    	try 
    	{
    		TopicSubscriber deadSubscriber = session.createDurableSubscriber(null, "testDurableWithInvalidDestinationsub");
    		assertNull("Subscriber should not have been created", deadSubscriber);
    	} 
    	catch (InvalidDestinationException e)
    	{
    		// This was expected
    	}
    	MessageProducer producer = session.createProducer(topic);    	
    	producer.send(session.createTextMessage("testDurableWithInvalidSelector1"));
    	
    	TopicSubscriber liveSubscriber = session.createDurableSubscriber(topic, "testDurableWithInvalidDestinationsub");
    	assertNotNull("Subscriber should have been created", liveSubscriber);
    	
    	producer.send(session.createTextMessage("testDurableWithInvalidSelector2"));
    	Message msg = liveSubscriber.receive(POSITIVE_RECEIVE_TIMEOUT);
    	assertNotNull ("Message should have been received", msg);
    	assertEquals ("testDurableWithInvalidSelector2", ((TextMessage) msg).getText());
    	assertNull("Should not receive subsequent message", liveSubscriber.receive(200));

        session.unsubscribe("testDurableWithInvalidDestinationsub");
    }
    
    /**
     * Tests QPID-1202
     * Creates a durable subscription with a selector, then changes that selector on resubscription
     * @throws Exception 
     */
    public void testResubscribeWithChangedSelector() throws Exception
    {
        Connection conn = getConnection();
        conn.start();
        Session session = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);
        AMQTopic topic = new AMQTopic((AMQConnection) conn, "testResubscribeWithChangedSelector");
        MessageProducer producer = session.createProducer(topic);
        
        // Create durable subscriber that matches A
        TopicSubscriber subA = session.createDurableSubscriber(topic, 
                "testResubscribeWithChangedSelector",
                "Match = True", false);

        // Send 1 matching message and 1 non-matching message
        sendMatchingAndNonMatchingMessage(session, producer);

        Message rMsg = subA.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertNotNull(rMsg);
        assertEquals("Content was wrong", 
                     "testResubscribeWithChangedSelector1",
                     ((TextMessage) rMsg).getText());
        
        rMsg = subA.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertNull(rMsg);
        
        // Disconnect subscriber
        subA.close();
        
        // Reconnect with new selector that matches B
        TopicSubscriber subB = session.createDurableSubscriber(topic, 
                "testResubscribeWithChangedSelector","Match = False", false);
        
        
        // Check messages are recieved properly
        sendMatchingAndNonMatchingMessage(session, producer);
        rMsg = subB.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertNotNull(rMsg);
        assertEquals("Content was wrong", 
                     "testResubscribeWithChangedSelector2",
                     ((TextMessage) rMsg).getText());
        
        rMsg = subB.receive(NEGATIVE_RECEIVE_TIMEOUT);
        assertNull(rMsg);
        session.unsubscribe("testResubscribeWithChangedSelector");
    }

    public void testDurableSubscribeWithTemporaryTopic() throws Exception
    {
        Connection conn = getConnection();
        conn.start();
        Session ssn = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);
        Topic topic = ssn.createTemporaryTopic();
        try
        {
            ssn.createDurableSubscriber(topic, "test");
            fail("expected InvalidDestinationException");
        }
        catch (InvalidDestinationException ex)
        {
            // this is expected
        }
        try
        {
            ssn.createDurableSubscriber(topic, "test", null, false);
            fail("expected InvalidDestinationException");
        }
        catch (InvalidDestinationException ex)
        {
            // this is expected
        }
    }

    private void sendMatchingAndNonMatchingMessage(Session session, MessageProducer producer) throws JMSException
    {
        TextMessage msg = session.createTextMessage("testResubscribeWithChangedSelector1");
        msg.setBooleanProperty("Match", true);
        producer.send(msg);
        msg = session.createTextMessage("testResubscribeWithChangedSelector2");
        msg.setBooleanProperty("Match", false);
        producer.send(msg);
    }
    
    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(DurableSubscriptionTest.class);
    }  
}
