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

import javax.jms.InvalidDestinationException;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;
import javax.jms.Session;
import javax.jms.TemporaryTopic;
import javax.jms.TextMessage;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.client.AMQTopicSessionAdaptor;
import org.apache.qpid.test.utils.QpidTestCase;


/** @author Apache Software Foundation */
public class TopicSessionTest extends QpidTestCase
{

    protected void setUp() throws Exception
    {
        super.setUp();
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }


    public void testTopicSubscriptionUnsubscription() throws Exception
    {

        AMQConnection con = (AMQConnection) getConnection("guest", "guest");
        AMQTopic topic = new AMQTopic(con.getDefaultTopicExchangeName(), "MyTopic");
        TopicSession session1 = con.createTopicSession(true, AMQSession.NO_ACKNOWLEDGE);
        TopicSubscriber sub = session1.createDurableSubscriber(topic, "subscription0");
        TopicPublisher publisher = session1.createPublisher(topic);

        con.start();

        TextMessage tm = session1.createTextMessage("Hello");
        publisher.publish(tm);
        session1.commit();

        tm = (TextMessage) sub.receive(2000);
        assertNotNull(tm);
        session1.commit();
        session1.unsubscribe("subscription0");

        try
        {
            session1.unsubscribe("not a subscription");
            fail("expected InvalidDestinationException when unsubscribing from unknown subscription");
        }
        catch (InvalidDestinationException e)
        {
            ; // PASS
        }
        catch (Exception e)
        {
            fail("expected InvalidDestinationException when unsubscribing from unknown subscription, got: " + e);
        }

        con.close();
    }

    public void testSubscriptionNameReuseForDifferentTopicSingleConnection() throws Exception
    {
        subscriptionNameReuseForDifferentTopic(false);
    }

    public void testSubscriptionNameReuseForDifferentTopicTwoConnections() throws Exception
    {
        subscriptionNameReuseForDifferentTopic(true);
    }

    private void subscriptionNameReuseForDifferentTopic(boolean shutdown) throws Exception
    {
        AMQConnection con = (AMQConnection) getConnection("guest", "guest");
        AMQTopic topic = new AMQTopic(con, "MyTopic1" + String.valueOf(shutdown));
        AMQTopic topic2 = new AMQTopic(con, "MyOtherTopic1" + String.valueOf(shutdown));

        TopicSession session1 = con.createTopicSession(true, AMQSession.AUTO_ACKNOWLEDGE);
        TopicSubscriber sub = session1.createDurableSubscriber(topic, "subscription0");
        TopicPublisher publisher = session1.createPublisher(null);

        con.start();

        publisher.publish(topic, session1.createTextMessage("hello"));
        session1.commit();
        TextMessage m = (TextMessage) sub.receive(2000);
        assertNotNull(m);
        session1.commit();

        if (shutdown)
        {
            session1.close();
            con.close();
            con =  (AMQConnection) getConnection("guest", "guest");
            con.start();
            session1 = con.createTopicSession(true, AMQSession.NO_ACKNOWLEDGE);
            publisher = session1.createPublisher(null);
        }
        TopicSubscriber sub2 = session1.createDurableSubscriber(topic2, "subscription0");
        publisher.publish(topic, session1.createTextMessage("hello"));
        session1.commit();
        if (!shutdown)
        {
            m = (TextMessage) sub.receive(2000);
            assertNull(m);
            session1.commit();
        }
        publisher.publish(topic2, session1.createTextMessage("goodbye"));
        session1.commit();
        m = (TextMessage) sub2.receive(2000);
        assertNotNull(m);
        assertEquals("goodbye", m.getText());
        session1.unsubscribe("subscription0");
        con.close();
    }

    public void testUnsubscriptionAfterConnectionClose() throws Exception
    {
        AMQConnection con1 = (AMQConnection) getConnection("guest", "guest", "clientid");
        AMQTopic topic = new AMQTopic(con1, "MyTopic3");

        TopicSession session1 = con1.createTopicSession(true, AMQSession.AUTO_ACKNOWLEDGE);
        TopicPublisher publisher = session1.createPublisher(topic);

        AMQConnection con2 = (AMQConnection) getConnection("guest", "guest", "clientid");
        TopicSession session2 = con2.createTopicSession(true, AMQSession.AUTO_ACKNOWLEDGE);
        TopicSubscriber sub = session2.createDurableSubscriber(topic, "subscription0");

        con2.start();

        publisher.publish(session1.createTextMessage("Hello"));
        session1.commit();
        TextMessage tm = (TextMessage) sub.receive(2000);
        session2.commit();
        assertNotNull(tm);
        con2.close();
        publisher.publish(session1.createTextMessage("Hello2"));
        session1.commit();
        con2 =  (AMQConnection) getConnection("guest", "guest", "clientid");
        session2 = con2.createTopicSession(true, AMQSession.NO_ACKNOWLEDGE);
        sub = session2.createDurableSubscriber(topic, "subscription0");
        con2.start();
        tm = (TextMessage) sub.receive(2000);
        session2.commit();
        assertNotNull(tm);
        assertEquals("Hello2", tm.getText());
        session2.unsubscribe("subscription0");
        con1.close();
        con2.close();
    }

    public void testTextMessageCreation() throws Exception
    {

        AMQConnection con = (AMQConnection) getConnection("guest", "guest");
        AMQTopic topic = new AMQTopic(con, "MyTopic4");
        TopicSession session1 = con.createTopicSession(true, AMQSession.AUTO_ACKNOWLEDGE);
        TopicPublisher publisher = session1.createPublisher(topic);
        MessageConsumer consumer1 = session1.createConsumer(topic);
        con.start();
        TextMessage tm = session1.createTextMessage("Hello");
        publisher.publish(tm);
        session1.commit();
        tm = (TextMessage) consumer1.receive(10000L);
        assertNotNull(tm);
        String msgText = tm.getText();
        assertEquals("Hello", msgText);
        tm = session1.createTextMessage();
        msgText = tm.getText();
        assertNull(msgText);
        publisher.publish(tm);
        session1.commit();
        tm = (TextMessage) consumer1.receive(10000L);
        assertNotNull(tm);
        session1.commit();
        msgText = tm.getText();
        assertNull(msgText);
        tm.clearBody();
        tm.setText("Now we are not null");
        publisher.publish(tm);
        session1.commit();
        tm = (TextMessage) consumer1.receive(2000);
        assertNotNull(tm);
        session1.commit();
        msgText = tm.getText();
        assertEquals("Now we are not null", msgText);

        tm = session1.createTextMessage("");
        msgText = tm.getText();
        assertEquals("Empty string not returned", "", msgText);
        publisher.publish(tm);
        session1.commit();
        tm = (TextMessage) consumer1.receive(2000);
        session1.commit();
        assertNotNull(tm);
        assertEquals("Empty string not returned", "", msgText);
        con.close();
    }

    public void testSendingSameMessage() throws Exception
    {
        AMQConnection conn = (AMQConnection) getConnection("guest", "guest");
        TopicSession session = conn.createTopicSession(true, Session.AUTO_ACKNOWLEDGE);
        TemporaryTopic topic = session.createTemporaryTopic();
        assertNotNull(topic);
        TopicPublisher producer = session.createPublisher(topic);
        MessageConsumer consumer = session.createConsumer(topic);
        conn.start();
        TextMessage sentMessage = session.createTextMessage("Test Message");
        producer.send(sentMessage);
        session.commit();
        TextMessage receivedMessage = (TextMessage) consumer.receive(2000);
        assertNotNull(receivedMessage);
        assertEquals(sentMessage.getText(), receivedMessage.getText());
        producer.send(sentMessage);
        session.commit();
        receivedMessage = (TextMessage) consumer.receive(2000);
        assertNotNull(receivedMessage);
        assertEquals(sentMessage.getText(), receivedMessage.getText());
        session.commit();
        conn.close();

    }

    public void testTemporaryTopic() throws Exception
    {
        AMQConnection conn = (AMQConnection) getConnection("guest", "guest");
        TopicSession session = conn.createTopicSession(true, Session.AUTO_ACKNOWLEDGE);
        TemporaryTopic topic = session.createTemporaryTopic();
        assertNotNull(topic);
        TopicPublisher producer = session.createPublisher(topic);
        MessageConsumer consumer = session.createConsumer(topic);
        conn.start();
        producer.send(session.createTextMessage("hello"));
        session.commit();
        TextMessage tm = (TextMessage) consumer.receive(2000);
        assertNotNull(tm);
        assertEquals("hello", tm.getText());
        session.commit();
        try
        {
            topic.delete();
            fail("Expected JMSException : should not be able to delete while there are active consumers");
        }
        catch (JMSException je)
        {
            ; //pass
        }

        consumer.close();

        try
        {
            topic.delete();
        }
        catch (JMSException je)
        {
            fail("Unexpected Exception: " + je.getMessage());
        }

        TopicSession session2 = conn.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
        try
        {
            session2.createConsumer(topic);
            fail("Expected a JMSException when subscribing to a temporary topic created on adifferent session");
        }
        catch (JMSException je)
        {
            ; // pass
        }


        conn.close();
    }


    public void testNoLocal() throws Exception
    {

        AMQConnection con = (AMQConnection) getConnection("guest", "guest");

        AMQTopic topic = new AMQTopic(con, "testNoLocal");

        TopicSession session1 = con.createTopicSession(true, AMQSession.AUTO_ACKNOWLEDGE);
        TopicSubscriber noLocal = session1.createSubscriber(topic,  "", true);

        TopicSubscriber select = session1.createSubscriber(topic,  "Selector = 'select'", false);
        TopicSubscriber normal = session1.createSubscriber(topic);


        TopicPublisher publisher = session1.createPublisher(topic);

        con.start();
        TextMessage m;
        TextMessage message;

        //send message to all consumers
        publisher.publish(session1.createTextMessage("hello-new2"));
        session1.commit();
        //test normal subscriber gets message
        m = (TextMessage) normal.receive(1000);
        assertNotNull(m);
        session1.commit();

        //test selector subscriber doesn't message
        m = (TextMessage) select.receive(1000);
        assertNull(m);
        session1.commit();

        //test nolocal subscriber doesn't message
        m = (TextMessage) noLocal.receive(1000);
        if (m != null)
        {
            System.out.println("Message:" + m.getText());
        }
        assertNull(m);

        //send message to all consumers
        message = session1.createTextMessage("hello2");
        message.setStringProperty("Selector", "select");

        publisher.publish(message);
        session1.commit();

        //test normal subscriber gets message
        m = (TextMessage) normal.receive(1000);
        assertNotNull(m);
        session1.commit();

        //test selector subscriber does get message
        m = (TextMessage) select.receive(1000);
        assertNotNull(m);
        session1.commit();

        //test nolocal subscriber doesn't message
        m = (TextMessage) noLocal.receive(100);
        assertNull(m);

        AMQConnection con2 = (AMQConnection) getConnection("guest", "guest", "foo");
        TopicSession session2 = con2.createTopicSession(true, AMQSession.AUTO_ACKNOWLEDGE);
        TopicPublisher publisher2 = session2.createPublisher(topic);


        message = session2.createTextMessage("hello2");
        message.setStringProperty("Selector", "select");

        publisher2.publish(message);
        session2.commit();

        //test normal subscriber gets message
        m = (TextMessage) normal.receive(1000);
        assertNotNull(m);
        session1.commit();

        //test selector subscriber does get message
        m = (TextMessage) select.receive(1000);
        assertNotNull(m);
        session1.commit();

        //test nolocal subscriber does message
        m = (TextMessage) noLocal.receive(1000);
        assertNotNull(m);


        con.close();
        con2.close();
    }

    /**
     * This tests QPID-1191, where messages which are sent to a topic but are not consumed by a subscriber
     * due to a selector can be leaked.
     * @throws Exception
     */
    public void testNonMatchingMessagesDoNotFillQueue() throws Exception
    {
        AMQConnection con = (AMQConnection) getConnection("guest", "guest");

        // Setup Topic
        AMQTopic topic = new AMQTopic(con, "testNoLocal");

        TopicSession session = con.createTopicSession(true, AMQSession.NO_ACKNOWLEDGE);

        // Setup subscriber with selector
        TopicSubscriber selector = session.createSubscriber(topic,  "Selector = 'select'", false);
        TopicPublisher publisher = session.createPublisher(topic);

        con.start();
        TextMessage m;
        TextMessage message;

        // Send non-matching message
        message = session.createTextMessage("non-matching 1");
        publisher.publish(message);
        session.commit();

        // Send and consume matching message
        message = session.createTextMessage("hello");
        message.setStringProperty("Selector", "select");

        publisher.publish(message);
        session.commit();

        m = (TextMessage) selector.receive(1000);
        assertNotNull("should have received message", m);
        assertEquals("Message contents were wrong", "hello", m.getText());

        // Send non-matching message
        message = session.createTextMessage("non-matching 2");
        publisher.publish(message);
        session.commit();

        // Assert queue count is 0
        long depth = ((AMQTopicSessionAdaptor) session).getSession().getQueueDepth(topic);
        assertEquals("Queue depth was wrong", 0, depth);

    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(TopicSessionTest.class);
    }
}
