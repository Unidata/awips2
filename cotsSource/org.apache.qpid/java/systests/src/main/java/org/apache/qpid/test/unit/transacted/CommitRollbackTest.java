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
package org.apache.qpid.test.unit.transacted;

import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.client.AMQConnection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * This class tests a number of commits and roll back scenarios
 *
 * Assumptions; - Assumes empty Queue
 */
public class CommitRollbackTest extends QpidTestCase
{
    protected AMQConnection conn;
    protected String queue = "direct://amq.direct//Qpid.Client.Transacted.CommitRollback.queue";
    protected static int testMethod = 0;
    protected String payload = "xyzzy";
    private Session _session;
    private MessageProducer _publisher;
    private Session _pubSession;
    private MessageConsumer _consumer;
    Queue _jmsQueue;

    private static final Logger _logger = LoggerFactory.getLogger(CommitRollbackTest.class);
    private boolean _gotone = false;
    private boolean _gottwo = false;
    private boolean _gottwoRedelivered = false;

    protected void setUp() throws Exception
    {
        super.setUp();
        testMethod++;
        queue += testMethod;
       newConnection();
    }

    private void newConnection() throws Exception
    {
        conn = (AMQConnection) getConnection("guest", "guest");

        _session = conn.createSession(true, Session.CLIENT_ACKNOWLEDGE);

        _jmsQueue = _session.createQueue(queue);
        _consumer = _session.createConsumer(_jmsQueue);

        _pubSession = conn.createSession(true, Session.CLIENT_ACKNOWLEDGE);

        _publisher = _pubSession.createProducer(_pubSession.createQueue(queue));

        conn.start();
    }

    protected void tearDown() throws Exception
    {
        conn.close();
        super.tearDown();
    }

    /**
     * PUT a text message, disconnect before commit, confirm it is gone.
     *
     * @throws Exception On error
     */
    public void testPutThenDisconnect() throws Exception
    {
        assertTrue("session is not transacted", _session.getTransacted());
        assertTrue("session is not transacted", _pubSession.getTransacted());

        _logger.info("sending test message");
        String MESSAGE_TEXT = "testPutThenDisconnect";
        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));

        _logger.info("reconnecting without commit");
        conn.close();

        newConnection();

        _logger.info("receiving result");
        Message result = _consumer.receive(1000);

        // commit to ensure message is removed from queue
        _session.commit();

        assertNull("test message was put and disconnected before commit, but is still present", result);
    }

    /**
     * PUT a text message, disconnect before commit, confirm it is gone.
     *
     * @throws Exception On error
     */
    public void testPutThenCloseDisconnect() throws Exception
    {
        assertTrue("session is not transacted", _session.getTransacted());
        assertTrue("session is not transacted", _pubSession.getTransacted());

        _logger.info("sending test message");
        String MESSAGE_TEXT = "testPutThenDisconnect";
        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));

        _logger.info("closing publisher without commit");
        _publisher.close();

        _logger.info("reconnecting without commit");
        conn.close();

        newConnection();

        _logger.info("receiving result");
        Message result = _consumer.receive(1000);

        // commit to ensure message is removed from queue
        _session.commit();

        assertNull("test message was put and disconnected before commit, but is still present", result);
    }

    /**
     * PUT a text message, rollback, confirm message is gone. The consumer is on the same connection but different
     * session as producer
     *
     * @throws Exception On error
     */
    public void testPutThenRollback() throws Exception
    {
        assertTrue("session is not transacted", _session.getTransacted());
        assertTrue("session is not transacted", _pubSession.getTransacted());

        _logger.info("sending test message");
        String MESSAGE_TEXT = "testPutThenRollback";
        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));

        _logger.info("rolling back");
        _pubSession.rollback();

        _logger.info("receiving result");
        Message result = _consumer.receive(1000);

        assertNull("test message was put and rolled back, but is still present", result);
    }

    /**
     * GET a text message, disconnect before commit, confirm it is still there. The consumer is on a new connection
     *
     * @throws Exception On error
     */
    public void testGetThenDisconnect() throws Exception
    {
        assertTrue("session is not transacted", _session.getTransacted());
        assertTrue("session is not transacted", _pubSession.getTransacted());

        _logger.info("sending test message");
        String MESSAGE_TEXT = "testGetThenDisconnect";
        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));

        _pubSession.commit();

        _logger.info("getting test message");

        Message msg = _consumer.receive(1000);
        assertNotNull("retrieved message is null", msg);

        _logger.info("closing connection");
        conn.close();

        newConnection();

        _logger.info("receiving result");
        Message result = _consumer.receive(1000);

        _session.commit();

        assertNotNull("test message was consumed and disconnected before commit, but is gone", result);
        assertEquals("test message was correct message", MESSAGE_TEXT, ((TextMessage) result).getText());
    }

    /**
     * GET a text message, close consumer, disconnect before commit, confirm it is still there. The consumer is on the
     * same connection but different session as producer
     *
     * @throws Exception On error
     */
    public void testGetThenCloseDisconnect() throws Exception
    {
        assertTrue("session is not transacted", _session.getTransacted());
        assertTrue("session is not transacted", _pubSession.getTransacted());

        _logger.info("sending test message");
        String MESSAGE_TEXT = "testGetThenCloseDisconnect";
        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));

        _pubSession.commit();

        _logger.info("getting test message");

        Message msg = _consumer.receive(1000);
        assertNotNull("retrieved message is null", msg);
        assertEquals("test message was correct message", MESSAGE_TEXT, ((TextMessage) msg).getText());

        _logger.info("reconnecting without commit");
        _consumer.close();
        conn.close();

        newConnection();

        _logger.info("receiving result");
        Message result = _consumer.receive(1000);

        _session.commit();

        assertNotNull("test message was consumed and disconnected before commit, but is gone", result);
        assertEquals("test message was correct message", MESSAGE_TEXT, ((TextMessage) result).getText());
    }

    /**
     * GET a text message, rollback, confirm it is still there. The consumer is on the same connection but differnt
     * session to the producer
     *
     * @throws Exception On error
     */
    public void testGetThenRollback() throws Exception
    {
        assertTrue("session is not transacted", _session.getTransacted());
        assertTrue("session is not transacted", _pubSession.getTransacted());

        _logger.info("sending test message");
        String MESSAGE_TEXT = "testGetThenRollback";
        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));

        _pubSession.commit();

        _logger.info("getting test message");

        Message msg = _consumer.receive(1000);

        assertNotNull("retrieved message is null", msg);
        assertEquals("test message was correct message", MESSAGE_TEXT, ((TextMessage) msg).getText());

        _logger.info("rolling back");

        _session.rollback();

        _logger.info("receiving result");

        Message result = _consumer.receive(1000);

        _session.commit();
        assertNotNull("test message was consumed and rolled back, but is gone", result);
        assertEquals("test message was correct message", MESSAGE_TEXT, ((TextMessage) result).getText());
        assertTrue("Message is not marked as redelivered", result.getJMSRedelivered());
    }

    /**
     * GET a text message, close message producer, rollback, confirm it is still there. The consumer is on the same
     * connection but different session as producer
     *
     * @throws Exception On error
     */
    public void testGetThenCloseRollback() throws Exception
    {
        assertTrue("session is not transacted", _session.getTransacted());
        assertTrue("session is not transacted", _pubSession.getTransacted());

        _logger.info("sending test message");
        String MESSAGE_TEXT = "testGetThenCloseRollback";
        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));

        _pubSession.commit();

        _logger.info("getting test message");

        Message msg = _consumer.receive(1000);

        assertNotNull("retrieved message is null", msg);
        assertEquals("test message was correct message", MESSAGE_TEXT, ((TextMessage) msg).getText());

        _logger.info("Closing consumer");
        _consumer.close();

        _logger.info("rolling back");
        _session.rollback();

        _logger.info("receiving result");

        _consumer = _session.createConsumer(_jmsQueue);

        Message result = _consumer.receive(1000);

        _session.commit();
        assertNotNull("test message was consumed and rolled back, but is gone", result);
        assertEquals("test message was correct message", MESSAGE_TEXT, ((TextMessage) result).getText());
        assertTrue("Message is not marked as redelivered", result.getJMSRedelivered());
    }

    /**
     * Test that rolling back a session purges the dispatcher queue, and the messages arrive in the correct order
     *
     * @throws Exception On error
     */
    public void testSend2ThenRollback() throws Exception
    {
        int run = 0;
        while (run < 10)
        {
            run++;
            _logger.info("Run:" + run);
            assertTrue("session is not transacted", _session.getTransacted());
            assertTrue("session is not transacted", _pubSession.getTransacted());

            _logger.info("sending two test messages");
            _publisher.send(_pubSession.createTextMessage("1"));
            _publisher.send(_pubSession.createTextMessage("2"));
            _pubSession.commit();

            _logger.info("getting test message");
            assertEquals("1", ((TextMessage) _consumer.receive(1000)).getText());

            _logger.info("rolling back");
            _session.rollback();

            _logger.info("receiving result");
            Message result = _consumer.receive(1000);

            assertNotNull("test message was consumed and rolled back, but is gone", result);

            // Message Order is:

            // Send 1 , 2
            // Retrieve 1 and then rollback
            // Receieve 1 (redelivered) , 2 (may or may not be redelivered??)

            verifyMessages(result);

            // Occassionally get message 2 first!
//            assertEquals("Should get message one first", "1", ((TextMessage) result).getText());
//            assertTrue("Message is not marked as redelivered", result.getJMSRedelivered());
//
//            result = _consumer.receive(1000);
//            assertEquals("Second message should be message 2", "2", ((TextMessage) result).getText());
//            assertTrue("Message is not marked as redelivered", result.getJMSRedelivered());
//
//            result = _consumer.receive(1000);
//            assertNull("There should be no more messages", result);

            _session.commit();
        }
    }

    private void verifyMessages(Message result) throws JMSException
    {

        if (result == null)
        {
            assertTrue("Didn't receive redelivered message one", _gotone);
            assertTrue("Didn't receive message two at all", _gottwo | _gottwoRedelivered);
            _gotone = false;
            _gottwo = false;
            _gottwoRedelivered = false;
            return;
        }

        if (((TextMessage) result).getText().equals("1"))
        {
            _logger.info("Got 1 redelivered");
            assertTrue("Message is not marked as redelivered", result.getJMSRedelivered());
            assertFalse("Already received message one", _gotone);
            _gotone = true;

        }
        else
        {
            assertEquals("2", ((TextMessage) result).getText());

            if (result.getJMSRedelivered())
            {
                _logger.info("Got 2 redelivered, message was prefetched");
                assertFalse("Already received message redelivered two", _gottwoRedelivered);

                _gottwoRedelivered = true;
            }
            else
            {
                _logger.warn("Got 2, message prefetched wasn't cleared or messages was in transit when rollback occured");
                assertFalse("Already received message two", _gottwo);
                assertFalse("Already received message redelivered two", _gottwoRedelivered);
                _gottwo = true;
            }
        }

        verifyMessages(_consumer.receive(1000));
    }

    /**
     * This test sends two messages receives on of them but doesn't ack it.
     * The consumer is then closed
     * the first message should be returned as redelivered.
     *  the second message should be delivered normally.
     * @throws Exception
     */
    public void testSend2ThenCloseAfter1andTryAgain() throws Exception
    {
        assertTrue("session is not transacted", _session.getTransacted());
        assertTrue("session is not transacted", _pubSession.getTransacted());

        _logger.info("sending two test messages");
        _publisher.send(_pubSession.createTextMessage("1"));
        _publisher.send(_pubSession.createTextMessage("2"));
        _pubSession.commit();

        _logger.info("getting test message");
        Message result = _consumer.receive(5000);

        assertNotNull("Message received should not be null", result);
        assertEquals("1", ((TextMessage) result).getText());
        assertTrue("Messasge is marked as redelivered" + result, !result.getJMSRedelivered());

        _logger.info("Closing Consumer");
        
        _consumer.close();

        _logger.info("Creating New consumer");
        _consumer = _session.createConsumer(_jmsQueue);

        _logger.info("receiving result");


        // Message 2 may be marked as redelivered if it was prefetched.
        result = _consumer.receive(5000);
        assertNotNull("Second message was not consumed, but is gone", result);

        // The first message back will be 2, message 1 has been received but not committed
        // Closing the consumer does not commit the session.

        // if this is message 1 then it should be marked as redelivered
        if("1".equals(((TextMessage) result).getText()))
        {
            fail("First message was recieved again");
        }

        result = _consumer.receive(1000);
        assertNull("test message should be null:" + result, result);

        _session.commit();
    }

    public void testPutThenRollbackThenGet() throws Exception
    {
        assertTrue("session is not transacted", _session.getTransacted());
        assertTrue("session is not transacted", _pubSession.getTransacted());

        _logger.info("sending test message");
        String MESSAGE_TEXT = "testPutThenRollbackThenGet";

        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));
        _pubSession.commit();

        assertNotNull(_consumer.receive(1000));

        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));

        _logger.info("rolling back");
        _pubSession.rollback();

        _logger.info("receiving result");
        Message result = _consumer.receive(1000);
        assertNull("test message was put and rolled back, but is still present", result);

        _publisher.send(_pubSession.createTextMessage(MESSAGE_TEXT));

        _pubSession.commit();

        assertNotNull(_consumer.receive(100));

        _session.commit();
    }

    /**
     * Qpid-1163
     * Check that when commt is called inside onMessage then
     * the last message is nor redelivered. 
     *
     * @throws Exception
     */
    public void testCommitWhithinOnMessage() throws Exception
    {
        Queue queue = (Queue) getInitialContext().lookup("queue");
        // create a consumer
             MessageConsumer cons = _session.createConsumer(queue);           
        MessageProducer prod = _session.createProducer(queue);
        Message message =  _session.createTextMessage("Message");
        message.setJMSCorrelationID("m1");
        prod.send(message);
        _session.commit();
        _logger.info("Sent message to queue");
        CountDownLatch cd = new CountDownLatch(1);
        cons.setMessageListener(new CommitWhithinOnMessageListener(cd));
        conn.start();
        cd.await(30, TimeUnit.SECONDS);
        if( cd.getCount() > 0 )
        {
           fail("Did not received message");
        }
        // Check that the message has been dequeued
        _session.close();
        conn.close();
        conn = (AMQConnection) getConnection("guest", "guest");
        conn.start();
        Session session = conn.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        cons = session.createConsumer(queue);        
        message = cons.receiveNoWait();
        if(message != null)
        {
            if(message.getJMSCorrelationID().equals("m1"))
            {
                fail("received message twice");
            }
            else
            {
                fail("queue should have been empty, received message: " + message);
            }
        }
    }

    private class CommitWhithinOnMessageListener implements MessageListener
    {
        private CountDownLatch _cd;
        private CommitWhithinOnMessageListener(CountDownLatch cd)
        {
            _cd = cd;
        }
        public void onMessage(Message message)
        {
            try
            {
                _logger.info("received message " + message);
                assertEquals("Wrong message received", message.getJMSCorrelationID(), "m1");
                _logger.info("commit session");
                _session.commit();
                _cd.countDown();
            }
            catch (JMSException e)
            {
                e.printStackTrace();
            }
        }
    }
}
