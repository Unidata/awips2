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

import org.apache.qpid.test.utils.QpidTestCase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.Connection;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.naming.Context;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * QPID-293 Setting MessageListener after connection has started can cause messages to be "lost" on a internal delivery
 * queue <p/> The message delivery process: Mina puts a message on _queue in AMQSession and the dispatcher thread
 * take()s from here and dispatches to the _consumers. If the _consumer doesn't have a message listener set at
 * connection start then messages are stored on _synchronousQueue (which needs to be > 1 to pass JMS TCK as multiple
 * consumers on a session can run in any order and a synchronous put/poll will block the dispatcher). <p/> When setting
 * the message listener later the _synchronousQueue is just poll()'ed and the first message delivered the remaining
 * messages will be left on the queue and lost, subsequent messages on the session will arrive first.
 */
public class MessageListenerTest extends QpidTestCase implements MessageListener
{
    private static final Logger _logger = LoggerFactory.getLogger(MessageListenerTest.class);

    Context _context;

    private static final int MSG_COUNT = 5;
    private int receivedCount = 0;
    private MessageConsumer _consumer;
    private Connection _clientConnection;
    private CountDownLatch _awaitMessages = new CountDownLatch(MSG_COUNT);

    protected void setUp() throws Exception
    {
        super.setUp();

        // Create Client
        _clientConnection = getConnection("guest", "guest");

        _clientConnection.start();

        Session clientSession = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        Queue queue =clientSession.createQueue("message-listener-test-queue");

        _consumer = clientSession.createConsumer(queue);

        // Create Producer

        Connection producerConnection = getConnection("guest", "guest");

        producerConnection.start();

        Session producerSession = producerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        MessageProducer producer = producerSession.createProducer(queue);

        for (int msg = 0; msg < MSG_COUNT; msg++)
        {
            producer.send(producerSession.createTextMessage("Message " + msg));
        }

        producerConnection.close();

    }

    protected void tearDown() throws Exception
    {
        _clientConnection.close();
        super.tearDown();
    }

    public void testSynchronousRecieve() throws Exception
    {
        for (int msg = 0; msg < MSG_COUNT; msg++)
        {
            assertTrue(_consumer.receive(2000) != null);
        }
    }

    public void testSynchronousRecieveNoWait() throws Exception
    {
        for (int msg = 0; msg < MSG_COUNT; msg++)
        {
            assertTrue(_consumer.receiveNoWait() != null);
        }
    }

    public void testAsynchronousRecieve() throws Exception
    {
        _consumer.setMessageListener(this);

        _logger.info("Waiting 3 seconds for messages");

        try
        {
            _awaitMessages.await(3000, TimeUnit.MILLISECONDS);
        }
        catch (InterruptedException e)
        {
            // do nothing
        }
        // Should have recieved all async messages
        assertEquals(MSG_COUNT, receivedCount);

    }

    public void testRecieveThenUseMessageListener() throws Exception
    {

        _logger.error("Test disabled as initial receive is not called first");
        // Perform initial receive to start connection
        assertTrue(_consumer.receive(2000) != null);
        receivedCount++;

        // Sleep to ensure remaining 4 msgs end up on _synchronousQueue
        Thread.sleep(1000);

        // Set the message listener and wait for the messages to come in.
        _consumer.setMessageListener(this);

        _logger.info("Waiting 3 seconds for messages");

        try
        {
            _awaitMessages.await(3000, TimeUnit.MILLISECONDS);
        }
        catch (InterruptedException e)
        {
            // do nothing
        }
        // Should have recieved all async messages
        assertEquals(MSG_COUNT, receivedCount);

        _clientConnection.close();

        Connection conn = getConnection("guest", "guest");
        Session clientSession = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);
        Queue queue = clientSession.createQueue("message-listener-test-queue");
        MessageConsumer cons = clientSession.createConsumer(queue);
        conn.start();

        // check that the messages were actually dequeued
        assertTrue(cons.receive(2000) == null);
    }

    public void onMessage(Message message)
    {
        _logger.info("Received Message(" + receivedCount + "):" + message);

        receivedCount++;
        _awaitMessages.countDown();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(MessageListenerTest.class);
    }
}
