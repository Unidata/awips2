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
import java.util.UUID;

/**
 * QPID-293 Setting MessageListener after connection has started can cause messages to be "lost" on a internal delivery
 * queue <p/> The message delivery process: Mina puts a message on _queue in AMQSession and the dispatcher thread
 * take()s from here and dispatches to the _consumers. If the _consumer1 doesn't have a message listener set at
 * connection start then messages are stored on _synchronousQueue (which needs to be > 1 to pass JMS TCK as multiple
 * consumers on a session can run in any order and a synchronous put/poll will block the dispatcher). <p/> When setting
 * the message listener later the _synchronousQueue is just poll()'ed and the first message delivered the remaining
 * messages will be left on the queue and lost, subsequent messages on the session will arrive first.
 */
public class MessageListenerMultiConsumerTest extends QpidTestCase
{
    private static final Logger _logger = LoggerFactory.getLogger(MessageListenerMultiConsumerTest.class);

    Context _context;

    private static final int MSG_COUNT = 6;
    private int receivedCount1 = 0;
    private int receivedCount2 = 0;
    private Connection _clientConnection;
    private MessageConsumer _consumer1;
    private MessageConsumer _consumer2;
    private Session _clientSession1;
    private Queue _queue;
    private final CountDownLatch _allMessagesSent = new CountDownLatch(2); // all messages Sent Lock
    private static final String QUEUE_NAME = "queue" + UUID.randomUUID().toString();

    protected void setUp() throws Exception
    {
        super.setUp();

        // Create Client 1
        _clientConnection = getConnection("guest", "guest");

        _clientConnection.start();

        _clientSession1 = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        _queue =_clientSession1.createQueue(QUEUE_NAME);

        _consumer1 = _clientSession1.createConsumer(_queue);

        // Create Client 2
        Session clientSession2 = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

       _consumer2 = clientSession2.createConsumer(_queue);

        // Create Producer
        Connection producerConnection = getConnection("guest", "guest");

        producerConnection.start();

        Session producerSession = producerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        MessageProducer producer = producerSession.createProducer(_queue);

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

    public void testRecieveInterleaved() throws Exception
    {
        int msg = 0;
        int MAX_LOOPS = MSG_COUNT * 2;
        for (int loops = 0; (msg < MSG_COUNT) || (loops < MAX_LOOPS); loops++)
        {

            if (_consumer1.receive(1000) != null)
            {
                msg++;
            }

            if (_consumer2.receive(1000) != null)
            {
                msg++;
            }
        }

        assertEquals("Not all messages received.", MSG_COUNT, msg);
    }

    public void testAsynchronousRecieve() throws Exception
    {
        _consumer1.setMessageListener(new MessageListener()
            {
                public void onMessage(Message message)
                {
                    _logger.info("Client 1 Received Message(" + receivedCount1 + "):" + message);

                    receivedCount1++;

                    if (receivedCount1 == (MSG_COUNT / 2))
                    {
                        _allMessagesSent.countDown();
                    }

                }
            });

        _consumer2.setMessageListener(new MessageListener()
            {
                public void onMessage(Message message)
                {
                    _logger.info("Client 2 Received Message(" + receivedCount2 + "):" + message);

                    receivedCount2++;
                    if (receivedCount2 == (MSG_COUNT / 2))
                    {
                        _allMessagesSent.countDown();
                    }
                }
            });

        _logger.info("Waiting upto 2 seconds for messages");

        try
        {
            _allMessagesSent.await(4000, TimeUnit.MILLISECONDS);
        }
        catch (InterruptedException e)
        {
            // do nothing
        }

        assertEquals(MSG_COUNT, receivedCount1 + receivedCount2);
    }

    public void testRecieveC2Only() throws Exception
    {
        if (
            !Boolean.parseBoolean(
                    System.getProperties().getProperty(AMQSession.IMMEDIATE_PREFETCH,
                        AMQSession.IMMEDIATE_PREFETCH_DEFAULT)))
        {
            _logger.info("Performing Receive only on C2");
            for (int msg = 0; msg < MSG_COUNT; msg++)
            {
                assertTrue(MSG_COUNT + " msg should be received. Only received:" + msg, _consumer2.receive(1000) != null);
            }
        }
    }

    public void testRecieveBoth() throws Exception
    {
        if (
            !Boolean.parseBoolean(
                    System.getProperties().getProperty(AMQSession.IMMEDIATE_PREFETCH,
                        AMQSession.IMMEDIATE_PREFETCH_DEFAULT)))
        {
            _logger.info("Performing Receive only with two consumers on one session ");

            //Create a new consumer on session one that we don't use
            _clientSession1.createConsumer(_queue);

            int msg;
            for (msg = 0; msg < (MSG_COUNT / 2); msg++)
            {

                // Attempt to receive up to half the messages
                // The other half may have gone to the consumer above
                final Message message = _consumer1.receive(1000);
                if(message == null)
                {
                    break;
                }

            }

            _consumer1.close();
            // This will close the unused consumer above.
            _clientSession1.close();


            // msg will now have recorded the number received on session 1
            // attempt to retrieve the rest on session 2
            for (; msg < MSG_COUNT ; msg++)
            {
                assertTrue("Failed at msg id" + msg, _consumer2.receive(1000) != null);
            }

        }
        else
        {
            _logger.info("Performing Receive only on both C1 and C2");

            for (int msg = 0; msg < (MSG_COUNT / 2); msg++)
            {

                assertTrue(_consumer1.receive(3000) != null);
            }

            for (int msg = 0; msg < (MSG_COUNT / 2); msg++)
            {
                assertTrue(_consumer2.receive(3000) != null);
            }
        }
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(MessageListenerMultiConsumerTest.class);
    }
}
