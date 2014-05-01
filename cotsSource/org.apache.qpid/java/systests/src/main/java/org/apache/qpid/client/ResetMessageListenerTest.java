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
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.naming.Context;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * QPID-293 Setting MessageListener after connection has started can cause messages to be "lost" on a internal delivery
 * queue <p/> The message delivery process: Mina puts a message on _queue in AMQSession and the dispatcher thread
 * take()s from here and dispatches to the _consumers. If the _consumer1 doesn't have a message listener set at
 * connection start then messages are stored on _synchronousQueue (which needs to be > 1 to pass JMS TCK as multiple
 * consumers on a session can run in any order and a synchronous put/poll will block the dispatcher). <p/> When setting
 * the message listener later the _synchronousQueue is just poll()'ed and the first message delivered the remaining
 * messages will be left on the queue and lost, subsequent messages on the session will arrive first.
 */
public class ResetMessageListenerTest extends QpidTestCase
{
    private static final Logger _logger = LoggerFactory.getLogger(ResetMessageListenerTest.class);

    Context _context;

    private static final int MSG_COUNT = 6;
    private Connection _clientConnection, _producerConnection;
    private MessageConsumer _consumer1;
    MessageProducer _producer;
    Session _clientSession, _producerSession;

    private final CountDownLatch _allFirstMessagesSent = new CountDownLatch(MSG_COUNT); // all messages Sent Lock
    private final CountDownLatch _allSecondMessagesSent = new CountDownLatch(MSG_COUNT); // all messages Sent Lock

    protected void setUp() throws Exception
    {
        super.setUp();

        _clientConnection = getConnection("guest", "guest");
        _clientConnection.start();        
        // Create Client 1

        _clientSession = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        Queue queue = _clientSession.createQueue("reset-message-listener-test-queue");

        _consumer1 = _clientSession.createConsumer(queue);

        // Create Producer
        _producerConnection = getConnection("guest", "guest");

        _producerConnection.start();

        _producerSession = _producerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        _producer = _producerSession.createProducer(queue);

        TextMessage m = _producerSession.createTextMessage();
        m.setStringProperty("rank", "first");
        for (int msg = 0; msg < MSG_COUNT; msg++)
        {
            m.setText("Message " + msg);
            _producer.send(m);
        }
    }

    protected void tearDown() throws Exception
    {
        _clientConnection.close();

        super.tearDown();
    }

    public void testAsynchronousRecieve()
    {

        _logger.info("Test Start");
        
        try
        {
            _consumer1.setMessageListener(new MessageListener()
            {
                public void onMessage(Message message)
                {
                    try
                    {
                        if (message.getStringProperty("rank").equals("first"))
                        {
                            _allFirstMessagesSent.countDown();
                        }
                    }
                    catch (JMSException e)
                    {
                        e.printStackTrace();
                        fail("error receiving message");
                    }
                }
            });
        }
        catch (JMSException e)
        {
            _logger.error("Error Setting Default ML on consumer1");
        }
        try
        {
            assertTrue("Did not receive all first batch of messages", 
                    _allFirstMessagesSent.await(1000, TimeUnit.MILLISECONDS));
            _logger.info("Received first batch of messages");
        }
        catch (InterruptedException e)
        {
            // do nothing
        }

        try
        {
            _clientConnection.stop();
        }
        catch (JMSException e)
        {
            _logger.error("Error stopping connection");
        }

        _logger.info("Reset Message Listener ");
        try
        {
            _consumer1.setMessageListener(new MessageListener()
            {
                public void onMessage(Message message)
                {
                    try
                    {
                        if (message.getStringProperty("rank").equals("first"))
                        {
                            // Something ugly will happen, it'll probably kill the dispatcher
                            fail("All first set of messages should have been received");
                        }
                        else
                        {
                            _allSecondMessagesSent.countDown();
                        }
                    }
                    catch (JMSException e)
                    {
                        e.printStackTrace();
                        // Something ugly will happen, it'll probably kill the dispatcher
                        fail("error receiving message");
                    }
                }
            });

            _clientConnection.start();
        }
        catch (javax.jms.IllegalStateException e)
        {
            _logger.error("Connection not stopped while setting ML", e);
            fail("Unable to change message listener:" + e.getCause());
        }
        catch (JMSException e)
        {
            _logger.error("Error Setting Better ML on consumer1", e);
        }

        try
        {
            _logger.info("Send additional messages");
            TextMessage m = _producerSession.createTextMessage();
            m.setStringProperty("rank", "second");
            for (int msg = 0; msg < MSG_COUNT; msg++)
            {
                m.setText("Message " + msg);
                _producer.send(m);
            }
        }
        catch (JMSException e)
        {
            _logger.error("Unable to send additional messages", e);
        }

        _logger.info("Waiting for messages");

        try
        {
            assertTrue(_allSecondMessagesSent.await(1000, TimeUnit.MILLISECONDS));
        }
        catch (InterruptedException e)
        {
            // do nothing
        }
        assertEquals("First batch of messages not received correctly", 0, _allFirstMessagesSent.getCount());
        assertEquals("Second batch of messages not received correctly", 0, _allSecondMessagesSent.getCount());                  
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(ResetMessageListenerTest.class);
    }
}
