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

import java.util.Hashtable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.naming.Context;
import javax.naming.spi.InitialContextFactory;

import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.jndi.PropertiesFileInitialContextFactory;
import org.apache.qpid.test.utils.QpidTestCase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * QPID-293 Setting MessageListener after connection has started can cause messages to be "lost" on a internal delivery queue
 * <p/>
 * The message delivery process:
 * Mina puts a message on _queue in AMQSession and the dispatcher thread take()s
 * from here and dispatches to the _consumers. If the _consumer doesn't have a message listener set at connection start
 * then messages are stored on _synchronousQueue (which needs to be > 1 to pass JMS TCK as multiple consumers on a
 * session can run in any order and a synchronous put/poll will block the dispatcher).
 * <p/>
 * When setting the message listener later the _synchronousQueue is just poll()'ed and the first message delivered
 * the remaining messages will be left on the queue and lost, subsequent messages on the session will arrive first.
 */
public class DispatcherTest extends QpidTestCase
{
    private static final Logger _logger = LoggerFactory.getLogger(DispatcherTest.class);

    Context _context;

    private static final int MSG_COUNT = 6;
    private int _receivedCount = 0;
    private int _receivedCountWhileStopped = 0;
    private Connection _clientConnection, _producerConnection;
    private MessageConsumer _consumer;
    MessageProducer _producer;
    Session _clientSession, _producerSession;

    private final CountDownLatch _allFirstMessagesSent = new CountDownLatch(1); // all messages Sent Lock
    private final CountDownLatch _allSecondMessagesSent = new CountDownLatch(1); // all messages Sent Lock

    private volatile boolean _connectionStopped = false;

    protected void setUp() throws Exception
    {
        super.setUp();

        InitialContextFactory factory = new PropertiesFileInitialContextFactory();

        Hashtable<String, String> env = new Hashtable<String, String>();

        // Create Client 1
        _clientConnection = getConnection();

        _clientSession = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        Queue queue = _clientSession.createQueue(this.getClass().getName());
        _consumer = _clientSession.createConsumer(queue);

        // Create Producer
        _producerConnection = getConnection();

        _producerConnection.start();

        _producerSession = _producerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        _producer = _producerSession.createProducer(queue);

        for (int msg = 0; msg < MSG_COUNT; msg++)
        {
            _producer.send(_producerSession.createTextMessage("Message " + msg));
        }
    }

    protected void tearDown() throws Exception
    {

        _clientConnection.close();

        _producerConnection.close();
        super.tearDown();
    }

    public void testAsynchronousRecieve()
    {
        _logger.info("Test Start");

        assertTrue(!((AMQConnection) _clientConnection).started());

        // Set default Message Listener
        try
        {
            _consumer.setMessageListener(new MessageListener()
                {
                    public void onMessage(Message message)
                    {
                        _logger.info("Client 1 ML 1 Received Message(" + _receivedCount + "):" + message);

                        _receivedCount++;

                        if (_receivedCount == MSG_COUNT)
                        {
                            _allFirstMessagesSent.countDown();
                        }

                        if (_connectionStopped)
                        {
                            _logger.info("Running with Message:" + _receivedCount);
                        }

                        if (_connectionStopped && (_allFirstMessagesSent.getCount() == 0))
                        {
                            _receivedCountWhileStopped++;
                        }

                        if (_allFirstMessagesSent.getCount() == 0)
                        {
                            if (_receivedCount == (MSG_COUNT * 2))
                            {
                                _allSecondMessagesSent.countDown();
                            }
                        }
                    }
                });

            assertTrue("Connecion should not be started", !((AMQConnection) _clientConnection).started());
            _clientConnection.start();
        }
        catch (JMSException e)
        {
            _logger.error("Error Setting Default ML on consumer1");
        }

        try
        {
            _allFirstMessagesSent.await(1000, TimeUnit.MILLISECONDS);
        }
        catch (InterruptedException e)
        {
            // do nothing
        }

        try
        {
            assertTrue("Connecion should be started", ((AMQConnection) _clientConnection).started());
            _clientConnection.stop();
            _connectionStopped = true;
        }
        catch (JMSException e)
        {
            _logger.error("Error stopping connection");
        }

        try
        {
            _logger.error("Send additional messages");

            for (int msg = 0; msg < MSG_COUNT; msg++)
            {
                _producer.send(_producerSession.createTextMessage("Message " + msg));
            }
        }
        catch (JMSException e)
        {
            _logger.error("Unable to send additional messages", e);
        }

        try
        {
            Thread.sleep(1000);
        }
        catch (InterruptedException e)
        {
            // ignore
        }

        try
        {
            _logger.info("Restarting connection");

            _connectionStopped = false;
            _clientConnection.start();
        }
        catch (JMSException e)
        {
            _logger.error("Error Setting Better ML on consumer1", e);
        }

        _logger.info("Waiting upto 2 seconds for messages");

        try
        {
            _allSecondMessagesSent.await(1000, TimeUnit.MILLISECONDS);
        }
        catch (InterruptedException e)
        {
            // do nothing
        }

        assertEquals("Messages not received correctly", 0, _allFirstMessagesSent.getCount());
        assertEquals("Messages not received correctly", 0, _allSecondMessagesSent.getCount());
        assertEquals("Client didn't get all messages", MSG_COUNT * 2, _receivedCount);
        assertEquals("Messages received while stopped is not 0", 0, _receivedCountWhileStopped);
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(DispatcherTest.class);
    }
}
