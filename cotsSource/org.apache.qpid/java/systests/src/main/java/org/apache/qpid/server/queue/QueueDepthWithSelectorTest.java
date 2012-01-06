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
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.jndi.PropertiesFileInitialContextFactory;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.naming.Context;
import javax.naming.NamingException;
import javax.naming.spi.InitialContextFactory;
import java.util.Hashtable;

/**
 * Test Case to ensure that messages are correctly returned.
 * This includes checking:
 * - The message is returned.
 * - The broker doesn't leak memory.
 * - The broker's state is correct after test.
 */
public class QueueDepthWithSelectorTest extends TestCase
{
    protected static final Logger _logger = Logger.getLogger(QueueDepthWithSelectorTest.class);

    protected final String BROKER = "vm://:1";
    protected final String VHOST = "test";
    protected final String QUEUE = this.getClass().getName();

    protected Context _context;

    protected Connection _clientConnection;
    protected Connection _producerConnection;
    private Session _clientSession;
    protected Session _producerSession;
    protected MessageProducer _producer;
    private MessageConsumer _consumer;

    protected static int MSG_COUNT = 50;

    protected Message[] _messages = new Message[MSG_COUNT];

    protected Queue _queue;

    protected void setUp() throws Exception
    {

        System.err.println("amqj.logging.level:" + System.getProperty("amqj.logging.level"));
        System.err.println("_logger.level:" + _logger.getLevel());
        System.err.println("_logger.isE-Error:" + _logger.isEnabledFor(Level.ERROR));
        System.err.println("_logger.isE-Warn:" + _logger.isEnabledFor(Level.WARN));
        System.err.println("_logger.isInfo:" + _logger.isInfoEnabled() + ":" + _logger.isEnabledFor(Level.INFO));
        System.err.println("_logger.isDebug:" + _logger.isDebugEnabled() + ":" + _logger.isEnabledFor(Level.DEBUG));
        System.err.println("_logger.isTrace:" + _logger.isTraceEnabled() + ":" + _logger.isEnabledFor(Level.TRACE));

        System.err.println(Logger.getRootLogger().getLoggerRepository());

        if (BROKER.startsWith("vm://"))
        {
            ApplicationRegistry.getInstance(1);
            TransportConnection.createVMBroker(1);
        }
        InitialContextFactory factory = new PropertiesFileInitialContextFactory();

        Hashtable<String, String> env = new Hashtable<String, String>();

        env.put("connectionfactory.connection", "amqp://guest:guest@TTL_TEST_ID/" + VHOST + "?brokerlist='" + BROKER + "'");
        env.put("queue.queue", QUEUE);

        _context = factory.getInitialContext(env);

        _messages = new Message[MSG_COUNT];
        _queue = (Queue) _context.lookup("queue");
        init();
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();

        if (_producerConnection != null)
        {
            _producerConnection.close();
        }

        if (_clientConnection != null)
        {
            _clientConnection.close();
        }

        if (BROKER.startsWith("vm://"))
        {
            TransportConnection.killVMBroker(1);
            ApplicationRegistry.remove(1);
        }
    }

    public void test() throws Exception
    {
        //Send messages
        _logger.info("Starting to send messages");
        for (int msg = 0; msg < MSG_COUNT; msg++)
        {
            _producer.send(nextMessage(msg));
        }
        _logger.info("Closing connection");
        //Close the connection.. .giving the broker time to clean up its state.
        _producerConnection.close();

        //Verify we get all the messages.
        _logger.info("Verifying messages");
        verifyAllMessagesRecevied(0);

        //Close the connection.. .giving the broker time to clean up its state.
        _clientConnection.close();

        //Verify Broker state
        _logger.info("Verifying broker state");
        verifyBrokerState(0);
    }

    protected void init() throws NamingException, JMSException, AMQException
    {
        //Create Producer
        _producerConnection = ((ConnectionFactory) _context.lookup("connection")).createConnection();
        _producerConnection.start();
        _producerSession = _producerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        _producer = _producerSession.createProducer(_queue);

        // Create consumer
        _clientConnection = ((ConnectionFactory) _context.lookup("connection")).createConnection();
        _clientConnection.start();
        _clientSession = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        _consumer = _clientSession.createConsumer(_queue, "key = 23");
    }

    protected void verifyBrokerState(int expectedDepth)
    {
        try
        {
            _clientConnection = ((ConnectionFactory) _context.lookup("connection")).createConnection();

            _clientSession = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        }
        catch (Exception e)
        {
            fail(e.getMessage());
        }

        try
        {
            Thread.sleep(2000);
            long queueDepth = ((AMQSession) _clientSession).getQueueDepth((AMQDestination) _queue);
            assertEquals("Session reports Queue depth not as expected", expectedDepth, queueDepth);
        }
        catch (InterruptedException e)
        {
            fail(e.getMessage());
        }
        catch (AMQException e)
        {
            fail(e.getMessage());
        }
        finally
        {
            try
            {
                _clientConnection.close();
            }
            catch (JMSException e)
            {
                fail(e.getMessage());
            }
        }

    }

    protected void verifyAllMessagesRecevied(int expectedDepth) throws Exception
    {

        boolean[] msgIdRecevied = new boolean[MSG_COUNT];

        for (int i = 0; i < MSG_COUNT; i++)
        {
            _messages[i] = _consumer.receive(1000);
            assertNotNull("should have received a message but didn't", _messages[i]);
        }

        long queueDepth = ((AMQSession) _clientSession).getQueueDepth((AMQDestination) _queue);
        assertEquals("Session reports Queue depth not as expected", expectedDepth, queueDepth);

        //Check received messages
        int msgId = 0;
        for (Message msg : _messages)
        {
            assertNotNull("Message should not be null", msg);
            assertEquals("msgId was wrong", msgId, msg.getIntProperty("ID"));
            assertFalse("Already received msg id " + msgId, msgIdRecevied[msgId]);
            msgIdRecevied[msgId] = true;
            msgId++;
        }

        //Check all received
        for (msgId = 0; msgId < MSG_COUNT; msgId++)
        {
            assertTrue("Message " + msgId + " not received.", msgIdRecevied[msgId]);
        }
    }

    /**
     * Get the next message putting the given count into the intProperties as ID.
     *
     * @param msgNo the message count to store as ID.
     *
     * @return
     *
     * @throws JMSException
     */
    protected Message nextMessage(int msgNo) throws JMSException
    {
        Message send = _producerSession.createTextMessage("MessageReturnTest");
        send.setIntProperty("ID", msgNo);
        send.setIntProperty("key", 23);
        return send;
    }

}
