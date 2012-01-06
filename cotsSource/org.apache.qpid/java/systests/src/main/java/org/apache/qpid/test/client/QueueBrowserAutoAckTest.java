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
package org.apache.qpid.test.client;

import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.test.utils.FailoverBaseCase;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.QueueBrowser;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.naming.NamingException;
import java.util.Enumeration;
import java.util.Random;

public class QueueBrowserAutoAckTest extends FailoverBaseCase
{
    private static final Logger _logger = Logger.getLogger(QueueBrowserAutoAckTest.class);

    protected Connection _clientConnection;
    protected Session _clientSession;
    protected Queue _queue;
    protected static final String MESSAGE_ID_PROPERTY = "MessageIDProperty";
    protected boolean CLUSTERED = Boolean.getBoolean("profile.clustered");

    public void setUp() throws Exception
    {
        super.setUp();


        //Create Client
        _clientConnection = getConnection();

        _clientConnection.start();

        setupSession();

        _queue = _clientSession.createQueue(getTestQueueName());
        _clientSession.createConsumer(_queue).close();
        
        //Ensure there are no messages on the queue to start with.
        checkQueueDepth(0);
    }

    protected void setupSession() throws Exception
    {
        _clientSession = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);
    }

    public void tearDown() throws Exception
    {
        if (_clientConnection != null)
        {
            _clientConnection.close();
        }

        super.tearDown();
    }

    protected void sendMessages(int num) throws JMSException
    {
        Connection producerConnection = null;
        try
        {
            producerConnection = getConnection();
        }
        catch (Exception e)
        {
            fail("Unable to lookup connection in JNDI.");
        }

        sendMessages(producerConnection, num);
    }

    protected void sendMessages(String connection, int num) throws JMSException
    {
        Connection producerConnection = null;
        try
        {
            producerConnection = getConnectionFactory(connection).createConnection("guest", "guest");
        }
        catch (Exception e)
        {
            e.printStackTrace();
            fail("Unable to lookup connection in JNDI.");
        }
        sendMessages(producerConnection, num);
    }


    protected void sendMessages(Connection producerConnection, int messageSendCount) throws JMSException
    {
        producerConnection.start();

        Session producerSession = producerConnection.createSession(true, Session.AUTO_ACKNOWLEDGE);

        //Ensure _queue is created
        producerSession.createConsumer(_queue).close();

        MessageProducer producer = producerSession.createProducer(_queue);

        for (int messsageID = 0; messsageID < messageSendCount; messsageID++)
        {
            TextMessage textMsg = producerSession.createTextMessage("Message " + messsageID);
            textMsg.setIntProperty(MESSAGE_ID_PROPERTY, messsageID);
            producer.send(textMsg);
        }
        producerSession.commit();

        producerConnection.close();
    }

    /**
     * Using the Protocol getQueueDepth method ensure that the correct number of messages are on the queue.
     *
     * Also uses a QueueBrowser as a second method of validating the message count on the queue.
     *
     * @param expectedDepth The expected Queue depth
     * @throws JMSException on error
     */
    protected void checkQueueDepth(int expectedDepth) throws JMSException
    {

        // create QueueBrowser
        _logger.info("Creating Queue Browser");

        QueueBrowser queueBrowser = _clientSession.createBrowser(_queue);

        // check for messages
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Checking for " + expectedDepth + " messages with QueueBrowser");
        }

        //Check what the session believes the queue count to be.
        long queueDepth = 0;

        try
        {
            queueDepth = ((AMQSession) _clientSession).getQueueDepth((AMQDestination) _queue);
        }
        catch (AMQException e)
        {
        }

        assertEquals("Session reports Queue expectedDepth not as expected", expectedDepth, queueDepth);



        // Browse the queue to get a second opinion
        int msgCount = 0;
        Enumeration msgs = queueBrowser.getEnumeration();

        while (msgs.hasMoreElements())
        {
            msgs.nextElement();
            msgCount++;
        }

        if (_logger.isDebugEnabled())
        {
            _logger.debug("Found " + msgCount + " messages total in browser");
        }

        // check to see if all messages found
        assertEquals("Browser did not find all messages", expectedDepth, msgCount);

        //Close browser
        queueBrowser.close();
    }

    protected void closeBrowserBeforeAfterGetNext(int messageCount) throws JMSException
    {
        QueueBrowser queueBrowser = _clientSession.createBrowser(_queue);

        Enumeration msgs = queueBrowser.getEnumeration();

        int msgCount = 0;

        while (msgs.hasMoreElements() && msgCount < messageCount)
        {
            msgs.nextElement();
            msgCount++;
        }

        try
        {
            queueBrowser.close();
        }
        catch (JMSException e)
        {
            fail("Close should happen without error:" + e.getMessage());
        }
    }

    /**
     * This method checks that multiple calls to getEnumeration() on a queueBrowser provide the same behaviour.
     *
     * @param sentMessages The number of messages sent
     * @param browserEnumerationCount The number of times to call getEnumeration()
     * @throws JMSException
     */
    protected void checkMultipleGetEnum(int sentMessages, int browserEnumerationCount) throws JMSException
    {
        QueueBrowser queueBrowser = _clientSession.createBrowser(_queue);

        for (int count = 0; count < browserEnumerationCount; count++)
        {
            _logger.info("Checking getEnumeration:" + count);
            Enumeration msgs = queueBrowser.getEnumeration();

            int msgCount = 0;

            while (msgs.hasMoreElements())
            {
                msgs.nextElement();
                msgCount++;
            }

            // Verify that the browser can see all the messages sent.
            assertEquals(sentMessages, msgCount);
        }

        try
        {
            queueBrowser.close();
        }
        catch (JMSException e)
        {
            fail("Close should happen without error:" + e.getMessage());
        }
    }

    protected void checkOverlappingMultipleGetEnum(int expectedMessages, int browserEnumerationCount) throws JMSException
    {
        checkOverlappingMultipleGetEnum(expectedMessages, browserEnumerationCount, null);
    }

    protected void checkOverlappingMultipleGetEnum(int expectedMessages, int browserEnumerationCount, String selector) throws JMSException
    {
        QueueBrowser queueBrowser = selector == null ?
                                    _clientSession.createBrowser(_queue, selector) :
                                    _clientSession.createBrowser(_queue);

        Enumeration[] msgs = new Enumeration[browserEnumerationCount];
        int[] msgCount = new int[browserEnumerationCount];

        //create Enums
        for (int count = 0; count < browserEnumerationCount; count++)
        {
            msgs[count] = queueBrowser.getEnumeration();
        }

        //interleave reads
        for (int cnt = 0; cnt < expectedMessages; cnt++)
        {
            for (int count = 0; count < browserEnumerationCount; count++)
            {
                if (msgs[count].hasMoreElements())
                {
                    msgs[count].nextElement();
                    msgCount[count]++;
                }
            }
        }

        //validate all browsers get right message count.
        for (int count = 0; count < browserEnumerationCount; count++)
        {
            assertEquals(msgCount[count], expectedMessages);
        }

        try
        {
            queueBrowser.close();
        }
        catch (JMSException e)
        {
            fail("Close should happen without error:" + e.getMessage());
        }
    }

    protected void validate(int messages) throws JMSException
    {
        //Create a new connection to validate message content
        Connection connection = null;

        try
        {
            connection = getConnection();
        }
        catch (Exception e)
        {
            fail("Unable to make validation connection");
        }

        Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        connection.start();

        MessageConsumer consumer = session.createConsumer(_queue);

        _logger.info("Verify messages are still on the queue");

        Message tempMsg;

        for (int msgCount = 0; msgCount < messages; msgCount++)
        {
            tempMsg = (TextMessage) consumer.receive(RECEIVE_TIMEOUT);
            if (tempMsg == null)
            {
                fail("Message " + msgCount + " not retrieved from queue");
            }
        }

        //Close this new connection
        connection.close();

        _logger.info("All messages recevied from queue");

        //ensure no message left.
        checkQueueDepth(0);
    }

    protected void checkQueueDepthWithSelectors(int totalMessages, int clients) throws JMSException
    {

        String selector = MESSAGE_ID_PROPERTY + " % " + clients;

        checkOverlappingMultipleGetEnum(totalMessages / clients, clients, selector);
    }


    /**
     * This tests you can browse an empty queue, see QPID-785
     *
     * @throws Exception
     */
    public void testBrowsingEmptyQueue() throws Exception
    {
        checkQueueDepth(0);
    }

    /*
    * Test Messages Remain on Queue
    * Create a queu and send messages to it. Browse them and then receive them all to verify they were still there
    *
    */
    public void testQueueBrowserMsgsRemainOnQueue() throws Exception
    {
        int messages = 10;

        sendMessages(messages);

        checkQueueDepth(messages);

        validate(messages);
    }


    public void testClosingBrowserMidReceiving() throws NamingException, JMSException
    {
        int messages = 100;

        sendMessages(messages);

        checkQueueDepth(messages);

        closeBrowserBeforeAfterGetNext(10);

        validate(messages);

    }

    /**
     * This tests that multiple getEnumerations on a QueueBrowser return the required number of messages.
     * @throws NamingException
     * @throws JMSException
     */
    public void testMultipleGetEnum() throws NamingException, JMSException
    {
        int messages = 10;

        sendMessages(messages);

        checkQueueDepth(messages);

        checkMultipleGetEnum(messages, 5);

        validate(messages);
    }

    public void testMultipleOverlappingGetEnum() throws NamingException, JMSException
    {
        int messages = 25;

        sendMessages(messages);

        checkQueueDepth(messages);

        checkOverlappingMultipleGetEnum(messages, 5);

        validate(messages);
    }


    public void testBrowsingWithSelector() throws JMSException
    {
        int messages = 40;

        sendMessages(messages);

        checkQueueDepth(messages);

        for (int clients = 2; clients <= 10; clients++)
        {
            checkQueueDepthWithSelectors(messages, clients);
        }

        validate(messages);
    }

    /**
     * Testing that a QueueBrowser doesn't actually consume messages from a broker when it fails over.
     * @throws JMSException
     */
    public void testFailoverWithQueueBrowser() throws JMSException
    {
        int messages = 5;


        sendMessages("connection1", messages);
        if (!CLUSTERED)
        {
            sendMessages("connection2", messages);
        }


        checkQueueDepth(messages);


        _logger.info("Creating Queue Browser");

        QueueBrowser queueBrowser = _clientSession.createBrowser(_queue);

        long queueDepth = 0;

        try
        {
            queueDepth = ((AMQSession) _clientSession).getQueueDepth((AMQDestination) _queue);
        }
        catch (AMQException e)
        {
        }

        assertEquals("Session reports Queue depth not as expected", messages, queueDepth);


        int msgCount = 0;

        int failPoint = 0;

        failPoint = new Random().nextInt(messages) + 1;

        Enumeration msgs = queueBrowser.getEnumeration();

        while (msgs.hasMoreElements())
        {
            msgs.nextElement();
            msgCount++;

            if (msgCount == failPoint)
            {
                failBroker(getFailingPort());
            }
        }

        assertTrue("We should get atleast " + messages + " msgs.", msgCount >= messages);

        if (_logger.isDebugEnabled())
        {
            _logger.debug("QBAAT Found " + msgCount + " messages total in browser");
        }

        //Close browser
        queueBrowser.close();

        _logger.info("Closed Queue Browser, validating messages on broker.");

        //Validate all messages still on Broker
        validate(messages);
    }

    public void testFailoverAsQueueBrowserCreated() throws JMSException
    {
        // The IoServiceListenerSupport seems to get stuck in with a managedSession that isn't closing when requested.
        // So it hangs waiting for the session.
        int messages = 50;

        sendMessages("connection1", messages);
        if (!CLUSTERED)
        {
            sendMessages("connection2", messages);
        }

        failBroker(getFailingPort());

        checkQueueDepth(messages);

        //Validate all messages still on Broker 1
        validate(messages);
    }

}
