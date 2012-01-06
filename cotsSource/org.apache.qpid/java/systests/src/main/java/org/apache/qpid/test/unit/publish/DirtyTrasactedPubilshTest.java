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
package org.apache.qpid.test.unit.publish;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.jms.ConnectionListener;
import org.apache.qpid.test.utils.FailoverBaseCase;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TransactionRolledBackException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * QPID-1816 : Whilst testing Acknoledgement after failover this completes testing
 * of the client after failover. When we have a dirty session we should receive
 * an error if we attempt to publish. This test ensures that both in the synchronous
 * and asynchronous message delivery paths we receive the expected exceptions at
 * the expected time.
 */
public class DirtyTrasactedPubilshTest extends FailoverBaseCase implements ConnectionListener
{
    protected CountDownLatch _failoverCompleted = new CountDownLatch(1);

    protected int NUM_MESSAGES;
    protected Connection _connection;
    protected Queue _queue;
    protected Session _consumerSession;
    protected MessageConsumer _consumer;
    protected MessageProducer _producer;

    private static final String MSG = "MSG";
    private static final String SEND_FROM_ON_MESSAGE_TEXT = "sendFromOnMessage";
    protected CountDownLatch _receviedAll;
    protected AtomicReference<Exception> _causeOfFailure = new AtomicReference<Exception>(null);

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        NUM_MESSAGES = 10;

        _queue = getTestQueue();

        //Create Producer put some messages on the queue
        _connection = getConnection();
    }

    /**
     * Initialise the test variables
     * @param transacted is this a transacted test
     * @param mode if not trasacted then what ack mode to use
     * @throws Exception if there is a setup issue.
     */
    protected void init(boolean transacted, int mode) throws Exception
    {
        _consumerSession = _connection.createSession(transacted, mode);
        _consumer = _consumerSession.createConsumer(_queue);
        _producer = _consumerSession.createProducer(_queue);

        // These should all end up being prefetched by session
        sendMessage(_consumerSession, _queue, 1);

        assertEquals("Wrong number of messages on queue", 1,
                     ((AMQSession) _consumerSession).getQueueDepth((AMQDestination) _queue));
    }

    /**
     * If a transacted session has failed over whilst it has uncommitted sent
     * data then we need to throw a TransactedRolledbackException on commit()
     *
     * The alternative would be to maintain a replay buffer so that the message
     * could be resent. This is not currently implemented
     *
     * @throws Exception if something goes wrong.
     */
    public void testDirtySendingSynchronousTransacted() throws Exception
    {
        Session producerSession = _connection.createSession(true, Session.SESSION_TRANSACTED);

        // Ensure we get failover notifications
        ((AMQConnection) _connection).setConnectionListener(this);

        MessageProducer producer = producerSession.createProducer(_queue);

        // Create and send message 0
        Message msg = producerSession.createMessage();
        msg.setIntProperty(INDEX, 0);
        producer.send(msg);

        // DON'T commit message .. fail connection

        failBroker(getFailingPort());

        // Ensure destination exists for sending
        producerSession.createConsumer(_queue).close();

        // Send the next message
        msg.setIntProperty(INDEX, 1);
        try
        {
            producer.send(msg);
            fail("Should fail with Qpid as we provide early warning of the dirty session via a JMSException.");
        }
        catch (JMSException jmse)
        {
            assertEquals("Early warning of dirty session not correct",
                         "Failover has occurred and session is dirty so unable to send.", jmse.getMessage());
        }

        // Ignore that the session is dirty and attempt to commit to validate the
        // exception is thrown. AND that the above failure notification did NOT
        // clean up the session.

        try
        {
            producerSession.commit();
            fail("Session is dirty we should get an TransactionRolledBackException");
        }
        catch (TransactionRolledBackException trbe)
        {
            // Normal path.
        }

        // Resending of messages should now work ok as the commit was forcilbly rolledback
        msg.setIntProperty(INDEX, 0);
        producer.send(msg);
        msg.setIntProperty(INDEX, 1);
        producer.send(msg);

        producerSession.commit();

        assertEquals("Wrong number of messages on queue", 2,
                     ((AMQSession) producerSession).getQueueDepth((AMQDestination) _queue));
    }

    /**
     * If a transacted session has failed over whilst it has uncommitted sent
     * data then we need to throw a TransactedRolledbackException on commit()
     *
     * The alternative would be to maintain a replay buffer so that the message
     * could be resent. This is not currently implemented
     *
     * @throws Exception if something goes wrong.
     */
    public void testDirtySendingOnMessageTransacted() throws Exception
    {
        NUM_MESSAGES = 1;
        _receviedAll = new CountDownLatch(NUM_MESSAGES);
        ((AMQConnection) _connection).setConnectionListener(this);

        init(true, Session.SESSION_TRANSACTED);

        _consumer.setMessageListener(new MessageListener()
        {

            public void onMessage(Message message)
            {
                try
                {
                    // Create and send message 0
                    Message msg = _consumerSession.createMessage();
                    msg.setIntProperty(INDEX, 0);
                    _producer.send(msg);

                    // DON'T commit message .. fail connection

                    failBroker(getFailingPort());

                    // rep
                    repopulateBroker();

                    // Destination will exist as this failBroker will populate
                    // the queue with 1 message

                    // Send the next message
                    msg.setIntProperty(INDEX, 1);
                    try
                    {
                        _producer.send(msg);
                        fail("Should fail with Qpid as we provide early warning of the dirty session via a JMSException.");
                    }
                    catch (JMSException jmse)
                    {
                        assertEquals("Early warning of dirty session not correct",
                                     "Failover has occurred and session is dirty so unable to send.", jmse.getMessage());
                    }

                    // Ignore that the session is dirty and attempt to commit to validate the
                    // exception is thrown. AND that the above failure notification did NOT
                    // clean up the session.

                    try
                    {
                        _consumerSession.commit();
                        fail("Session is dirty we should get an TransactionRolledBackException");
                    }
                    catch (TransactionRolledBackException trbe)
                    {
                        // Normal path.
                    }

                    // Resend messages
                    msg.setIntProperty(INDEX, 0);
                    msg.setStringProperty(MSG, SEND_FROM_ON_MESSAGE_TEXT);
                    _producer.send(msg);
                    msg.setIntProperty(INDEX, 1);
                    msg.setStringProperty(MSG, SEND_FROM_ON_MESSAGE_TEXT);
                    _producer.send(msg);

                    _consumerSession.commit();

                    // Stop this consumer .. can't do _consumer.stop == DEADLOCK
                    // this doesn't seem to stop dispatcher running
                    _connection.stop();

                    // Signal that the onMessage send part of test is complete
                    // main thread can validate that messages are correct
                    _receviedAll.countDown();

                }
                catch (Exception e)
                {
                    fail(e);
                }

            }

        });

        _connection.start();

        if (!_receviedAll.await(10000L, TimeUnit.MILLISECONDS))
        {
            // Check to see if we ended due to an exception in the onMessage handler
            Exception cause = _causeOfFailure.get();
            if (cause != null)
            {
                cause.printStackTrace();
                fail(cause.getMessage());
            }
            else
            {
                fail("All messages not received:" + _receviedAll.getCount() + "/" + NUM_MESSAGES);
            }
        }

        // Check to see if we ended due to an exception in the onMessage handler
        Exception cause = _causeOfFailure.get();
        if (cause != null)
        {
            cause.printStackTrace();
            fail(cause.getMessage());
        }

        _consumer.close();
        _consumerSession.close();

        _consumerSession = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        _connection.start();

        // Validate that we could send the messages as expected.
        assertEquals("Wrong number of messages on queue", 3,
                     ((AMQSession) _consumerSession).getQueueDepth((AMQDestination) _queue));

        MessageConsumer consumer = _consumerSession.createConsumer(_queue);

        //Validate the message sent to setup the failed over broker.
        Message message = consumer.receive(1000);
        assertNotNull("Message " + 0 + " not received.", message);
        assertEquals("Incorrect message received", 0, message.getIntProperty(INDEX));

        // Validate the two messages sent from within the onMessage
        for (int index = 0; index <= 1; index++)
        {
            message = consumer.receive(1000);
            assertNotNull("Message " + index + " not received.", message);
            assertEquals("Incorrect message received", index, message.getIntProperty(INDEX));
            assertEquals("Incorrect message text for message:" + index, SEND_FROM_ON_MESSAGE_TEXT, message.getStringProperty(MSG));
        }

        assertNull("Extra message received.", consumer.receiveNoWait());

        _consumerSession.close();

        assertEquals("Wrong number of messages on queue", 0,
                     ((AMQSession) getConnection().createSession(false, Session.AUTO_ACKNOWLEDGE)).getQueueDepth((AMQDestination) _queue));
    }

    private void repopulateBroker() throws Exception
    {
        // Repopulate this new broker so we can test what happends after failover

        //Get the connection to the first (main port) broker.
        Connection connection = getConnection();
        // Use a transaction to send messages so we can be sure they arrive.
        Session session = connection.createSession(true, Session.SESSION_TRANSACTED);
        // ensure destination is created.
        session.createConsumer(_queue).close();

        sendMessage(session, _queue, NUM_MESSAGES);

        assertEquals("Wrong number of messages on queue", NUM_MESSAGES,
                     ((AMQSession) session).getQueueDepth((AMQDestination) _queue));

        connection.close();
    }

    // AMQConnectionListener Interface.. used so we can validate that we
    // actually failed over.

    public void bytesSent(long count)
    {
    }

    public void bytesReceived(long count)
    {
    }

    public boolean preFailover(boolean redirect)
    {
        //Allow failover
        return true;
    }

    public boolean preResubscribe()
    {
        //Allow failover
        return true;
    }

    public void failoverComplete()
    {
        _failoverCompleted.countDown();
    }

    /**
     * Override so we can block until failover has completd
     *
     * @param port int the port of the broker to fail.
     */
    @Override
    public void failBroker(int port)
    {
        super.failBroker(port);

        try
        {
            if (!_failoverCompleted.await(DEFAULT_FAILOVER_TIME, TimeUnit.MILLISECONDS))
            {
                fail("Failover did not occur in specified time:" + DEFAULT_FAILOVER_TIME);
            }
        }
        catch (InterruptedException e)
        {
            fail("Failover was interuppted");
        }
    }

    /**
     * Pass the given exception back to the waiting thread to fail the test run.
     *
     * @param e The exception that is causing the test to fail.
     */
    protected void fail(Exception e)
    {
        _causeOfFailure.set(e);
        // End the test.
        while (_receviedAll.getCount() != 0)
        {
            _receviedAll.countDown();
        }
    }
}
