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
package org.apache.qpid.test.unit.ack;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.jms.ConnectionListener;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Session;
import javax.jms.TransactionRolledBackException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class AcknowledgeAfterFailoverOnMessageTest extends AcknowledgeOnMessageTest implements ConnectionListener
{

    protected CountDownLatch _failoverCompleted = new CountDownLatch(1);
    private MessageListener _listener = null;

    @Override
    public void setUp() throws Exception
    {
        super.setUp();
        NUM_MESSAGES = 10;
    }

    /**
     * Override default init to add connectionListener so we can verify that
     * failover took place
     *
     * @param transacted create a transacted session for this test
     * @param mode       if not transacted what ack mode to use for this test
     *
     * @throws Exception if a problem occured during test setup.
     */
    @Override
    public void init(boolean transacted, int mode) throws Exception
    {
        super.init(transacted, mode);
        ((AMQConnection) _connection).setConnectionListener(this);
        // Override the listener for the dirtyAck testing.
        if (_listener != null)
        {
            _consumer.setMessageListener(_listener);
        }
    }

    protected void prepBroker(int count) throws Exception
    {
        //Stop the connection whilst we repopulate the broker, or the no_ack
        // test will drain the msgs before we can check we put the right number
        // back on again.
//        _connection.stop();

        Connection connection = getConnection();
        Session session = connection.createSession(true, Session.SESSION_TRANSACTED);
        // ensure destination is created.
        session.createConsumer(_queue).close();

        sendMessage(session, _queue, count, NUM_MESSAGES - count, 0);

        if (_consumerSession.getAcknowledgeMode() != AMQSession.NO_ACKNOWLEDGE)
        {
            assertEquals("Wrong number of messages on queue", count,
                         ((AMQSession) session).getQueueDepth((AMQDestination) _queue));
        }

        connection.close();

//        _connection.start();
    }

    @Override
    public void doAcknowlegement(Message msg) throws JMSException
    {
        //Acknowledge current message
        super.doAcknowlegement(msg);

        int msgCount = msg.getIntProperty(INDEX);

        if (msgCount % 2 == 0)
        {
            failBroker(getFailingPort());
        }
        else
        {
            failBroker(getPort());
        }

        try
        {
            prepBroker(NUM_MESSAGES - msgCount - 1);
        }
        catch (Exception e)
        {
            fail("Unable to prep new broker," + e.getMessage());
        }

        try
        {

            if (msgCount % 2 == 0)
            {
                startBroker(getFailingPort());
            }
            else
            {
                startBroker(getPort());
            }
        }
        catch (Exception e)
        {
            fail("Unable to start failover broker," + e.getMessage());
        }

    }

    int msgCount = 0;
    boolean cleaned = false;

    class DirtyAckingHandler implements MessageListener
    {
        /**
         * Validate first message but do nothing with it.
         *
         * Failover
         *
         * The receive the message again
         *
         * @param message
         */
        public void onMessage(Message message)
        {
            // Stop processing if we have an error and had to stop running.
            if (_receivedAll.getCount() == 0)
            {
                _logger.debug("Dumping msgs due to error(" + _causeOfFailure.get().getMessage() + "):" + message);
                return;
            }

            try
            {
                // Check we have the next message as expected
                assertNotNull("Message " + msgCount + " not correctly received.", message);
                assertEquals("Incorrect message received", msgCount, message.getIntProperty(INDEX));

                if (msgCount == 0 && _failoverCompleted.getCount() != 0)
                {
                    // This is the first message we've received so lets fail the broker

                    failBroker(getFailingPort());

                    repopulateBroker();

                    _logger.error("Received first msg so failing over");

                    return;
                }

                msgCount++;

                // Don't acknowlege the first message after failover so we can commit
                // them together
                if (msgCount == 1)
                {
                    _logger.error("Received first msg after failover ignoring:" + msgCount);

                    // Acknowledge the first message if we are now on the cleaned pass
                    if (cleaned)
                    {
                        _receivedAll.countDown();
                    }

                    return;
                }

                if (_consumerSession.getTransacted())
                {
                    try
                    {
                        _consumerSession.commit();
                        if (!cleaned)
                        {
                            fail("Session is dirty we should get an TransactionRolledBackException");
                        }
                    }
                    catch (TransactionRolledBackException trbe)
                    {
                        //expected path
                    }
                }
                else
                {
                    try
                    {
                        message.acknowledge();
                        if (!cleaned)
                        {
                            fail("Session is dirty we should get an IllegalStateException");
                        }
                    }
                    catch (javax.jms.IllegalStateException ise)
                    {
                        assertEquals("Incorrect Exception thrown", "has failed over", ise.getMessage());
                        // Recover the sesion and try again.
                        _consumerSession.recover();
                    }
                }

                // Acknowledge the last message if we are in a clean state
                // this will then trigger test teardown.
                if (cleaned)
                {
                    _receivedAll.countDown();
                }

                //Reset message count so we can try again.
                msgCount = 0;
                cleaned = true;
            }
            catch (Exception e)
            {
                // If something goes wrong stop and notifiy main thread.
                fail(e);
            }
        }
    }

    /**
     * Test that Acking/Committing a message received before failover causes
     * an exception at commit/ack time.
     *
     * Expected behaviour is that in:
     * * tx mode commit() throws a transacted RolledBackException
     * * client ack mode throws an IllegalStateException
     *
     * @param transacted is this session trasacted
     * @param mode       What ack mode should be used if not trasacted
     *
     * @throws Exception if something goes wrong.
     */
    protected void testDirtyAcking(boolean transacted, int mode) throws Exception
    {
        NUM_MESSAGES = 2;
        _listener = new DirtyAckingHandler();

        super.testAcking(transacted, mode);
    }

    public void testDirtyClientAck() throws Exception
    {
        testDirtyAcking(false, Session.CLIENT_ACKNOWLEDGE);
    }

    public void testDirtyAckingTransacted() throws Exception
    {
        testDirtyAcking(true, Session.SESSION_TRANSACTED);
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
     * @param port
     */
    @Override
    public void failBroker(int port)
    {
        super.failBroker(port);

        try
        {
            if (!_failoverCompleted.await(DEFAULT_FAILOVER_TIME, TimeUnit.MILLISECONDS))
            {
                // Use an exception so that we use our local fail() that notifies the main thread of failure
                throw new Exception("Failover did not occur in specified time:" + DEFAULT_FAILOVER_TIME);
            }

        }
        catch (Exception e)
        {
            // Use an exception so that we use our local fail() that notifies the main thread of failure
            fail(e);
        }
    }

}
