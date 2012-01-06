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
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TransactionRolledBackException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 *
 */
public class AcknowledgeAfterFailoverTest extends AcknowledgeTest implements ConnectionListener
{

    protected CountDownLatch _failoverCompleted = new CountDownLatch(1);

    @Override
    public void setUp() throws Exception
    {
        super.setUp();
        // This must be even for the test to run correctly.
        // Otherwise we will kill the standby broker
        // not the one we are connected to.
        // The test will still pass but it will not be exactly
        // as described.
        NUM_MESSAGES = 6;
    }

    /**
     * Override default init to add connectionListener so we can verify that
     * failover took place
     *
     * @param transacted create a transacted session for this test
     * @param mode if not transacted what ack mode to use for this test
     * @throws Exception if a problem occured during test setup.
     */                                                                                                               
    @Override
    protected void init(boolean transacted, int mode) throws Exception
    {
        super.init(transacted, mode);
        ((AMQConnection) _connection).setConnectionListener(this);
    }

    protected void prepBroker(int count) throws Exception
    {
        if (count % 2 == 1)
        {
            failBroker(getFailingPort());
        }
        else
        {
            failBroker(getPort());
        }

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

        try
        {
            if (count % 2 == 1)
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

    @Override
    public void doAcknowlegement(Message msg) throws JMSException
    {
        //Acknowledge current message
        super.doAcknowlegement(msg);

        try
        {
            prepBroker(NUM_MESSAGES - msg.getIntProperty(INDEX) - 1);
        }
        catch (Exception e)
        {
            fail("Unable to prep new broker," + e.getMessage());
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
        //Test Dirty Failover Fails
        init(transacted, mode);

        _connection.start();

        Message msg = _consumer.receive(1500);

        int count = 0;
        assertNotNull("Message " + count + " not correctly received.", msg);
        assertEquals("Incorrect message received", count, msg.getIntProperty(INDEX));

        //Don't acknowledge just prep the next broker. Without changing count
        // Prep the new broker to have all all the messages so we can validate
        // that they can all be correctly received.
        try
        {

            //Stop the connection so we can validate the number of message count
            // on the queue is correct after failover
            _connection.stop();
            failBroker(getFailingPort());

            //Get the connection to the first (main port) broker.
            Connection connection = getConnection();//getConnectionFactory("connection1").getConnectionURL());
            // Use a transaction to send messages so we can be sure they arrive.
            Session session = connection.createSession(true, Session.SESSION_TRANSACTED);
            // ensure destination is created.
            session.createConsumer(_queue).close();

            sendMessage(session, _queue, NUM_MESSAGES);

            assertEquals("Wrong number of messages on queue", NUM_MESSAGES,
                         ((AMQSession) session).getQueueDepth((AMQDestination) _queue));

            connection.close();

            //restart connection
            _connection.start();
        }
        catch (Exception e)
        {
            fail("Unable to prep new broker," + e.getMessage());
        }

        // Consume the next message - don't check what it is as a normal would
        // assume it is msg 1 but as we've fallen over it is msg 0 again.
        msg = _consumer.receive(1500);

        if (_consumerSession.getTransacted())
        {
            try
            {
                _consumerSession.commit();
                fail("Session is dirty we should get an TransactionRolledBackException");
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
                msg.acknowledge();
                fail("Session is dirty we should get an IllegalStateException");
            }
            catch (javax.jms.IllegalStateException ise)
            {
                assertEquals("Incorrect Exception thrown", "has failed over", ise.getMessage());
                // Recover the sesion and try again.
                _consumerSession.recover();
            }
        }

        msg = _consumer.receive(1500);
        // Validate we now get the first message back
        assertEquals(0, msg.getIntProperty(INDEX));

        msg = _consumer.receive(1500);
        // and the second message
        assertEquals(1, msg.getIntProperty(INDEX));

        // And now verify that we can now commit the clean session
        if (_consumerSession.getTransacted())
        {
            _consumerSession.commit();
        }
        else
        {
            msg.acknowledge();
        }

        assertEquals("Wrong number of messages on queue", 0,
                     ((AMQSession) _consumerSession).getQueueDepth((AMQDestination) _queue));
    }

    public void testDirtyClientAck() throws Exception
    {
        testDirtyAcking(false, Session.CLIENT_ACKNOWLEDGE);
    }

    public void testDirtyAckingTransacted() throws Exception
    {
        testDirtyAcking(true, Session.SESSION_TRANSACTED);
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
                fail("Failover did not occur in specified time:" + DEFAULT_FAILOVER_TIME);
            }
        }
        catch (InterruptedException e)
        {
            fail("Failover was interrupted");
        }
    }

}
