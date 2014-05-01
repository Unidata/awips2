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
package org.apache.qpid.server.failover;

import org.apache.mina.common.WriteTimeoutException;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.protocol.AMQProtocolSession;
import org.apache.qpid.jms.ConnectionListener;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.test.utils.FailoverBaseCase;
import org.apache.qpid.AMQConnectionClosedException;

import javax.jms.Destination;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * Test case based on user reported error.
 *
 * Summary:
 * A user has reported message loss from their application. On bouncing of
 * the broker the 'lost' messages are delivered to the broker.
 *
 * Note:
 * The client was using Spring so that may influence the situation.
 *
 * Issue:
 * The log files show 7 instances of the following which result in 7
 * missing messages.
 *
 * The client log files show:
 *
 * The broker log file show:
 *
 *
 * 7 missing messages have delivery tags 5-11. Which says that they are
 * sequentially the next message from the broker.
 *
 * The only way for the 'without a handler' log to occur is if the consumer
 * has been removed from the look up table of the dispatcher.
 * And the only way for the 'null message' log to occur on the broker is is
 * if the message does not exist in the unacked-map
 *
 * The consumer is only removed from the list during session
 * closure and failover.
 *
 * If the session was closed then the broker would requeue the unacked
 * messages so the potential exists to have an empty map but the broker
 * will not send a message out after the unacked map has been cleared.
 *
 * When failover occurs the _consumer map is cleared and the consumers are
 * resubscribed. This is down without first stopping any existing
 * dispatcher so there exists the potential to receive a message after
 * the _consumer map has been cleared which is how the 'without a handler'
 * log statement occurs.
 *
 * Scenario:
 *
 * Looking over logs the sequence that best fits the events is as follows:
 * - Something causes Mina to be delayed causing the WriteTimoutException.
 * - This exception is recevied by AMQProtocolHandler#exceptionCaught
 * - As the WriteTimeoutException is an IOException this will cause
 * sessionClosed to be called to start failover.
 * + This is potentially the issues here. All IOExceptions are treated
 * as connection failure events.
 * - Failover Runs
 * + Failover assumes that the previous connection has been closed.
 * + Failover binds the existing objects (AMQConnection/Session) to the
 * new connection objects.
 * - Everything is reported as being successfully failed over.
 * However, what is neglected is that the original connection has not
 * been closed.
 * + So what occurs is that the broker sends a message to the consumer on
 * the original connection, as it was not notified of the client
 * failing over.
 * As the client failover reuses the original AMQSession and Dispatcher
 * the new messages the broker sends to the old consumer arrives at the
 * client and is processed by the same AMQSession and Dispatcher.
 * However, as the failover process cleared the _consumer map and
 * resubscribe the consumers the Dispatcher does not recognise the
 * delivery tag and so logs the 'without a handler' message.
 * - The Dispatcher then attempts to reject the message, however,
 * + The AMQSession/Dispatcher pair have been swapped to using a new Mina
 * ProtocolSession as part of the failover process so the reject is
 * sent down the second connection. The broker receives the Reject
 * request but as the Message was sent on a different connection the
 * unacknowledgemap is empty and a 'message is null' log message
 * produced.
 *
 * Test Strategy:
 *
 * It should be easy to demonstrate if we can send an IOException to
 * AMQProtocolHandler#exceptionCaught and then try sending a message.
 *
 * The current unknowns here are the type of consumers that are in use.
 * If it was an exclusive queue(Durable Subscription) then why did the
 * resubscribe not fail.
 *
 * If it was not exclusive then why did the messages not round robin?
 */
public class MessageDisappearWithIOExceptionTest extends FailoverBaseCase implements ConnectionListener
{
    private CountDownLatch _failoverOccured = new CountDownLatch(1);
    AMQConnection _connection;
    Session _session;
    Queue _queue;
    MessageConsumer _consumer;

    public void setUp() throws Exception
    {
        super.setUp();
        stopBroker(getFailingPort());

    }

    /**
     * Test Summary:
     *
     * Create a queue consumer and send 10 messages to the broker.
     *
     * Consume the first message.
     * This will pull the rest into the prefetch
     *
     * Send an IOException to the MinaProtocolHandler.
     *
     * This will force failover to occur.
     *
     * 9 messages would normally be expected but it is expected that none will
     * arrive. As they are still in the prefetch of the first session.
     *
     * To free the messages we need to close all connections.
     * - Simply doing connection.close() and retesting will not be enough as
     * the original connection's IO layer will still exist and is nolonger
     * connected to the connection object as a result of failover.
     *
     * - Test will need to retain a reference to the original connection IO so
     * that it can be closed releasing the messages to validate that the
     * messages have indeed been 'lost' on that sesssion.
     */
    public void test() throws Exception
    {
        initialiseConnection();

        // Create Producer
        // Send 10 messages
        List<Message> messages = sendNumberedBytesMessage(_session, _queue, 10);

        // Consume first messasge
        Message received = _consumer.receive(2000);

        // Verify received messages
        assertNotNull("First message not received.", received);
        assertEquals("Incorrect message Received",
                     messages.remove(0).getIntProperty("count"),
                     received.getIntProperty("count"));

        // When the Exception is received by the underlying IO layer it will
        // initiate failover. The first step of which is to ensure that the
        // existing conection is closed. So in this situation the connection
        // will be flushed casuing the above ACK to be sent to the broker.
        //
        // That said:
        // when the socket close is detected on the server it will rise up the
        // Mina filter chain and interrupt processing.
        // this has been raised as QPID-2138
        _session.createConsumer(_session.createTemporaryQueue()).close();

        //Retain IO Layer
        AMQProtocolSession protocolSession = _connection.getProtocolHandler().getProtocolSession();

        // Send IO Exception - causing failover
        _connection.getProtocolHandler().
                exception(new WriteTimeoutException("WriteTimeoutException to cause failover."));

        // Verify Failover occured through ConnectionListener
        assertTrue("Failover did not occur",
                   _failoverOccured.await(4000, TimeUnit.MILLISECONDS));

        /***********************************/
        // This verifies that the bug has been resolved

        // Attempt to consume again. Expect 9 messages
        for (int count = 1; count < 10; count++)
        {
            received = _consumer.receive(2000);
            assertNotNull("Expected message not received:" + count, received);
            assertEquals(messages.remove(0).getIntProperty("count"),
                         received.getIntProperty("count"));
        }

        //Verify there are no more messages
        received = _consumer.receive(1000);
        assertNull("Message receieved when there should be none:" + received,
                   received);

//        /***********************************/
//        // This verifies that the bug exists
//
//        // Attempt to consume remaining 9 messages.. Expecting NONE.
//        // receiving just one message should fail so no need to fail 9 times
//        received = _consumer.receive(1000);
//        assertNull("Message receieved when it should be null:" + received, received);
//
////        //Close the Connection which you would assume would free the messages
////        _connection.close();
////
////        // Reconnect
////        initialiseConnection();
////
////        // We should still be unable to receive messages
////        received = _consumer.receive(1000);
////        assertNull("Message receieved when it should be null:" + received, received);
////
////        _connection.close();
//
//        // Close original IO layer. Expecting messages to be released
//        protocolSession.closeProtocolSession();
//
//        // Reconnect and all should be good.
////        initialiseConnection();
//
//        // Attempt to consume again. Expect 9 messages
//        for (int count = 1; count < 10; count++)
//        {
//            received = _consumer.receive(2000);
//            assertNotNull("Expected message not received:" + count, received);
//            assertEquals(messages.remove(0).getIntProperty("count"),
//                         received.getIntProperty("count"));
//        }
//
//        //Verify there are no more messages
//        received = _consumer.receive(1000);
//        assertNull("Message receieved when there should be none:" + received,
//                   received);
    }

    private void initialiseConnection()
            throws Exception
    {
        //Create Connection using the default connection URL. i.e. not the Failover URL that would be used by default
        _connection = (AMQConnection) getConnection(getConnectionFactory("default").getConnectionURL());
        // The default connection does not have any retries configured so
        // Allow this connection to retry so that we can block on the failover.
        // The alternative would be to use the getConnection() default. However,
        // this would add additional complexity in the logging as a second
        // broker is defined in that url. We do not need it for this test.
        _connection.getFailoverPolicy().getCurrentMethod().setRetries(1);
        _connection.setConnectionListener(this);

        _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        _queue = _session.createQueue(getTestQueueName());

        // Create Consumer
        _consumer = _session.createConsumer(_queue);

        //Start connection
        _connection.start();
    }

    /** QpidTestCase back port to this release */

    // modified from QTC as sendMessage is not testable.
    //  - should be renamed sendBlankBytesMessage
    //  - should be renamed sendNumberedBytesMessage
    public List<Message> sendNumberedBytesMessage(Session session, Destination destination,
                                                  int count) throws Exception
    {
        List<Message> messages = new ArrayList<Message>(count);

        MessageProducer producer = session.createProducer(destination);

        for (int i = 0; i < count; i++)
        {
            Message next = session.createMessage();

            next.setIntProperty("count", i);

            producer.send(next);

            messages.add(next);
        }

        producer.close();
        return messages;
    }

    public void bytesSent(long count)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void bytesReceived(long count)
    {
    }

    public boolean preFailover(boolean redirect)
    {
        //Allow failover to occur
        return true;
    }

    public boolean preResubscribe()
    {
        //Allow failover to occur
        return true;
    }

    public void failoverComplete()
    {
        _failoverOccured.countDown();
    }
}
