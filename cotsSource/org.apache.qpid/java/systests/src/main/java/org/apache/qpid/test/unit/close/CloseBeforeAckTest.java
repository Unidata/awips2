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
package org.apache.qpid.test.unit.close;

import junit.framework.Assert;

import org.apache.qpid.test.utils.QpidTestCase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.apache.qpid.junit.concurrency.TestRunnable;
import org.apache.qpid.junit.concurrency.ThreadTestCoordinator;

import javax.jms.Connection;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Session;

/**
 * This test forces the situation where a session is closed whilst a message consumer is still in its onMessage method.
 * Running in AUTO_ACK mode, the close call ought to wait until the onMessage method completes, and the ack is sent
 * before closing the connection.
 *
 * <p><table id="crc"><caption>CRC Card</caption> <tr><th> Responsibilities <th> Collaborations <tr><td> Check that
 * closing a connection whilst handling a message, blocks till completion of the handler. </table>
 */
public class CloseBeforeAckTest extends QpidTestCase
{
    private static final Logger log = LoggerFactory.getLogger(CloseBeforeAckTest.class);

    Connection connection;
    Session session;
    public static final String TEST_QUEUE_NAME = "TestQueue";
    private int TEST_COUNT = 25;

    class TestThread1 extends TestRunnable implements MessageListener
    {
        public void runWithExceptions() throws Exception
        {
            // Set this up to listen for message on the test session.
            session.createConsumer(session.createQueue(TEST_QUEUE_NAME)).setMessageListener(this);
        }

        public void onMessage(Message message)
        {
            // Give thread 2 permission to close the session.
            allow(new int[] { 1 });

            // Wait until thread 2 has closed the connection, or is blocked waiting for this to complete.
            waitFor(new int[] { 1 }, true);
        }
    }

    TestThread1 testThread1 = new TestThread1();

    TestRunnable testThread2 =
        new TestRunnable()
        {
            public void runWithExceptions() throws Exception
            {
                // Send a message to be picked up by thread 1.
                session.createProducer(null).send(session.createQueue(TEST_QUEUE_NAME),
                    session.createTextMessage("Hi there thread 1!"));

                // Wait for thread 1 to pick up the message and give permission to continue.
                waitFor(new int[] { 0 }, false);

                // Close the connection.
                session.close();

                // Allow thread 1 to continue to completion, if it is erronously still waiting.
                allow(new int[] { 1 });
            }
        };

    public void testCloseBeforeAutoAck_QPID_397() throws Exception
    {
        // Create a session in auto acknowledge mode. This problem shows up in auto acknowledge if the client acks
        // message at the end of the onMessage method, after a close has been sent.
        session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        ThreadTestCoordinator tt = new ThreadTestCoordinator(2);

        tt.addTestThread(testThread1, 0);
        tt.addTestThread(testThread2, 1);
        tt.setDeadlockTimeout(500);
        tt.run();

        String errorMessage = tt.joinAndRetrieveMessages();

        // Print any error messages or exceptions.
        log.debug(errorMessage);

        if (!tt.getExceptions().isEmpty())
        {
            for (Exception e : tt.getExceptions())
            {
                log.debug("Exception thrown during test thread: ", e);
            }
        }

        Assert.assertTrue(errorMessage, "".equals(errorMessage));
    }

    public void closeBeforeAutoAckManyTimes() throws Exception
    {
        for (int i = 0; i < TEST_COUNT; i++)
        {
            testCloseBeforeAutoAck_QPID_397();
        }
    }

    protected void setUp() throws Exception
    {
        super.setUp();
        connection =  getConnection("guest", "guest");
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }
}
