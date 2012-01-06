package org.apache.qpid.test.client;

import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQSession;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

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

public class DupsOkTest extends QpidTestCase
{

    private Queue _queue;
    // Question why do we need to send so many messages?
    private static final int MSG_COUNT = 4999;
    private CountDownLatch _awaitCompletion = new CountDownLatch(1);

    public void setUp() throws Exception
    {
        super.setUp();

        _queue = (Queue)  getInitialContext().lookup("queue");


        //Declare the queue
        Connection consumerConnection = getConnection();
        consumerConnection.createSession(false,Session.AUTO_ACKNOWLEDGE).createConsumer(_queue).close();

        //Create Producer put some messages on the queue
        Connection producerConnection = getConnection();

        producerConnection.start();

        Session producerSession = producerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        MessageProducer producer = producerSession.createProducer(_queue);

        for (int count = 1; count <= MSG_COUNT; count++)
        {
            Message msg = producerSession.createTextMessage("Message " + count);
            msg.setIntProperty("count", count);
            producer.send(msg);
        }

        producerConnection.close();
    }

    /**
     * This test sends x messages and receives them with an async consumer.
     * Waits for all messages to be received or for 60 s
     * and checks whether the queue is empty.
     *
     * @throws Exception
     */
    public void testDupsOK() throws Exception
    {
        //Create Client
        Connection clientConnection = getConnection();

        final Session clientSession = clientConnection.createSession(false, Session.DUPS_OK_ACKNOWLEDGE);

        MessageConsumer consumer = clientSession.createConsumer(_queue);

        assertEquals("The queue should have msgs at start", MSG_COUNT, ((AMQSession) clientSession).getQueueDepth((AMQDestination) _queue));

        clientConnection.start();

        consumer.setMessageListener(new MessageListener()
        {
            int _msgCount = 0;

            public void onMessage(Message message)
            {
                _msgCount++;
                if (message == null)
                {
                    fail("Should not get null messages");
                }

                if (message instanceof TextMessage)
                {
                    try
                    {
                        if (message.getIntProperty("count") == MSG_COUNT)
                        {
                            try
                            {
                                if(_msgCount != MSG_COUNT)
                                {
                                    assertEquals("Wrong number of messages seen.", MSG_COUNT, _msgCount);
                                }
                            }
                            finally
                            {
                                //This is the last message so release test.
                                _awaitCompletion.countDown();
                            }
                        }
                    }
                    catch (JMSException e)
                    {
                        fail("Unable to get int property 'count'");
                    }
                }
                else
                {
                    fail("Got wrong message type");
                }
            }
        });

        try
        {
            if (!_awaitCompletion.await(120, TimeUnit.SECONDS))
            {
                fail("Test did not complete in 120 seconds");
            }
        }
        catch (InterruptedException e)
        {
            fail("Unable to wait for test completion");
            throw e;
        }

        //Close consumer to give broker time to process in bound Acks. As The main thread will be released while
        // before the dispatcher has sent the ack back to the broker.
        consumer.close();

        clientSession.close();

        final Session clientSession2 = clientConnection.createSession(false, Session.DUPS_OK_ACKNOWLEDGE);

        assertEquals("The queue should have 0 msgs left", 0, ((AMQSession) clientSession2).getQueueDepth((AMQDestination) _queue));

        clientConnection.close();
    }

}
