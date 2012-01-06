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

import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.client.AMQConnection;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.Queue;
import javax.jms.Session;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * Test DeapQueueConsumerWithSelector
 * Summary:
 * Prior to M4 the broker had a different queue model which pre-processed the
 * messages on the queue for any connecting subscription that had a selector.
 *
 * If the queue had a lot of data then this may take a long time to process
 * to such an extent that the subscription creation may time out. During this
 * pre-process phase the virtualhost would be come unresposive.
 *
 * Our solution was to allow the timeout to be adjusted QPID-1119, which allowed
 * the subscription to connect but did not address the unresponsiveness.
 *
 * The new queue model introduced in M4 resolved this.
 *
 * This test is to validate that the new queueing model does indeed remove the
 * long pre-processing phase and allow immediate subscription so that there is
 * no unresponsive period.
 *
 * Test Strategy:
 *
 * Add 100k messages to the queue with a numberic header property that will
 * allow later subscribers to use as in a selector.
 *
 * Connect the subscriber and time how long it takes to connect.
 *
 * Finally consume all the messages from the queue to clean up.
 */
public class DeepQueueConsumeWithSelector extends QpidTestCase implements MessageListener
{

    private static final int MESSAGE_COUNT = 10000;
    private static final int BATCH_SIZE = MESSAGE_COUNT / 10;

    private CountDownLatch _receviedLatch = new CountDownLatch(MESSAGE_COUNT);

    protected long SYNC_WRITE_TIMEOUT = 120000L;


    public void setUp() throws Exception
    {
        //Set the syncWrite timeout to be just larger than the delay on the commitTran.
        setSystemProperty("amqj.default_syncwrite_timeout", String.valueOf(SYNC_WRITE_TIMEOUT));

        super.setUp();
    }

    public void test() throws Exception
    {
        // Create Connection
        Connection connection = getConnection();
        Session session = ((AMQConnection)connection).createSession(true, Session.SESSION_TRANSACTED, 100000);

        Queue queue = (Queue) getInitialContext().lookup("queue");

        // Validate that the destination exists
        session.createConsumer(queue).close();

        // Send Messages
        sendMessage(session, queue, MESSAGE_COUNT, BATCH_SIZE);

        session.close();

        session = ((AMQConnection) connection).createSession(false, Session.AUTO_ACKNOWLEDGE);//, 100000);


        // Setup Selector to perform a few calculations which will slow it down
        String selector = "((\"" + INDEX + "\" % 1) = 0) AND ('" + INDEX + "' IS NOT NULL) AND ('" + INDEX + "' <> -1)";

        // Setup timing
        long start = System.nanoTime();

        System.err.println("Create Consumer");
        // Connect Consumer
        MessageConsumer consumer = session.createConsumer(queue, selector);
        consumer.setMessageListener(this);

        // Validate timing details
        long end = System.nanoTime();

        System.err.println("Subscription time took:" + (end - start));

        // Consume Messages
        connection.start();



        assertTrue("Messages took to long to be received :"+_receviedLatch.getCount(),
                   _receviedLatch.await(SYNC_WRITE_TIMEOUT, TimeUnit.MILLISECONDS   ));

    }

    @Override
    public Message createNextMessage(Session session, int msgCount) throws JMSException
    {
        Message message = super.createNextMessage(session,msgCount);

        if ((msgCount % BATCH_SIZE) == 0 )
        {
            System.err.println("Sent:"+msgCount);
        }

        return message;
    }

    public void onMessage(Message message)
    {
        _receviedLatch.countDown();
        int msgCount = 0;
        try
        {
            msgCount = message.getIntProperty(INDEX);
        }
        catch (JMSException e)
        {
            //ignore
        }
        if ((msgCount % BATCH_SIZE) == 0 )
        {
            System.err.println("Received:"+msgCount);            
        }

    }
}
