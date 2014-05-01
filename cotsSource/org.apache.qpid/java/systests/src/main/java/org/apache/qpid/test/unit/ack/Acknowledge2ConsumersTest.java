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

import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.test.utils.FailoverBaseCase;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.Queue;
import javax.jms.Session;

public class Acknowledge2ConsumersTest extends FailoverBaseCase
{
    protected static int NUM_MESSAGES = 100;
    protected Connection _con;
    protected Queue _queue;
    private Session _producerSession;
    private Session _consumerSession;
    private MessageConsumer _consumerA;

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();

        _queue = (Queue) getInitialContext().lookup("queue");

        //Create Producer put some messages on the queue
        _con = getConnection();
    }

    private void init(boolean transacted, int mode) throws JMSException
    {
        _producerSession = _con.createSession(true, Session.SESSION_TRANSACTED);
        _consumerSession = _con.createSession(transacted, mode);
        _consumerA = _consumerSession.createConsumer(_queue);
        _con.start();
    }

    /**
     * Produces Messages that
     *
     * @param transacted
     * @param mode
     *
     * @throws Exception
     */
    private void test2ConsumersAcking(boolean transacted, int mode) throws Exception
    {
        init(transacted, mode);

        // These should all end up being prefetched by sessionA
        sendMessage(_producerSession, _queue, NUM_MESSAGES / 2);

        //Create a second consumer (consumerB) to consume some of the messages
        MessageConsumer consumerB = _consumerSession.createConsumer(_queue);

        // These messages should be roundrobined between A and B
        sendMessage(_producerSession, _queue, NUM_MESSAGES / 2);

        int count = 0;
        //Use consumerB to receive messages it has
        Message msg = consumerB.receive(1500);
        while (msg != null)
        {
            if (mode == Session.CLIENT_ACKNOWLEDGE)
            {
                msg.acknowledge();
            }
            count++;
            msg = consumerB.receive(1500);
        }
        if (transacted)
        {
            _consumerSession.commit();
        }

        // Close the consumers
        _consumerA.close();
        consumerB.close();

        // and close the session to release any prefetched messages.
        _consumerSession.close();
        assertEquals("Wrong number of messages on queue", NUM_MESSAGES - count,
                     ((AMQSession) _producerSession).getQueueDepth((AMQDestination) _queue));

        // Clean up messages that may be left on the queue
        _consumerSession = _con.createSession(transacted, mode);
        _consumerA = _consumerSession.createConsumer(_queue);
        msg = _consumerA.receive(1500);
        while (msg != null)
        {
            if (mode == Session.CLIENT_ACKNOWLEDGE)
            {
                msg.acknowledge();
            }
            msg = _consumerA.receive(1500);
        }
        _consumerA.close();
        if (transacted)
        {
            _consumerSession.commit();
        }
        _consumerSession.close();
    }

    public void test2ConsumersAutoAck() throws Exception
    {
        test2ConsumersAcking(false, Session.AUTO_ACKNOWLEDGE);
    }

    public void test2ConsumersClientAck() throws Exception
    {
    	test2ConsumersAcking(false, Session.CLIENT_ACKNOWLEDGE);
    }

    public void test2ConsumersTx() throws Exception
    {
    	test2ConsumersAcking(true, Session.SESSION_TRANSACTED);
    }



//
//    /**
//     * Check that session level acknowledge does correctly ack all previous
//     * values. Send 3 messages(0,1,2) then ack 1 and 2. If session ack is
//     * working correctly then acking 1 will also ack 0. Acking 2 will not
//     * attempt to re-ack 0 and 1.
//     *
//     * @throws Exception
//     */
//    public void testSessionAck() throws Exception
//    {
//        init(false, Session.CLIENT_ACKNOWLEDGE);
//
//        sendMessage(_producerSession, _queue, 3);
//        Message msg;
//
//        // Drop msg 0
//        _consumerA.receive(RECEIVE_TIMEOUT);
//
//        // Take msg 1
//        msg = _consumerA.receive(RECEIVE_TIMEOUT);
//
//        assertNotNull("Message 1 not correctly received.", msg);
//        assertEquals("Incorrect message received", 1, msg.getIntProperty(INDEX));
//
//        // This should also ack msg 0
//        msg.acknowledge();
//
//        // Take msg 2
//        msg = _consumerA.receive(RECEIVE_TIMEOUT);
//
//        assertNotNull("Message 2 not correctly received.", msg);
//        assertEquals("Incorrect message received", 2, msg.getIntProperty(INDEX));
//
//        // This should just ack msg 2
//        msg.acknowledge();
//
//        _consumerA.close();
//        _consumerSession.close();
//
//        assertEquals("Queue not empty.", 0,
//                     ((AMQSession) _producerSession).getQueueDepth((AMQDestination) _queue));
//        _con.close();
//
//
//    }
}
