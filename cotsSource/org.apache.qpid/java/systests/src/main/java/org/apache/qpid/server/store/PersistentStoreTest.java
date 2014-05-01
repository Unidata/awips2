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

package org.apache.qpid.server.store;

import org.apache.qpid.test.utils.QpidTestCase;

import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import java.util.ArrayList;
import java.util.List;

public class PersistentStoreTest extends QpidTestCase
{

    private static final int NUM_MESSAGES = 100;
    private Connection _con;
    private Session _session;
    private Queue _destination;
    private MessageConsumer _consumer;

    public void setUp() throws Exception, JMSException
    {
        super.setUp();
        _con = getConnection();
        _con.start();
        _session = _con.createSession(true, Session.AUTO_ACKNOWLEDGE);
        _destination = _session.createQueue(getTestQueueName());
        _consumer = _session.createConsumer(_destination);
        _consumer.close();

        sendMessage(_session, _destination, NUM_MESSAGES);
        _session.commit();
    }

    /** Checks that a new consumer on a new connection can get NUM_MESSAGES from _destination */
    private void checkMessages() throws Exception, JMSException
    {
        _con = getConnection();
        _session = _con.createSession(false, Session.AUTO_ACKNOWLEDGE);
        _con.start();
        _consumer = _session.createConsumer(_destination);
        for (int i = 0; i < NUM_MESSAGES; i++)
        {
            Message msg = _consumer.receive(RECEIVE_TIMEOUT);
            assertNotNull("Message " + i + " not received", msg);
        }
        assertNull("No more messages should be received", _consumer.receive(100));
    }

//    /**
//     * starts the server, sends 100 messages, restarts the server and gets 100 messages back
//     * the test formerly referred to as BDB-Qpid-1
//     * @throws Exception
//     */
//    public void testStartStop() throws Exception
//    {
//        restartBroker(); -- Not Currently a gracefull restart so not BDB-Qpid-1
//        checkMessages();
//    }

    /**
     * starts the server, sends 100 messages, nukes then starts the server and gets 100 messages back
     * the test formerly referred to as BDB-Qpid-2
     *
     * @throws Exception
     */
    public void testForcibleStartStop() throws Exception
    {
        restartBroker();
        checkMessages();
    }

//    /**
//     * starts the server, sends 100 committed messages, 5 uncommited ones,
//     * restarts the server and gets 100 messages back
//     * the test formerly referred to as BDB-Qpid-5
//     * @throws Exception
//     */
//    public void testStartStopMidTransaction() throws Exception
//    {
//        sendMessage(_session, _destination, 5);
//        restartBroker(); -- Not Currently a gracefull restart so not BDB-Qpid-1
//        checkMessages();
//    }

    /**
     * starts the server, sends 100 committed messages, 5 uncommited ones,
     * nukes and starts the server and gets 100 messages back
     * the test formerly referred to as BDB-Qpid-6
     *
     * @throws Exception
     */
    public void testForcibleStartStopMidTransaction() throws Exception
    {
        sendMessage(_session, _destination, 5);
        restartBroker();
        checkMessages();
    }

    /**
     * starts the server, sends 100 committed messages, 5 uncommited ones,
     * restarts the client and gets 100 messages back.
     * the test formerly referred to as BDB-Qpid-7
     *
     * FIXME: is this a PersistentStoreTest? Seems more like a transaction test to me.. aidan
     *
     * @throws Exception
     */
    public void testClientDeathMidTransaction() throws Exception
    {
        sendMessage(_session, _destination, 5);
        _con.close();
        checkMessages();
    }

//    /**
//     * starts the server, sends 50 committed messages, copies $QPID_WORK to a new location,
//     * sends 10 messages, stops the server, nukes the store, restores the copy, starts the server
//     * checks that we get the first 50 back.
//     */
//    public void testHotBackup()
//    {
//        -- removing as this will leave 100msgs on a queue
//    }

    /**
     * This test requires that we can send messages without commiting.
     * QTC always commits the messages sent via sendMessages.
     *
     * @param session the session to use for sending
     * @param destination where to send them to
     * @param count no. of messages to send
     *
     * @return the sent messges
     *
     * @throws Exception
     */
    @Override
    public List<Message> sendMessage(Session session, Destination destination,
                                     int count) throws Exception
    {
        List<Message> messages = new ArrayList<Message>(count);

        MessageProducer producer = session.createProducer(destination);

        for (int i = 0;i < (count); i++)
        {
            Message next = createNextMessage(session, i);

            producer.send(next);

            messages.add(next);
        }

        return messages;
    }

}
