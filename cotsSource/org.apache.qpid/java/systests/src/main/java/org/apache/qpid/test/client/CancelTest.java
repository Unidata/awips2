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

package org.apache.qpid.test.client;

import org.apache.log4j.Logger;
import org.apache.qpid.test.utils.QpidTestCase;

import javax.jms.Connection;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.QueueBrowser;
import javax.jms.Session;
import java.util.Enumeration;

public class CancelTest extends QpidTestCase
{
    private static final Logger _logger = Logger.getLogger(CancelTest.class);

    private Connection _clientConnection;
    private Session _clientSession;
    private Queue _queue;

    public void setUp() throws Exception
    {

        super.setUp();

        _queue = (Queue) getInitialContext().lookup("queue");

        //Create Client
        _clientConnection = getConnection();

        _clientConnection.start();

        _clientSession = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        //Ensure _queue is created
        _clientSession.createConsumer(_queue).close();
    }

    /**
     * Simply
     * This test originally did not assert anything but was just checking
     * that a message could be browsed and consumed without throwing an exception.
     * It now checks that at least a message is browsed and that a message is received.
     */
    public void test() throws Exception
    {
        Connection producerConnection = getConnection();

        producerConnection.start();

        Session producerSession = producerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        MessageProducer producer = producerSession.createProducer(_queue);
        producer.send(producerSession.createTextMessage());
        producerConnection.close();


        QueueBrowser browser = _clientSession.createBrowser(_queue);
        Enumeration e = browser.getEnumeration();

        assertTrue(e.hasMoreElements());

        int i = 0;
        while (e.hasMoreElements())
        {
            e.nextElement();
            if(++i > 1)
            {
                fail("Two many elemnts to browse!");
            }
        }

        browser.close();

        MessageConsumer consumer = _clientSession.createConsumer(_queue);
        assertNotNull( consumer.receive() );
        consumer.close();
    }
}
