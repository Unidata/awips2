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

import org.apache.qpid.client.AMQSession_0_8;
import org.apache.qpid.client.message.AbstractJMSMessage;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.log4j.Logger;

import javax.jms.*;

public class FlowControlTest extends QpidTestCase
{
    private static final Logger _logger = Logger.getLogger(FlowControlTest.class);

    private Connection _clientConnection;
    private Session _clientSession;
    private Queue _queue;

    /**
     * Simply
     *
     * @throws Exception
     */
    public void testBasicBytesFlowControl() throws Exception
    {
        _queue = (Queue) getInitialContext().lookup("queue");

        //Create Client
        _clientConnection = getConnection();

        _clientConnection.start();

        _clientSession = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        //Ensure _queue is created
        _clientSession.createConsumer(_queue).close();

        Connection producerConnection = getConnection();

        producerConnection.start();

        Session producerSession = producerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        MessageProducer producer = producerSession.createProducer(_queue);

        BytesMessage m1 = producerSession.createBytesMessage();
        m1.writeBytes(new byte[128]);
        m1.setIntProperty("msg", 1);
        producer.send(m1);
        BytesMessage m2 = producerSession.createBytesMessage();
        m2.writeBytes(new byte[128]);
        m2.setIntProperty("msg", 2);
        producer.send(m2);
        BytesMessage m3 = producerSession.createBytesMessage();
        m3.writeBytes(new byte[256]);
        m3.setIntProperty("msg", 3);
        producer.send(m3);

        producer.close();
        producerSession.close();
        producerConnection.close();

        Connection consumerConnection = getConnection();
        Session consumerSession = consumerConnection.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        ((AMQSession_0_8) consumerSession).setPrefetchLimits(0, 256);
        MessageConsumer recv = consumerSession.createConsumer(_queue);
        consumerConnection.start();

        Message r1 = recv.receive(RECEIVE_TIMEOUT);
        assertNotNull("First message not received", r1);
        assertEquals("Messages in wrong order", 1, r1.getIntProperty("msg"));

        Message r2 = recv.receive(RECEIVE_TIMEOUT);
        assertNotNull("Second message not received", r2);
        assertEquals("Messages in wrong order", 2, r2.getIntProperty("msg"));

        Message r3 = recv.receive(RECEIVE_TIMEOUT);
        assertNull("Third message incorrectly delivered", r3);

        ((AbstractJMSMessage)r1).acknowledgeThis();

        r3 = recv.receive(RECEIVE_TIMEOUT);
        assertNull("Third message incorrectly delivered", r3);

        ((AbstractJMSMessage)r2).acknowledgeThis();

        r3 = recv.receive(RECEIVE_TIMEOUT);
        assertNotNull("Third message not received", r3);
        assertEquals("Messages in wrong order", 3, r3.getIntProperty("msg"));

        ((AbstractJMSMessage)r3).acknowledgeThis();
        consumerConnection.close();
    }

    public void testTwoConsumersBytesFlowControl() throws Exception
    {
        _queue = (Queue) getInitialContext().lookup("queue");

        //Create Client
        _clientConnection = getConnection();

        _clientConnection.start();

        _clientSession = _clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        //Ensure _queue is created
        _clientSession.createConsumer(_queue).close();

        Connection producerConnection = getConnection();

        producerConnection.start();

        Session producerSession = producerConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        MessageProducer producer = producerSession.createProducer(_queue);

        BytesMessage m1 = producerSession.createBytesMessage();
        m1.writeBytes(new byte[128]);
        m1.setIntProperty("msg", 1);
        producer.send(m1);
        BytesMessage m2 = producerSession.createBytesMessage();
        m2.writeBytes(new byte[256]);
        m2.setIntProperty("msg", 2);
        producer.send(m2);
        BytesMessage m3 = producerSession.createBytesMessage();
        m3.writeBytes(new byte[128]);
        m3.setIntProperty("msg", 3);
        producer.send(m3);

        producer.close();
        producerSession.close();
        producerConnection.close();

        Connection consumerConnection = getConnection();
        Session consumerSession1 = consumerConnection.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        ((AMQSession_0_8) consumerSession1).setPrefetchLimits(0, 256);
        MessageConsumer recv1 = consumerSession1.createConsumer(_queue);

        consumerConnection.start();

        Message r1 = recv1.receive(RECEIVE_TIMEOUT);
        assertNotNull("First message not received", r1);
        assertEquals("Messages in wrong order", 1, r1.getIntProperty("msg"));

        Message r2 = recv1.receive(RECEIVE_TIMEOUT);
        assertNull("Second message incorrectly delivered", r2);

        Session consumerSession2 = consumerConnection.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        ((AMQSession_0_8) consumerSession2).setPrefetchLimits(0, 256);
        MessageConsumer recv2 = consumerSession2.createConsumer(_queue);

        r2 = recv2.receive(RECEIVE_TIMEOUT);
        assertNotNull("Second message not received", r2);
        assertEquals("Messages in wrong order", 2, r2.getIntProperty("msg"));

        Message r3 = recv2.receive(RECEIVE_TIMEOUT);
        assertNull("Third message incorrectly delivered", r3);

        r3 = recv1.receive(RECEIVE_TIMEOUT);
        assertNotNull("Third message not received", r3);
        assertEquals("Messages in wrong order", 3, r3.getIntProperty("msg"));

        r2.acknowledge();
        r3.acknowledge();                                                                 
        recv1.close();
        recv2.close();
        consumerSession1.close();
        consumerSession2.close();
        consumerConnection.close();

    }

    public static void main(String args[]) throws Throwable
    {
        FlowControlTest test = new FlowControlTest();

        int run = 0;
        while (true)
        {
            System.err.println("Test Run:" + ++run);
            Thread.sleep(1000);
            try
            {
                test.startBroker();
                test.testBasicBytesFlowControl();

                Thread.sleep(1000);
            }
            finally
            {
                test.stopBroker();
            }
        }
    }
}

