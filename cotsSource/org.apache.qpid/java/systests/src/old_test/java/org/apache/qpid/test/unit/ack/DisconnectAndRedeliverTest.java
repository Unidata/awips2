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

import junit.framework.TestCase;
import org.apache.log4j.Logger;
import org.apache.log4j.xml.DOMConfigurator;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.store.TestableMemoryMessageStore;
import org.apache.qpid.server.util.TestApplicationRegistry;

import javax.jms.*;

public class DisconnectAndRedeliverTest extends TestCase
{
    private static final Logger _logger = Logger.getLogger(DisconnectAndRedeliverTest.class);

    static
    {
        String workdir = System.getProperty("QPID_WORK");
        if (workdir == null || workdir.equals(""))
        {
            String tempdir = System.getProperty("java.io.tmpdir");
            System.out.println("QPID_WORK not set using tmp directory: " + tempdir);
            System.setProperty("QPID_WORK", tempdir);
        }
        DOMConfigurator.configure("../broker/etc/log4j.xml");
    }

    protected void setUp() throws Exception
    {
        super.setUp();
        TransportConnection.createVMBroker(1);
        ApplicationRegistry.initialise(new TestApplicationRegistry(), 1);
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
        TransportConnection.killAllVMBrokers();
    }

    /**
     * This tests that when there are unacknowledged messages on a channel they are requeued for delivery when
     * the channel is closed.
     *
     * @throws Exception
     */
    public void testDisconnectRedeliversMessages() throws Exception
    {
        Connection con = new AMQConnection("vm://:1", "guest", "guest", "consumer1", "/test");

        TestableMemoryMessageStore store = (TestableMemoryMessageStore) ApplicationRegistry.getInstance().getMessageStore();

        Session consumerSession = (AMQSession) con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        AMQQueue queue = new AMQQueue("someQ", "someQ", false, false);
        MessageConsumer consumer = consumerSession.createConsumer(queue);
        //force synch to ensure the consumer has resulted in a bound queue
        ((AMQSession) consumerSession).declareExchangeSynch("amq.direct", "direct");

        Connection con2 = new AMQConnection("vm://:1", "guest", "guest", "producer1", "/test");


        Session producerSession = con2.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        MessageProducer producer = producerSession.createProducer(queue);

        _logger.info("Sending four messages");
        producer.send(producerSession.createTextMessage("msg1"));
        producer.send(producerSession.createTextMessage("msg2"));
        producer.send(producerSession.createTextMessage("msg3"));
        producer.send(producerSession.createTextMessage("msg4"));

        con2.close();

        _logger.info("Starting connection");
        con.start();
        TextMessage tm = (TextMessage) consumer.receive();
        tm.acknowledge();
        _logger.info("Received and acknowledged first message");
        consumer.receive();
        consumer.receive();
        consumer.receive();
        _logger.info("Received all four messages. About to disconnect and reconnect");

        con.close();
        con = new AMQConnection("vm://:1", "guest", "guest", "consumer1", "/test");
        consumerSession = con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        consumer = consumerSession.createConsumer(queue);

        _logger.info("Starting second consumer connection");
        con.start();

        tm = (TextMessage) consumer.receive(3000);
        assertEquals("msg2", tm.getText());


        tm = (TextMessage) consumer.receive(3000);
        assertEquals("msg3", tm.getText());


        tm = (TextMessage) consumer.receive(3000);
        assertEquals("msg4", tm.getText());

        _logger.info("Received redelivery of three messages. Acknowledging last message");
        tm.acknowledge();

        con.close();

        con = new AMQConnection("vm://:1", "guest", "guest", "consumer1", "/test");
        consumerSession = con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        consumer = consumerSession.createConsumer(queue);
        _logger.info("Starting third consumer connection");
        con.start();
        tm = (TextMessage) consumer.receiveNoWait();
        assertNull(tm);
        _logger.info("No messages redelivered as is expected");
        con.close();

        con = new AMQConnection("vm://:1", "guest", "guest", "consumer1", "/test");
        consumerSession = con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        consumer = consumerSession.createConsumer(queue);
        _logger.info("Starting fourth consumer connection");
        con.start();
        tm = (TextMessage) consumer.receive(3000);
        assertNull(tm);
        _logger.info("No messages redelivered as is expected");
        con.close();

        _logger.info("Actually:" + store.getMessageMetaDataMap().size());
        //  assertTrue(store.getMessageMap().size() == 0);
    }

    /**
     * Tests that unacknowledged messages are thrown away when the channel is closed and they cannot be
     * requeued (due perhaps to the queue being deleted).
     *
     * @throws Exception
     */
    public void testDisconnectWithTransientQueueThrowsAwayMessages() throws Exception
    {

        Connection con = new AMQConnection("vm://:1", "guest", "guest", "consumer1", "/test");
        TestableMemoryMessageStore store = (TestableMemoryMessageStore) ApplicationRegistry.getInstance().getMessageStore();
        Session consumerSession = con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        Queue queue = new AMQQueue("someQ", "someQ", false, true);
        MessageConsumer consumer = consumerSession.createConsumer(queue);
        //force synch to ensure the consumer has resulted in a bound queue
        ((AMQSession) consumerSession).declareExchangeSynch("amq.direct", "direct");

        Connection con2 = new AMQConnection("vm://:1", "guest", "guest", "producer1", "/test");
        Session producerSession = con2.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        MessageProducer producer = producerSession.createProducer(queue);

        _logger.info("Sending four messages");
        producer.send(producerSession.createTextMessage("msg1"));
        producer.send(producerSession.createTextMessage("msg2"));
        producer.send(producerSession.createTextMessage("msg3"));
        producer.send(producerSession.createTextMessage("msg4"));

        con2.close();

        _logger.info("Starting connection");
        con.start();
        TextMessage tm = (TextMessage) consumer.receive();
        tm.acknowledge();
        _logger.info("Received and acknowledged first message");
        consumer.receive();
        consumer.receive();
        consumer.receive();
        _logger.info("Received all four messages. About to disconnect and reconnect");

        con.close();
        con = new AMQConnection("vm://:1", "guest", "guest", "consumer1", "/test");
        consumerSession = con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        consumer = consumerSession.createConsumer(queue);

        _logger.info("Starting second consumer connection");
        con.start();

        tm = (TextMessage) consumer.receiveNoWait();
        assertNull(tm);
        _logger.info("No messages redelivered as is expected");

        _logger.info("Actually:" + store.getMessageMetaDataMap().size());
        assertTrue(store.getMessageMetaDataMap().size() == 0);
        con.close();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(DisconnectAndRedeliverTest.class);
    }
}
