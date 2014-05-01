/*
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
package org.apache.qpid.test.unit.basic;

import junit.framework.Assert;

import org.apache.mina.common.ByteBuffer;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.message.JMSBytesMessage;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.transport.util.Waiter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.BytesMessage;
import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageNotReadableException;
import javax.jms.MessageNotWriteableException;
import javax.jms.MessageProducer;
import javax.jms.Session;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class BytesMessageTest extends QpidTestCase implements MessageListener
{
    private static final Logger _logger = LoggerFactory.getLogger(BytesMessageTest.class);

    private Connection _connection;
    private Destination _destination;
    private Session _session;
    private final List<JMSBytesMessage> received = new ArrayList<JMSBytesMessage>();
    private final List<byte[]> messages = new ArrayList<byte[]>();
    private int _count = 100;
    public String _connectionString = "vm://:1";

    protected void setUp() throws Exception
    {
        super.setUp();
        init((AMQConnection) getConnection("guest", "guest"));
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    void init(AMQConnection connection) throws Exception
    {
        init(connection, new AMQQueue(connection, randomize("BytesMessageTest"), true));
    }

    void init(AMQConnection connection, AMQDestination destination) throws Exception
    {
        _connection = connection;
        _destination = destination;
        _session = connection.createSession(false, AMQSession.NO_ACKNOWLEDGE);

        // Set up a slow consumer.
        _session.createConsumer(destination).setMessageListener(this);
        connection.start();
    }

    public void test() throws Exception
    {
        try
        {
            send(_count);
            waitFor(_count);
            check();
            _logger.info("Completed without failure");
        }
        finally
        {
            _connection.close();
        }
    }

    void send(int count) throws JMSException
    {
        // create a publisher
        MessageProducer producer = _session.createProducer(_destination);
        for (int i = 0; i < count; i++)
        {
            BytesMessage msg = _session.createBytesMessage();

            try
            {
                msg.readFloat();
                Assert.fail("Message should not be readable");
            }
            catch (MessageNotReadableException mnwe)
            {
                // normal execution
            }

            byte[] data = ("Message " + i).getBytes();
            msg.writeBytes(data);
            messages.add(data);
            producer.send(msg);
        }
    }

    void waitFor(int count) throws InterruptedException
    {
        synchronized (received)
        {
            Waiter w = new Waiter(received, 30000);
            while (received.size() < count && w.hasTime())
            {
                w.await();
            }
        }
    }

    void check() throws JMSException
    {
        List<byte[]> actual = new ArrayList<byte[]>();
        for (JMSBytesMessage m : received)
        {
            ByteBuffer buffer = m.getData();
            byte[] data = new byte[buffer.remaining()];
            buffer.get(data);
            actual.add(data);

            // Check Body Write Status
            try
            {
                m.writeBoolean(true);
                Assert.fail("Message should not be writeable");
            }
            catch (MessageNotWriteableException mnwe)
            {
                // normal execution
            }

            m.clearBody();

            try
            {
                m.writeBoolean(true);
            }
            catch (MessageNotWriteableException mnwe)
            {
                Assert.fail("Message should be writeable");
            }

            // Check property write status
            try
            {
                m.setStringProperty("test", "test");
                Assert.fail("Message should not be writeable");
            }
            catch (MessageNotWriteableException mnwe)
            {
                // normal execution
            }

            m.clearProperties();

            try
            {
                m.setStringProperty("test", "test");
            }
            catch (MessageNotWriteableException mnwe)
            {
                Assert.fail("Message should be writeable");
            }

        }

        assertEqual(messages.iterator(), actual.iterator());
    }

    private static void assertEqual(Iterator expected, Iterator actual)
    {
        List<String> errors = new ArrayList<String>();
        while (expected.hasNext() && actual.hasNext())
        {
            try
            {
                assertEquivalent((byte[]) expected.next(), (byte[]) actual.next());
            }
            catch (Exception e)
            {
                errors.add(e.getMessage());
            }
        }
        while (expected.hasNext())
        {
            errors.add("Expected " + expected.next() + " but no more actual values.");
        }
        while (actual.hasNext())
        {
            errors.add("Found " + actual.next() + " but no more expected values.");
        }

        if (!errors.isEmpty())
        {
            throw new RuntimeException(errors.toString());
        }
    }

    private static void assertEquivalent(byte[] expected, byte[] actual)
    {
        if (expected.length != actual.length)
        {
            throw new RuntimeException("Expected length " + expected.length + " got " + actual.length);
        }

        for (int i = 0; i < expected.length; i++)
        {
            if (expected[i] != actual[i])
            {
                throw new RuntimeException("Failed on byte " + i + " of " + expected.length);
            }
        }
    }

    public void onMessage(Message message)
    {
        synchronized (received)
        {
            received.add((JMSBytesMessage) message);
            received.notify();
        }
    }

    private static String randomize(String in)
    {
        return in + System.currentTimeMillis();
    }

    public static void main(String[] argv) throws Exception
    {
        final String connectionString;
        final int count;
        if (argv.length == 0)
        {
            connectionString = "vm://:1";
            count = 100;
        }
        else
        {
            connectionString = argv[0];
            count = Integer.parseInt(argv[1]);
        }

        System.out.println("connectionString = " + connectionString);
        System.out.println("count = " + count);

        BytesMessageTest test = new BytesMessageTest();
        test._connectionString = connectionString;
        test._count = count;
        test.test();
    }
}
