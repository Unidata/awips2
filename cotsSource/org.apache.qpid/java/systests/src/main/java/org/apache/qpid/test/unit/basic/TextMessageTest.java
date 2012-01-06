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
package org.apache.qpid.test.unit.basic;

import junit.framework.Assert;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.message.JMSTextMessage;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.test.utils.QpidTestCase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageNotWriteableException;
import javax.jms.MessageProducer;
import javax.jms.Session;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class TextMessageTest extends QpidTestCase implements MessageListener
{
    private static final Logger _logger = LoggerFactory.getLogger(TextMessageTest.class);

    private AMQConnection _connection;
    private Destination _destination;
    private AMQSession _session;
    private final List<JMSTextMessage> received = new ArrayList<JMSTextMessage>();
    private final List<String> messages = new ArrayList<String>();
    private int _count = 100;
    public String _connectionString = "vm://:1";
    private CountDownLatch _waitForCompletion;

    protected void setUp() throws Exception
    {
        super.setUp();
        try
        {
            init((AMQConnection) getConnection("guest", "guest"));
        }
        catch (Exception e)
        {
            fail("Unable to initialilse connection: " + e);
        }
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    private void init(AMQConnection connection) throws Exception
    {
        Destination destination =
            new AMQQueue(connection.getDefaultQueueExchangeName(), new AMQShortString(randomize("TextMessageTest")), true);
        init(connection, destination);
    }

    private void init(AMQConnection connection, Destination destination) throws Exception
    {
        _connection = connection;
        _destination = destination;
        _session = (AMQSession) connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        // set up a slow consumer
        try
        {
            _session.createConsumer(destination).setMessageListener(this);
        }
        catch (Throwable  e)
        {
            e.printStackTrace();
        }
        connection.start();
    }

    public void test() throws Exception
    {
        int count = _count;
        _waitForCompletion = new CountDownLatch(_count);
        send(count);
        _waitForCompletion.await(20, TimeUnit.SECONDS);
        check();
        _logger.info("Completed without failure");
        _connection.close();
    }

    void send(int count) throws JMSException
    {
        // create a publisher
        MessageProducer producer = _session.createProducer(_destination);
        for (int i = 0; i < count; i++)
        {
            String text = "Message " + i;
            messages.add(text);
            Message m = _session.createTextMessage(text);
            //m.setStringProperty("String", "hello");

            _logger.info("Sending Msg:" + m);
            producer.send(m);
        }
        _logger.info("sent " + count  + " mesages");
    }


    void check() throws JMSException
    {
        List<String> actual = new ArrayList<String>();
        for (JMSTextMessage m : received)
        {
            actual.add(m.getText());

            // Check body write status
            try
            {
                m.setText("Test text");
                Assert.fail("Message should not be writeable");
            }
            catch (MessageNotWriteableException mnwe)
            {
                // normal execution
            }

            m.clearBody();

            try
            {
                m.setText("Test text");
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
                assertEqual(expected.next(), actual.next());
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

    private static void assertEqual(Object expected, Object actual)
    {
        if (!expected.equals(actual))
        {
            throw new RuntimeException("Expected '" + expected + "' found '" + actual + "'");
        }
    }

    public void onMessage(Message message)
    {
        synchronized (received)
        {
            _logger.info("===== received one message");
            received.add((JMSTextMessage) message);
            _waitForCompletion.countDown();
        }
    }

    private static String randomize(String in)
    {
        return in + System.currentTimeMillis();
    }



    public static junit.framework.Test suite()
    {
         return new junit.framework.TestSuite(TextMessageTest.class);
    }
}
