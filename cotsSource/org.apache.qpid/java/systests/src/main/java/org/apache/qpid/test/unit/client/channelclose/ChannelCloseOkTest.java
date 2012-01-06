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
package org.apache.qpid.test.unit.client.channelclose;

import junit.textui.TestRunner;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.test.utils.QpidTestCase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.Destination;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;

import java.util.ArrayList;
import java.util.List;

/**
 * Due to bizarre exception handling all sessions are closed if you get
 * a channel close request and no exception listener is registered.
 * <p/>
 * JIRA issue IBTBLZ-10.
 * <p/>
 * Simulate by:
 * <p/>
 * 0. Create two sessions with no exception listener.
 * 1. Publish message to queue/topic that does not exist (wrong routing key).
 * 2. This will cause a channel close.
 * 3. Since client does not have an exception listener, currently all sessions are
 * closed.
 */
public class ChannelCloseOkTest extends QpidTestCase
{
    private AMQConnection _connection;
    private Destination _destination1;
    private Destination _destination2;
    private Session _session1;
    private Session _session2;
    private final List<Message> _received1 = new ArrayList<Message>();
    private final List<Message> _received2 = new ArrayList<Message>();

    private static final Logger _log = LoggerFactory.getLogger(ChannelCloseOkTest.class);

    protected void setUp() throws Exception
    {
        super.setUp();

        _connection =  (AMQConnection) getConnection("guest", "guest");

        _destination1 = new AMQQueue(_connection, "q1", true);
        _destination2 = new AMQQueue(_connection, "q2", true);
        _session1 = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        _session1.createConsumer(_destination1).setMessageListener(new MessageListener()
            {
                public void onMessage(Message message)
                {
                    _log.debug("consumer 1 got message [" + getTextMessage(message) + "]");
                    synchronized (_received1)
                    {
                        _received1.add(message);
                        _received1.notify();
                    }
                }
            });
        _session2 = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        _session2.createConsumer(_destination2).setMessageListener(new MessageListener()
            {
                public void onMessage(Message message)
                {
                    _log.debug("consumer 2 got message [" + getTextMessage(message) + "]");
                    synchronized (_received2)
                    {
                        _received2.add(message);
                        _received2.notify();
                    }
                }
            });

        _connection.start();
    }

    private String getTextMessage(Message message)
    {
        TextMessage tm = (TextMessage) message;
        try
        {
            return tm.getText();
        }
        catch (JMSException e)
        {
            return "oops " + e;
        }
    }

    protected void tearDown() throws Exception
    {
        closeConnection();
        super.tearDown();
    }

    public void closeConnection() throws JMSException
    {
        if (_connection != null)
        {
            _log.info(">>>>>>>>>>>>>>.. closing");
            _connection.close();
        }
    }

    public void testWithoutExceptionListener() throws Exception
    {
        doTest();
    }

    public void testWithExceptionListener() throws Exception
    {
        _connection.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException jmsException)
                {
                    _log.warn("onException - " + jmsException.getMessage());
                }
            });

        doTest();
    }

    public void doTest() throws Exception
    {
        // Check both sessions are ok.
        sendAndWait(_session1, _destination1, "first", _received1, 1);
        sendAndWait(_session2, _destination2, "second", _received2, 1);
        assertEquals(1, _received1.size());
        assertEquals(1, _received2.size());

        // Now send message to incorrect destination on session 1.
        Destination destination = new AMQQueue(_connection, "incorrect");
        send(_session1, destination, "third"); // no point waiting as message will never be received.

        // Ensure both sessions are still ok.
        // Send a bunch of messages as this give time for the sessions to be erroneously closed.
        final int num = 300;
        for (int i = 0; i < num; ++i)
        {
            send(_session1, _destination1, "" + i);
            send(_session2, _destination2, "" + i);
        }

        waitFor(_received1, num + 1);
        waitFor(_received2, num + 1);

        // Note that the third message is never received as it is sent to an incorrect destination.
        assertEquals(num + 1, _received1.size());
        assertEquals(num + 1, _received2.size());
    }

    private void sendAndWait(Session session, Destination destination, String message, List<Message> received, int count)
        throws JMSException, InterruptedException
    {
        send(session, destination, message);
        waitFor(received, count);
    }

    private void send(Session session, Destination destination, String message) throws JMSException
    {
        _log.debug("sending message " + message);
        MessageProducer producer1 = session.createProducer(destination);
        producer1.send(session.createTextMessage(message));
    }

    private void waitFor(List<Message> received, int count) throws InterruptedException
    {
        long timeout = 20000;

        synchronized (received)
        {
            long start = System.currentTimeMillis();
            while (received.size() < count)
            {
                if (System.currentTimeMillis() - start > timeout)
                {
                    fail("timeout expired waiting for messages");
                }
                try
                {
                     received.wait(timeout);
                }
                catch (InterruptedException e)
                {
                    _log.info("Interrupted: " + e);
                    throw e;
                }

            }
        }
    }

    private static String randomize(String in)
    {
        return in + System.currentTimeMillis();
    }

    public static void main(String[] args)
    {
        TestRunner.run(ChannelCloseOkTest.class);
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(ChannelCloseOkTest.class);
    }
}
