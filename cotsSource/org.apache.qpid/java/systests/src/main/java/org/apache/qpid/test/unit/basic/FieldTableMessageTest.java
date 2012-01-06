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

import org.apache.mina.common.ByteBuffer;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.message.JMSBytesMessage;
import org.apache.qpid.framing.AMQFrameDecodingException;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.FieldTableFactory;
import org.apache.qpid.test.utils.QpidTestCase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.BytesMessage;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class FieldTableMessageTest extends QpidTestCase implements MessageListener
{
    private static final Logger _logger = LoggerFactory.getLogger(FieldTableMessageTest.class);

    private AMQConnection _connection;
    private AMQDestination _destination;
    private AMQSession _session;
    private final ArrayList<JMSBytesMessage> received = new ArrayList<JMSBytesMessage>();
    private FieldTable _expected;
    private int _count = 10;
    public String _connectionString = "vm://:1";
    private CountDownLatch _waitForCompletion;

    protected void setUp() throws Exception
    {
        super.setUp();
        init( (AMQConnection) getConnection("guest", "guest"));
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    private void init(AMQConnection connection) throws Exception
    {
        init(connection, new AMQQueue(connection, randomize("FieldTableMessageTest"), true));
    }

    private void init(AMQConnection connection, AMQDestination destination) throws Exception
    {
        _connection = connection;
        _destination = destination;
        _session = (AMQSession) connection.createSession(false, AMQSession.NO_ACKNOWLEDGE);

        // set up a slow consumer
        _session.createConsumer(destination).setMessageListener(this);
        connection.start();

        // _expected = new FieldTableTest().load("FieldTableTest2.properties");
        _expected = load();
    }

    private FieldTable load() throws IOException
    {
        FieldTable result = FieldTableFactory.newFieldTable();
        result.setLong("one", 1L);
        result.setLong("two", 2L);
        result.setLong("three", 3L);
        result.setLong("four", 4L);
        result.setLong("five", 5L);

        return result;
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

    void send(int count) throws JMSException, IOException
    {
        // create a publisher
        MessageProducer producer = _session.createProducer(_destination);
        for (int i = 0; i < count; i++)
        {
            BytesMessage msg = _session.createBytesMessage();
            msg.writeBytes(_expected.getDataAsBytes());
            producer.send(msg);
        }
    }


    void check() throws JMSException, AMQFrameDecodingException
    {
        for (Object m : received)
        {
            ByteBuffer buffer = ((JMSBytesMessage) m).getData();
            FieldTable actual = FieldTableFactory.newFieldTable(buffer, buffer.remaining());
            for (String key : _expected.keys())
            {
                assertEquals("Values for " + key + " did not match", _expected.getObject(key), actual.getObject(key));
            }
        }
    }

    public void onMessage(Message message)
    {
        synchronized (received)
        {
            received.add((JMSBytesMessage) message);
            _waitForCompletion.countDown();
        }
    }

    private static String randomize(String in)
    {
        return in + System.currentTimeMillis();
    }

    public static void main(String[] argv) throws Exception
    {
        FieldTableMessageTest test = new FieldTableMessageTest();
        test._connectionString = (argv.length == 0) ? "vm://:1" : argv[0];
        test.setUp();
        test._count = (argv.length > 1) ? Integer.parseInt(argv[1]) : 5;
        test.test();
    }
}
