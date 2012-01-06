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
package org.apache.qpid.test.client.message;

import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.framing.AMQShortString;

import javax.jms.BytesMessage;
import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.ObjectMessage;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.StreamMessage;
import javax.jms.TextMessage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.util.UUID;

public class MessageToStringTest extends QpidTestCase
{
    private Connection _connection;
    private Session _session;
    private Queue _queue;
    MessageConsumer _consumer;
    private static final String BYTE_TEST = "MapByteTest";

    public void setUp() throws Exception
    {
        super.setUp();

        //Create Producer put some messages on the queue
        _connection = getConnection();

        //Create Consumer
        _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        String queueName = getTestQueueName();

        //Create Queue
        ((AMQSession) _session).createQueue(new AMQShortString(queueName), true, false, false);
        _queue = _session.createQueue("direct://amq.direct/"+queueName+"/"+queueName+"?durable='false'&autodelete='true'");


        _consumer = _session.createConsumer(_queue);

        _connection.start();
    }

    public void tearDown() throws Exception
    {
        //clean up
        _connection.close();

        super.tearDown();
    }

    public void testBytesMessage() throws JMSException
    {
        //Create Sample Message using UUIDs
        UUID test = UUID.randomUUID();

        BytesMessage testMessage = _session.createBytesMessage();

        //Convert UUID into bytes for transit
        byte[] testBytes = test.toString().getBytes();

        testMessage.writeBytes(testBytes);

        sendAndTest(testMessage, testBytes);
    }

    public void testMapMessage() throws JMSException, IOException
    {
        //Create Sample Message using UUIDs
        UUID test = UUID.randomUUID();

        MapMessage testMessage = _session.createMapMessage();

        byte[] testBytes = convertToBytes(test);

        testMessage.setBytes(BYTE_TEST, testBytes);

        sendAndTest(testMessage, testBytes);
    }

    public void testObjectMessage() throws JMSException
    {
        MessageProducer producer = _session.createProducer(_queue);

        //Create Sample Message using UUIDs
        UUID test = UUID.randomUUID();

        Message testMessage = _session.createObjectMessage(test);

        sendAndTest(testMessage, test);
    }

    public void testStreamMessage() throws JMSException, IOException
    {
        //Create Sample Message using UUIDs
        UUID test = UUID.randomUUID();

        StreamMessage testMessage = _session.createStreamMessage();

        byte[] testBytes = convertToBytes(test);

        testMessage.writeBytes(testBytes);

        sendAndTest(testMessage, testBytes);
    }

    public void testTextMessage() throws JMSException, IOException
    {
        //Create Sample Message using UUIDs
        UUID test = UUID.randomUUID();

        TextMessage testMessage = _session.createTextMessage();

        String stringValue = String.valueOf(test);
        byte[] testBytes = stringValue.getBytes();

        testMessage.setText(stringValue);
        
        sendAndTest(testMessage, testBytes);
    }

    //***************** Helpers

    private void sendAndTest(Message message, Object testBytes) throws JMSException
    {
        MessageProducer producer = _session.createProducer(_queue);

        producer.send(message);

        Message receivedMessage = _consumer.receive(1000);

        assertNotNull("Message was not received.", receivedMessage);

        //Ensure that to calling toString doesn't error and that doing this doesn't break next tests.
        assertNotNull("Message returned null from toString", receivedMessage.toString());

        byte[] byteResults;
        UUID result;

        try
        {
            if (receivedMessage instanceof ObjectMessage)
            {
                result = (UUID) ((ObjectMessage) receivedMessage).getObject();
                assertEquals("UUIDs were not equal", testBytes, result);
            }
            else
            {
                byteResults = getBytes(receivedMessage, ((byte[]) testBytes).length);
                assertBytesEquals("UUIDs were not equal", (byte[]) testBytes, byteResults);
            }
        }
        catch (Exception e)
        {
            fail(e.getMessage());
        }

    }

    private void assertBytesEquals(String message, byte[] expected, byte[] actual)
    {
        if (expected.length == actual.length)
        {
            int index = 0;
            boolean failed = false;
            for (byte b : expected)
            {
                if (actual[index++] != b)
                {
                    failed = true;
                    break;
                }
            }

            if (!failed)
            {
                return;
            }

        }

        fail(message);
    }

    private byte[] getBytes(Message receivedMessage, int testBytesLength) throws JMSException
    {
        byte[] byteResults = new byte[testBytesLength];

        if (receivedMessage instanceof BytesMessage)
        {
            assertEquals(testBytesLength, ((BytesMessage) receivedMessage).readBytes(byteResults));
        }
        else if (receivedMessage instanceof StreamMessage)
        {
            assertEquals(testBytesLength, ((StreamMessage) receivedMessage).readBytes(byteResults));
        }
        else if (receivedMessage instanceof MapMessage)
        {
            byteResults = ((MapMessage) receivedMessage).getBytes(BYTE_TEST);
            assertEquals(testBytesLength, byteResults.length);
        }
        else if (receivedMessage instanceof TextMessage)
        {
            byteResults = ((TextMessage) receivedMessage).getText().getBytes();
            assertEquals(testBytesLength, byteResults.length);
        }


        return byteResults;
    }

    private byte[] convertToBytes(UUID test) throws IOException
    {
        //Convert UUID into bytes for transit
        ObjectOutput out;
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        out = new ObjectOutputStream(bos);
        out.writeObject(test);
        out.close();

        return bos.toByteArray();
    }

}
