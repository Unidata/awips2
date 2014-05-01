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
import org.apache.qpid.client.message.JMSMapMessage;
import org.apache.qpid.test.utils.QpidTestCase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.MessageFormatException;
import javax.jms.MessageListener;
import javax.jms.MessageNotWriteableException;
import javax.jms.MessageProducer;
import javax.jms.Session;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MapMessageTest extends QpidTestCase implements MessageListener
{
    private static final Logger _logger = LoggerFactory.getLogger(MapMessageTest.class);

    private AMQConnection _connection;
    private Destination _destination;
    private AMQSession _session;
    private final List<JMSMapMessage> received = new ArrayList<JMSMapMessage>();

    private static final String MESSAGE = "Message ";
    private int _count = 100;
    public String _connectionString = "vm://:1";
    private byte[] _bytes = { 99, 98, 97, 96, 95 };
    private static final float _smallfloat = 100.0f;

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
        _logger.info("Tearing Down unit.basic.MapMessageTest");
        super.tearDown();
    }

    private void init(AMQConnection connection) throws Exception
    {
        Destination destination = new AMQQueue(connection, randomize("MapMessageTest"), true);
        init(connection, destination);
    }

    private void init(AMQConnection connection, Destination destination) throws Exception
    {
        _connection = connection;
        _destination = destination;
        _session = (AMQSession) connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        // set up a slow consumer
        _session.createConsumer(destination).setMessageListener(this);
        connection.start();
    }

    public void test() throws Exception
    {
        int count = _count;
        send(count);
        waitFor(count);
        check();
        _connection.close();
    }

    void send(int count) throws JMSException
    {
        // create a publisher
        MessageProducer producer = _session.createProducer(_destination);
        for (int i = 0; i < count; i++)
        {
            MapMessage message = _session.createMapMessage();

            setMapValues(message, i);

            producer.send(message);
        }
    }

    private void setMapValues(MapMessage message, int i) throws JMSException
    {
        message.setBoolean("odd", (i / 2) == 0);
        message.setByte("byte",Byte.MAX_VALUE);
        message.setBytes("bytes", _bytes);
        message.setChar("char",'c');
        message.setDouble("double", Double.MAX_VALUE);
        message.setFloat("float", Float.MAX_VALUE);
        message.setFloat("smallfloat", 100);
        message.setInt("messageNumber", i);
        message.setInt("int",  Integer.MAX_VALUE);
        message.setLong("long",  Long.MAX_VALUE);
        message.setShort("short", Short.MAX_VALUE);
        message.setString("message", MESSAGE + i);

        // Test Setting Object Values
        message.setObject("object-bool", true);
        message.setObject("object-byte", Byte.MAX_VALUE);
        message.setObject("object-bytes", _bytes);
        message.setObject("object-char", 'c');
        message.setObject("object-double", Double.MAX_VALUE);
        message.setObject("object-float", Float.MAX_VALUE);
        message.setObject("object-int", Integer.MAX_VALUE);
        message.setObject("object-long", Long.MAX_VALUE);
        message.setObject("object-short", Short.MAX_VALUE);

        // Set a null String value
        message.setString("nullString", null);
        // Highlight protocol problem
        message.setString("emptyString", "");

    }

    void waitFor(int count) throws Exception
    {
        long waitTime = 30000L;
        long waitUntilTime = System.currentTimeMillis() + 30000L;

        synchronized (received)
        {
            while ((received.size() < count) && (waitTime > 0))
            {
                if (received.size() < count)
                {
                    received.wait(waitTime);
                }

                if (received.size() < count)
                {
                    waitTime = waitUntilTime - System.currentTimeMillis();
                }
            }

            if (received.size() < count)
            {
                throw new Exception("Timed-out.  Waiting for " + count + " only got " + received.size());
            }
        }
    }

    void check() throws JMSException
    {
         int count = 0;
        for (JMSMapMessage m : received)
        {
             testMapValues(m, count);

            testCorrectExceptions(m);

            testMessageWriteStatus(m);

            testPropertyWriteStatus(m);

            count++;
        }
    }

    private void testCorrectExceptions(JMSMapMessage m) throws JMSException
    {
        testBoolean(m);

        testByte(m);

        testBytes(m);

        testChar(m);

        testDouble(m);

        testFloat(m);

        testInt(m);

        testLong(m);

        testShort(m);

        testString(m);
    }

    private void testString(JMSMapMessage m) throws JMSException
    {

        Assert.assertFalse(m.getBoolean("message"));

        try
        {
            m.getByte("message");
            fail("Exception Expected.");
        }
        catch (NumberFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getShort("message");
            fail("Exception Expected.");
        }
        catch (NumberFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getChar("message");
            fail("Exception Expected.");
        }
        catch (MessageFormatException npe)
        {
            // normal execution
        }

        try
        {
            m.getInt("message");
            fail("Exception Expected.");
        }
        catch (NumberFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getLong("message");
            fail("Exception Expected.");
        }
        catch (NumberFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getFloat("message");
            fail("Exception Expected.");
        }
        catch (NumberFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getDouble("message");
            fail("Exception Expected.");
        }
        catch (NumberFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getBytes("message");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals(MESSAGE + m.getInt("messageNumber"), m.getString("message"));
    }

    private void testShort(JMSMapMessage m) throws JMSException
    {

        // Try bad reads
        try
        {
            m.getBoolean("short");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getByte("short");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals(Short.MAX_VALUE, m.getShort("short"));

        // Try bad reads
        try
        {
            m.getChar("short");
            fail("Exception Expected.");
        }
        catch (MessageFormatException npe)
        {
            // normal execution
        }

        Assert.assertEquals(Short.MAX_VALUE, m.getInt("short"));

        Assert.assertEquals(Short.MAX_VALUE, m.getLong("short"));

        // Try bad reads
        try
        {
            m.getFloat("short");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getDouble("short");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getBytes("short");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals("" + Short.MAX_VALUE, m.getString("short"));
    }

    private void testLong(JMSMapMessage m) throws JMSException
    {

        // Try bad reads
        try
        {
            m.getBoolean("long");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getByte("long");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getShort("long");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getChar("long");
            fail("Exception Expected.");
        }
        catch (MessageFormatException npe)
        {
            // normal execution
        }

        try
        {
            m.getInt("long");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals(Long.MAX_VALUE, m.getLong("long"));

        // Try bad reads
        try
        {
            m.getFloat("long");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getDouble("long");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getBytes("long");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals("" + Long.MAX_VALUE, m.getString("long"));
    }

    private void testDouble(JMSMapMessage m) throws JMSException
    {

        // Try bad reads
        try
        {
            m.getBoolean("double");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getByte("double");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getShort("double");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getChar("double");
            fail("Exception Expected.");
        }
        catch (MessageFormatException npe)
        {
            // normal execution
        }

        try
        {
            m.getInt("double");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getLong("double");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getFloat("double");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals(Double.MAX_VALUE, m.getDouble("double"));

        // Try bad reads
        try
        {
            m.getBytes("double");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals("" + Double.MAX_VALUE, m.getString("double"));
    }

    private void testFloat(JMSMapMessage m) throws JMSException
    {

        // Try bad reads
        try
        {
            m.getBoolean("float");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getByte("float");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getShort("float");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getChar("float");
            fail("Exception Expected.");
        }
        catch (MessageFormatException npe)
        {
            // normal execution
        }

        try
        {
            m.getInt("float");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getLong("float");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals(Float.MAX_VALUE, m.getFloat("float"));

        Assert.assertEquals(_smallfloat, (float) m.getDouble("smallfloat"));

        // Try bad reads
        try
        {
            m.getBytes("float");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals("" + Float.MAX_VALUE, m.getString("float"));
    }

    private void testInt(JMSMapMessage m) throws JMSException
    {

        // Try bad reads
        try
        {
            m.getBoolean("int");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getByte("int");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getShort("int");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getChar("int");
            fail("Exception Expected.");
        }
        catch (MessageFormatException npe)
        {
            // normal execution
        }

        Assert.assertEquals(Integer.MAX_VALUE, m.getInt("int"));

        Assert.assertEquals(Integer.MAX_VALUE, (int) m.getLong("int"));

        // Try bad reads
        try
        {
            m.getFloat("int");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getDouble("int");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getBytes("int");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals("" + Integer.MAX_VALUE, m.getString("int"));
    }

    private void testChar(JMSMapMessage m) throws JMSException
    {

        // Try bad reads
        try
        {
            m.getBoolean("char");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getByte("char");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getShort("char");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals('c', m.getChar("char"));

        try
        {
            m.getInt("char");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getLong("char");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getFloat("char");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getDouble("char");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getBytes("char");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals("" + 'c', m.getString("char"));
    }

    private void testBytes(JMSMapMessage m) throws JMSException
    {
        // Try bad reads
        try
        {
            m.getBoolean("bytes");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getByte("bytes");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getShort("bytes");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getChar("bytes");
            fail("Exception Expected.");
        }
        catch (MessageFormatException npe)
        {
            // normal execution
        }

        try
        {
            m.getInt("bytes");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        try
        {
            m.getLong("bytes");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getFloat("bytes");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getDouble("bytes");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        assertBytesEqual(_bytes, m.getBytes("bytes"));

        try
        {
            m.getString("bytes");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

    }

    private void testByte(JMSMapMessage m) throws JMSException
    {
        // Try bad reads
        try
        {
            m.getBoolean("byte");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals(Byte.MAX_VALUE, m.getByte("byte"));

        Assert.assertEquals((short) Byte.MAX_VALUE, m.getShort("byte"));

        // Try bad reads
        try
        {
            m.getChar("byte");
            fail("Exception Expected.");
        }
        catch (MessageFormatException npe)
        {
            // normal execution
        }

        // Reading a byte as an int is ok
        Assert.assertEquals((short) Byte.MAX_VALUE, m.getInt("byte"));

        Assert.assertEquals((short) Byte.MAX_VALUE, m.getLong("byte"));

        // Try bad reads
        try
        {
            m.getFloat("byte");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getDouble("byte");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getBytes("byte");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals("" + Byte.MAX_VALUE, m.getString("byte"));

    }

    private void testBoolean(JMSMapMessage m) throws JMSException
    {

        Assert.assertEquals((m.getInt("messageNumber") / 2) == 0, m.getBoolean("odd"));

        // Try bad reads
        try
        {
            m.getByte("odd");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        // Try bad reads
        try
        {
            m.getShort("odd");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getChar("odd");
            fail("Exception Expected.");
        }
        catch (MessageFormatException npe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getInt("odd");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getLong("odd");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getFloat("odd");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getDouble("odd");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }
        // Try bad reads
        try
        {
            m.getBytes("odd");
            fail("Exception Expected.");
        }
        catch (MessageFormatException nfe)
        {
            // normal execution
        }

        Assert.assertEquals("" + ((m.getInt("messageNumber") / 2) == 0), m.getString("odd"));
    }

    private void testPropertyWriteStatus(JMSMapMessage m) throws JMSException
    {
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

    private void testMessageWriteStatus(JMSMapMessage m) throws JMSException
    {
        try
        {
            m.setInt("testint", 3);
            fail("Message should not be writeable");
        }
        catch (MessageNotWriteableException mnwe)
        {
            // normal execution
        }

        m.clearBody();

        try
        {
            m.setInt("testint", 3);
        }
        catch (MessageNotWriteableException mnwe)
        {
            Assert.fail("Message should be writeable");
        }
    }

    private void testMapValues(JMSMapMessage m, int count) throws JMSException
    {
        // Test get<Primiative>

        // Boolean
        assertEqual((count / 2) == 0, m.getBoolean("odd"));
        assertEqual("" + ((count / 2) == 0), m.getString("odd"));

        // Byte
        assertEqual(Byte.MAX_VALUE, m.getByte("byte"));
        assertEqual("" + Byte.MAX_VALUE, m.getString("byte"));

        // Bytes
        assertBytesEqual(_bytes, m.getBytes("bytes"));

        // Char
        assertEqual('c', m.getChar("char"));

        // Double
        assertEqual(Double.MAX_VALUE, m.getDouble("double"));
        assertEqual("" + Double.MAX_VALUE, m.getString("double"));

        // Float
        assertEqual(Float.MAX_VALUE, m.getFloat("float"));
        assertEqual(_smallfloat, (float) m.getDouble("smallfloat"));
        assertEqual("" + Float.MAX_VALUE, m.getString("float"));

        // Integer
        assertEqual(Integer.MAX_VALUE, m.getInt("int"));
        assertEqual("" + Integer.MAX_VALUE, m.getString("int"));
        assertEqual(count, m.getInt("messageNumber"));

        // long
        assertEqual(Long.MAX_VALUE, m.getLong("long"));
        assertEqual("" + Long.MAX_VALUE, m.getString("long"));

        // Short
        assertEqual(Short.MAX_VALUE, m.getShort("short"));
        assertEqual("" + Short.MAX_VALUE, m.getString("short"));
        assertEqual((int) Short.MAX_VALUE, m.getInt("short"));

        // String
        assertEqual(MESSAGE + count, m.getString("message"));

        // Test getObjects
        assertEqual(true, m.getObject("object-bool"));
        assertEqual(Byte.MAX_VALUE, m.getObject("object-byte"));
        assertBytesEqual(_bytes, (byte[]) m.getObject("object-bytes"));
        assertEqual('c', m.getObject("object-char"));
        assertEqual(Double.MAX_VALUE, m.getObject("object-double"));
        assertEqual(Float.MAX_VALUE, m.getObject("object-float"));
        assertEqual(Integer.MAX_VALUE, m.getObject("object-int"));
        assertEqual(Long.MAX_VALUE, m.getObject("object-long"));
        assertEqual(Short.MAX_VALUE, m.getObject("object-short"));

        // Check Special values
        assertTrue(m.getString("nullString") == null);
        assertEqual("", m.getString("emptyString"));
    }

    private void assertBytesEqual(byte[] expected, byte[] actual)
    {
        Assert.assertEquals(expected.length, actual.length);

        for (int index = 0; index < expected.length; index++)
        {
            Assert.assertEquals(expected[index], actual[index]);
        }
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
            _logger.info("****************** Recevied Messgage:" + message);
            received.add((JMSMapMessage) message);
            received.notify();
        }
    }

    private static String randomize(String in)
    {
        return in + System.currentTimeMillis();
    }

    public static void main(String[] argv) throws Exception
    {
        MapMessageTest test = new MapMessageTest();
        test._connectionString = (argv.length == 0) ? "vm://:1" : argv[0];
        test.setUp();
        if (argv.length > 1)
        {
            test._count = Integer.parseInt(argv[1]);
        }

        test.test();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(MapMessageTest.class);
    }
}
