/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */
package org.apache.qpid.test.unit.client.message;

import javax.jms.JMSException;
import javax.jms.MessageFormatException;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.apache.qpid.client.message.JMSMapMessage;
import org.apache.qpid.client.message.TestMessageHelper;


public class MapMessageTest extends TestCase
{

    //Test Lookups

    public void testBooleanLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();

            mm.setBoolean("value", true);
            Assert.assertEquals(true, mm.getBoolean("value"));
            Assert.assertEquals("true", mm.getString("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testByteLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setByte("value", Byte.MAX_VALUE);

            Assert.assertEquals(Byte.MAX_VALUE, mm.getByte("value"));
            Assert.assertEquals((short) Byte.MAX_VALUE, mm.getShort("value"));
            Assert.assertEquals(Byte.MAX_VALUE, mm.getInt("value"));
            Assert.assertEquals((long) Byte.MAX_VALUE, mm.getLong("value"));
            Assert.assertEquals("" + Byte.MAX_VALUE, mm.getString("value"));

        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testShortLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setShort("value", Short.MAX_VALUE);
            Assert.assertEquals(Short.MAX_VALUE, mm.getShort("value"));
            Assert.assertEquals((int) Short.MAX_VALUE, mm.getInt("value"));
            Assert.assertEquals((long) Short.MAX_VALUE, mm.getLong("value"));
            Assert.assertEquals("" + Short.MAX_VALUE, mm.getString("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }


    public void testCharLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();

            mm.setChar("value", 'c');
            Assert.assertEquals('c', mm.getChar("value"));
            Assert.assertEquals("c", mm.getString("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }

        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();

            mm.setString("value", null);
            mm.getChar("value");
            fail("Expected NullPointerException");

        }
        catch (NullPointerException e)
        {
            ; // pass
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }



    }

    public void testDoubleLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setDouble("value", Double.MAX_VALUE);
            Assert.assertEquals(Double.MAX_VALUE, mm.getDouble("value"));
            Assert.assertEquals("" + Double.MAX_VALUE, mm.getString("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testFloatLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setFloat("value", Float.MAX_VALUE);
            Assert.assertEquals(Float.MAX_VALUE, mm.getFloat("value"));
            Assert.assertEquals((double) Float.MAX_VALUE, mm.getDouble("value"));
            Assert.assertEquals("" + Float.MAX_VALUE, mm.getString("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testIntLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setInt("value", Integer.MAX_VALUE);
            Assert.assertEquals(Integer.MAX_VALUE, mm.getInt("value"));
            Assert.assertEquals((long) Integer.MAX_VALUE, mm.getLong("value"));
            Assert.assertEquals("" + Integer.MAX_VALUE, mm.getString("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testLongLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setLong("value", Long.MAX_VALUE);
            Assert.assertEquals(Long.MAX_VALUE, mm.getLong("value"));
            Assert.assertEquals("" + Long.MAX_VALUE, mm.getString("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testBytesLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            byte[] bytes = {99, 98, 97, 96, 95};
            mm.setBytes("bytes", bytes);
            assertBytesEqual(bytes, mm.getBytes("bytes"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    // Failed Lookups

    public void testFailedBooleanLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            Assert.assertEquals(false, mm.getBoolean("int"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testFailedByteLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getByte("random");
            Assert.fail("NumberFormatException expected");
        }
        catch (NumberFormatException e)
        {
            //normal execution
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received:" + e);
        }

    }

    public void testFailedBytesLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getBytes("random");
            Assert.fail("MessageFormatException expected");
        }
        catch (MessageFormatException mfe)
        {
            //normal path
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received:" + e);
        }
    }

    public void testFailedCharLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getChar("random");
            Assert.fail("MessageFormatException expected");
        }
        catch (MessageFormatException e)
        {
            //normal execution
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received:" + e);
        }
    }

    public void testFailedDoubleLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getDouble("random");
            Assert.fail("NullPointerException should be received.");
        }
        catch (NullPointerException e)
        {
            //normal execution
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received:" + e);
        }
    }

    public void testFailedFloatLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getFloat("random");
            Assert.fail("NullPointerException should be received.");
        }
        catch (NullPointerException e)
        {
            //normal execution
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received:" + e);
        }
    }

    public void testFailedIntLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getInt("random");
            Assert.fail("NumberFormatException should be received.");
        }
        catch (NumberFormatException e)
        {
            //normal execution
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received:" + e);
        }
    }

    public void testFailedLongLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getLong("random");
            Assert.fail("NumberFormatException should be received.");
        }
        catch (NumberFormatException e)
        {
            //normal execution
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received:" + e);
        }
    }

    public void testFailedShortLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getShort("random");
            Assert.fail("NumberFormatException  should be received.");
        }
        catch (NumberFormatException e)
        {
            //normal execution
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received:" + e);
        }
    }


    private void assertBytesEqual(byte[] expected, byte[] actual)
    {
        Assert.assertEquals(expected.length, actual.length);

        for (int index = 0; index < expected.length; index++)
        {
            Assert.assertEquals(expected[index], actual[index]);
        }
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(MapMessageTest.class);
    }


}
