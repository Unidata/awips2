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
package org.apache.qpid.test.unit.client.message;

import javax.jms.JMSException;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.apache.qpid.client.message.JMSMapMessage;
import org.apache.qpid.client.message.JMSTextMessage;
import org.apache.qpid.client.message.TestMessageHelper;

public class TextMessageTest extends TestCase
{
    public void testTextOnConstruction() throws Exception
    {
        JMSTextMessage tm = TestMessageHelper.newJMSTextMessage();
        tm.setText("pies");
        String val = tm.getText();
        assertEquals(val, "pies");
    }

    public void testClearBody() throws Exception
    {
        JMSTextMessage tm = TestMessageHelper.newJMSTextMessage();
        tm.setText("pies");
        tm.clearBody();
        String val = tm.getText();
        assertNull(val);
        tm.setText("Banana");
        val = tm.getText();
        assertEquals(val, "Banana");
    }


    public void testBooleanPropertyLookup()
    {
        try
        {
            JMSTextMessage tm = TestMessageHelper.newJMSTextMessage();

            tm.setBooleanProperty("value", true);
            Assert.assertEquals(true, tm.getBooleanProperty("value"));
            Assert.assertEquals("true", tm.getStringProperty("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testBytePropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setByteProperty("value", Byte.MAX_VALUE);

            Assert.assertEquals(Byte.MAX_VALUE, mm.getByteProperty("value"));
            Assert.assertEquals((short) Byte.MAX_VALUE, mm.getShortProperty("value"));
            Assert.assertEquals(Byte.MAX_VALUE, mm.getIntProperty("value"));
            Assert.assertEquals((long) Byte.MAX_VALUE, mm.getLongProperty("value"));
            Assert.assertEquals("" + Byte.MAX_VALUE, mm.getStringProperty("value"));

        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testShortPropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setShortProperty("value", Short.MAX_VALUE);
            Assert.assertEquals(Short.MAX_VALUE, mm.getShortProperty("value"));
            Assert.assertEquals((int) Short.MAX_VALUE, mm.getIntProperty("value"));
            Assert.assertEquals((long) Short.MAX_VALUE, mm.getLongProperty("value"));
            Assert.assertEquals("" + Short.MAX_VALUE, mm.getStringProperty("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testDoublePropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setDoubleProperty("value", Double.MAX_VALUE);
            Assert.assertEquals(Double.MAX_VALUE, mm.getDoubleProperty("value"));
            Assert.assertEquals("" + Double.MAX_VALUE, mm.getStringProperty("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testFloatPropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setFloatProperty("value", Float.MAX_VALUE);
            Assert.assertEquals(Float.MAX_VALUE, mm.getFloatProperty("value"));
            Assert.assertEquals((double) Float.MAX_VALUE, mm.getDoubleProperty("value"));
            Assert.assertEquals("" + Float.MAX_VALUE, mm.getStringProperty("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testIntPropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setIntProperty("value", Integer.MAX_VALUE);
            Assert.assertEquals(Integer.MAX_VALUE, mm.getIntProperty("value"));
            Assert.assertEquals((long) Integer.MAX_VALUE, mm.getLongProperty("value"));
            Assert.assertEquals("" + Integer.MAX_VALUE, mm.getStringProperty("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testLongPropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.setLongProperty("value", Long.MAX_VALUE);
            Assert.assertEquals(Long.MAX_VALUE, mm.getLongProperty("value"));
            Assert.assertEquals("" + Long.MAX_VALUE, mm.getStringProperty("value"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }


    // Failed Lookups

    public void testFailedBooleanPropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            Assert.assertEquals(false, mm.getBooleanProperty("int"));
        }
        catch (JMSException e)
        {
            Assert.fail("JMSException received." + e);
        }
    }

    public void testFailedBytePropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getByteProperty("random");
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

    public void testFailedDoublePropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getDoubleProperty("random");
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

    public void testFailedFloatPropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getFloatProperty("random");
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

    public void testFailedIntPropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getIntProperty("random");
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

    public void testFailedLongPropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getLongProperty("random");
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

    public void testFailedShortPropertyLookup()
    {
        try
        {
            JMSMapMessage mm = TestMessageHelper.newJMSMapMessage();
            mm.getShortProperty("random");
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


    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(TextMessageTest.class);
    }
}
