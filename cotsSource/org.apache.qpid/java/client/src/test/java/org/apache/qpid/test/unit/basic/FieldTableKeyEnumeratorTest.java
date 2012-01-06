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

import java.util.Enumeration;
import java.util.Iterator;
import java.util.NoSuchElementException;

import javax.jms.JMSException;

import junit.framework.TestCase;

import org.apache.qpid.client.message.JMSTextMessage;
import org.apache.qpid.client.message.TestMessageHelper;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.FieldTableFactory;

public class FieldTableKeyEnumeratorTest extends TestCase
{
    public void testTrue()
    {
        
    }
    public void testKeyEnumeration()
    {
        FieldTable result = FieldTableFactory.newFieldTable();
        result.setObject("one", 1L);
        result.setObject("two", 2L);
        result.setObject("three", 3L);
        result.setObject("four", 4L);
        result.setObject("five", 5L);

        Iterator iterator = result.keys().iterator();

        try
        {
            assertTrue("one".equals(iterator.next()));
            assertTrue("two".equals(iterator.next()));
            assertTrue("three".equals(iterator.next()));
            assertTrue("four".equals(iterator.next()));
            assertTrue("five".equals(iterator.next()));
        }
        catch (NoSuchElementException e)
        {
            fail("All elements should be found.");
        }

    }

    public void testPropertEnu()
    {
        try
        {
            JMSTextMessage text = TestMessageHelper.newJMSTextMessage();

            text.setBooleanProperty("Boolean1", true);
            text.setBooleanProperty("Boolean2", true);
            text.setIntProperty("Int", 2);
            text.setLongProperty("Long", 2);

            Enumeration e = text.getPropertyNames();

            assertTrue("Boolean1".equals(e.nextElement()));
            assertTrue("Boolean2".equals(e.nextElement()));
            assertTrue("Int".equals(e.nextElement()));
            assertTrue("Long".equals(e.nextElement()));
        }
        catch (JMSException e)
        {

        }
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(FieldTableKeyEnumeratorTest.class);
    }
}
