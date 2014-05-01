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

import javax.jms.JMSException;

import junit.framework.TestCase;

import org.apache.qpid.client.message.JMSTextMessage;
import org.apache.qpid.client.message.TestMessageHelper;

public class FieldTablePropertyTest extends TestCase
{
    public void testPropertyNames()
    {
        try
        {
            JMSTextMessage text = TestMessageHelper.newJMSTextMessage();

            text.setBooleanProperty("Boolean1", true);
            text.setBooleanProperty("Boolean2", true);
            text.setIntProperty("Int", 2);
            text.setLongProperty("Long", 2);

            Enumeration e = text.getPropertyNames();

            assertEquals("Boolean1", e.nextElement());
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
        return new junit.framework.TestSuite(FieldTablePropertyTest.class);
    }
}
