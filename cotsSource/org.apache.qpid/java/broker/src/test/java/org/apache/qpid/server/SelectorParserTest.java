package org.apache.qpid.server;

import junit.framework.TestCase;
import org.apache.qpid.server.filter.JMSSelectorFilter;
import org.apache.qpid.AMQException;/*
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

public class SelectorParserTest extends TestCase
{
    public void testSelectorWithHyphen()
    {
        testPass("Cost = 2 AND \"property-with-hyphen\" = 'wibble'");
    }

    public void testLike()
    {        
        testFail("Cost LIKE 2");
        testPass("Cost LIKE 'Hello'");
    }

    public void testStringQuoted()
    {
        testPass("string = 'Test'");
    }

    public void testProperty()
    {
        testPass("prop1 = prop2");
    }

    public void testPropertyNames()
    {
        testPass("$min= TRUE AND _max= FALSE AND Prop_2 = true AND prop$3 = false");
    }

    public void testProtected()
    {
        testFail("NULL = 0 ");
        testFail("TRUE = 0 ");
        testFail("FALSE = 0 ");
        testFail("NOT = 0 ");
        testFail("AND = 0 ");
        testFail("OR = 0 ");
        testFail("BETWEEN = 0 ");
        testFail("LIKE = 0 ");
        testFail("IN = 0 ");
        testFail("IS = 0 ");
        testFail("ESCAPE = 0 ");
   }


    public void testBoolean()
    {
        testPass("min= TRUE  AND max= FALSE ");
        testPass("min= true AND max= false");
    }

    public void testDouble()
    {
        testPass("positive=31E2 AND negative=-31.4E3");
        testPass("min=" + Double.MIN_VALUE + " AND max=" + Double.MAX_VALUE);
    }

    public void testLong()
    {
        testPass("minLong=" + Long.MIN_VALUE + "L AND maxLong=" + Long.MAX_VALUE + "L");
    }

    public void testInt()
    {
        testPass("minInt=" + Integer.MIN_VALUE + " AND maxInt=" + Integer.MAX_VALUE);
    }

    public void testSigned()
    {
        testPass("negative=-42 AND positive=+42");
    }

    public void testOctal()
    {
        testPass("octal=042");
    }


    private void testPass(String selector)
    {
        try
        {
            new JMSSelectorFilter(selector);
        }
        catch (AMQException e)
        {
            fail("Selector '" + selector + "' was not parsed :" + e.getMessage());
        }
    }

    private void testFail(String selector)
    {
        try
        {
            new JMSSelectorFilter(selector);
            fail("Selector '" + selector + "' was parsed ");
        }
        catch (AMQException e)
        {
            //normal path
        }
    }

}
