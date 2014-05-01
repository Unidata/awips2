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
package org.apache.qpid.test.unit.jndi;

import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQTopic;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.naming.spi.InitialContextFactory;
import java.util.Properties;
import java.io.InputStream;


import junit.framework.TestCase;

public class PropertiesFileInitialContextFactoryTest extends TestCase
{
    InitialContextFactory contextFactory;
    Properties _properties;
    Properties _fileProperties;

    protected void setUp() throws Exception
    {
        super.setUp();

        //create simple set of hardcoded props
        _properties = new Properties();
        _properties.put("java.naming.factory.initial", "org.apache.qpid.jndi.PropertiesFileInitialContextFactory");
        _properties.put("connectionfactory.local", "amqp://guest:guest@clientid/testpath?brokerlist='vm://:1'");
        _properties.put("queue.MyQueue", "example.MyQueue");
        _properties.put("topic.ibmStocks", "stocks.nyse.ibm");
        _properties.put("destination.direct", "direct://amq.direct//directQueue");

        //create properties from file as a more realistic test
        _fileProperties = new Properties();
        ClassLoader cl = this.getClass().getClassLoader();
        InputStream is = cl.getResourceAsStream("org/apache/qpid/test/unit/jndi/example.properties");
        _fileProperties.load(is);
    }

    /**
     * Test using hardcoded properties
     */
    public void testWithoutFile()
    {
        Context ctx = null;

        try
        {
            ctx = new InitialContext(_properties);
        }
        catch (NamingException ne)
        {
            fail("Error loading context:" + ne);
        }

        checkPropertiesMatch(ctx, "Using hardcoded properties: ");
    }

    /**
     * Test using properties from example file
     */
    public void testWithFile()
    {
        Context ctx = null;

        try
        {
            ctx = new InitialContext(_fileProperties);
        }
        catch (Exception e)
        {
            fail("Error loading context:" + e);
        }

        checkPropertiesMatch(ctx, "Using properties from file: ");
    }

    public void tearDown()
    {
        _properties = null;
        _fileProperties = null;
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(PropertiesFileInitialContextFactoryTest.class);
    }

    private void checkPropertiesMatch(Context ctx, String errorInfo)
    {
        try
        {
            AMQConnectionFactory cf = (AMQConnectionFactory) ctx.lookup("local");
            assertEquals("amqp://guest:guest@clientid/testpath?brokerlist='vm://:1'", cf.getConnectionURL().toString());
        }
        catch (NamingException ne)
        {
            fail(errorInfo + "Unable to create Connection Factory:" + ne);
        }

        try
        {
            AMQQueue queue = (AMQQueue) ctx.lookup("MyQueue");
            assertEquals("example.MyQueue", queue.getRoutingKey().toString());
        }
        catch (NamingException ne)
        {
            fail(errorInfo + "Unable to create queue:" + ne);
        }

        try
        {
            AMQTopic topic = (AMQTopic) ctx.lookup("ibmStocks");
            assertEquals("stocks.nyse.ibm", topic.getTopicName().toString());
        }
        catch (Exception ne)
        {
            fail(errorInfo + "Unable to create topic:" + ne);
        }

        try
        {
            AMQQueue direct = (AMQQueue) ctx.lookup("direct");
            assertEquals("directQueue", direct.getRoutingKey().toString());
        }
        catch (NamingException ne)
        {
            fail(errorInfo + "Unable to create direct destination:" + ne);
        }
    }
}
