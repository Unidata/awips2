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
package org.apache.qpid.server.exchange;

import junit.framework.TestCase;

import org.apache.qpid.management.common.mbeans.ManagedExchange;
import org.apache.qpid.server.queue.QueueRegistry;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.AMQQueueFactory;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.registry.IApplicationRegistry;
import org.apache.qpid.server.management.ManagedObject;
import org.apache.qpid.server.virtualhost.VirtualHostImpl;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;

import javax.management.openmbean.TabularData;
import java.util.ArrayList;

/**
 * Unit test class for testing different Exchange MBean operations
 */
public class ExchangeMBeanTest  extends TestCase
{
    private AMQQueue _queue;
    private QueueRegistry _queueRegistry;
    private VirtualHost _virtualHost;

    /**
     * Test for direct exchange mbean
     * @throws Exception
     */

    public void testDirectExchangeMBean() throws Exception
    {
        DirectExchange exchange = new DirectExchange();
        exchange.initialise(_virtualHost, ExchangeDefaults.DIRECT_EXCHANGE_NAME, false, 0, true);
        ManagedObject managedObj = exchange.getManagedObject();
        ManagedExchange mbean = (ManagedExchange)managedObj;

        mbean.createNewBinding(_queue.getName().toString(), "binding1");
        mbean.createNewBinding(_queue.getName().toString(), "binding2");

        TabularData data = mbean.bindings();
        ArrayList<Object> list = new ArrayList<Object>(data.values());
        assertTrue(list.size() == 2);

        // test general exchange properties
        assertEquals(mbean.getName(), "amq.direct");
        assertEquals(mbean.getExchangeType(), "direct");
        assertTrue(mbean.getTicketNo() == 0);
        assertTrue(!mbean.isDurable());
        assertTrue(mbean.isAutoDelete());
    }

    /**
     * Test for "topic" exchange mbean
     * @throws Exception
     */

    public void testTopicExchangeMBean() throws Exception
    {
        TopicExchange exchange = new TopicExchange();
        exchange.initialise(_virtualHost,ExchangeDefaults.TOPIC_EXCHANGE_NAME, false, 0, true);
        ManagedObject managedObj = exchange.getManagedObject();
        ManagedExchange mbean = (ManagedExchange)managedObj;

        mbean.createNewBinding(_queue.getName().toString(), "binding1");
        mbean.createNewBinding(_queue.getName().toString(), "binding2");

        TabularData data = mbean.bindings();
        ArrayList<Object> list = new ArrayList<Object>(data.values());
        assertTrue(list.size() == 2);

        // test general exchange properties
        assertEquals(mbean.getName(), "amq.topic");
        assertEquals(mbean.getExchangeType(), "topic");
        assertTrue(mbean.getTicketNo() == 0);
        assertTrue(!mbean.isDurable());
        assertTrue(mbean.isAutoDelete());
    }

    /**
     * Test for "Headers" exchange mbean
     * @throws Exception
     */

    public void testHeadersExchangeMBean() throws Exception
    {
        HeadersExchange exchange = new HeadersExchange();
        exchange.initialise(_virtualHost,ExchangeDefaults.HEADERS_EXCHANGE_NAME, false, 0, true);
        ManagedObject managedObj = exchange.getManagedObject();
        ManagedExchange mbean = (ManagedExchange)managedObj;

        mbean.createNewBinding(_queue.getName().toString(), "key1=binding1,key2=binding2");
        mbean.createNewBinding(_queue.getName().toString(), "key3=binding3");

        TabularData data = mbean.bindings();
        ArrayList<Object> list = new ArrayList<Object>(data.values());
        assertTrue(list.size() == 2);

        // test general exchange properties
        assertEquals(mbean.getName(), "amq.match");
        assertEquals(mbean.getExchangeType(), "headers");
        assertTrue(mbean.getTicketNo() == 0);
        assertTrue(!mbean.isDurable());
        assertTrue(mbean.isAutoDelete());
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();

        IApplicationRegistry applicationRegistry = ApplicationRegistry.getInstance();
        _virtualHost = applicationRegistry.getVirtualHostRegistry().getVirtualHost("test");
        _queueRegistry = _virtualHost.getQueueRegistry();
        _queue = AMQQueueFactory.createAMQQueueImpl(new AMQShortString("testQueue"), false, new AMQShortString("ExchangeMBeanTest"), false, _virtualHost,
                                                    null);
        _queueRegistry.registerQueue(_queue);
    }

    protected void tearDown()
    {
        // Correctly Close the AR that we created above
        ApplicationRegistry.remove();
    }

}
