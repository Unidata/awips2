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
 */
package org.apache.qpid.server.configuration;


import junit.framework.TestCase;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.queue.AMQPriorityQueue;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.virtualhost.VirtualHostImpl;

public class VirtualHostConfigurationTest extends TestCase
{

    private VirtualHostConfiguration vhostConfig;
    private XMLConfiguration  configXml;

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        //Highlight that this test will cause a new AR to be created
        ApplicationRegistry.getInstance();
        // Fill config file with stuff
        configXml = new XMLConfiguration();
        configXml.setRootElementName("virtualhosts");
        configXml.addProperty("virtualhost(-1).name", "test");
    }

    public void tearDown() throws Exception
    {
        //Correctly close the AR we created
        ApplicationRegistry.remove();

        super.tearDown();
    }

    public void testQueuePriority() throws Exception
    {
        // Set up queue with 5 priorities
        configXml.addProperty("virtualhost.test.queues(-1).queue(-1).name(-1)",
                              "atest");
        configXml.addProperty("virtualhost.test.queues.queue.atest(-1).exchange",
                              "amq.direct");
        configXml.addProperty("virtualhost.test.queues.queue.atest.priorities",
                              "5");

        // Set up queue with JMS style priorities
        configXml.addProperty("virtualhost.test.queues(-1).queue(-1).name(-1)",
                              "ptest");
        configXml.addProperty("virtualhost.test.queues.queue.ptest(-1).exchange",
                              "amq.direct");
        configXml.addProperty("virtualhost.test.queues.queue.ptest.priority",
                               "true");

        // Set up queue with no priorities
        configXml.addProperty("virtualhost.test.queues(-1).queue(-1).name(-1)",
                              "ntest");
        configXml.addProperty("virtualhost.test.queues.queue.ntest(-1).exchange",
                              "amq.direct");
        configXml.addProperty("virtualhost.test.queues.queue.ntest.priority",
                              "false");

        VirtualHost vhost = new VirtualHostImpl(new VirtualHostConfiguration("test", configXml.subset("virtualhost.test")));

        // Check that atest was a priority queue with 5 priorities
        AMQQueue atest = vhost.getQueueRegistry().getQueue(new AMQShortString("atest"));
        assertTrue(atest instanceof AMQPriorityQueue);
        assertEquals(5, ((AMQPriorityQueue) atest).getPriorities());

        // Check that ptest was a priority queue with 10 priorities
        AMQQueue ptest = vhost.getQueueRegistry().getQueue(new AMQShortString("ptest"));
        assertTrue(ptest instanceof AMQPriorityQueue);
        assertEquals(10, ((AMQPriorityQueue) ptest).getPriorities());

        // Check that ntest wasn't a priority queue
        AMQQueue ntest = vhost.getQueueRegistry().getQueue(new AMQShortString("ntest"));
        assertFalse(ntest instanceof AMQPriorityQueue);
    }

    public void testQueueAlerts() throws Exception
    {
        // Set up queue with 5 priorities
        configXml.addProperty("virtualhost.test.queues.exchange", "amq.topic");
        configXml.addProperty("virtualhost.test.queues.maximumQueueDepth", "1");
        configXml.addProperty("virtualhost.test.queues.maximumMessageSize", "2");
        configXml.addProperty("virtualhost.test.queues.maximumMessageAge", "3");

        configXml.addProperty("virtualhost.test.queues(-1).queue(1).name(1)", "atest");
        configXml.addProperty("virtualhost.test.queues.queue.atest(-1).exchange", "amq.direct");
        configXml.addProperty("virtualhost.test.queues.queue.atest(-1).maximumQueueDepth", "4");
        configXml.addProperty("virtualhost.test.queues.queue.atest(-1).maximumMessageSize", "5");
        configXml.addProperty("virtualhost.test.queues.queue.atest(-1).maximumMessageAge", "6");

        configXml.addProperty("virtualhost.test.queues(-1).queue(-1).name(-1)", "btest");

        VirtualHost vhost = new VirtualHostImpl(new VirtualHostConfiguration("test", configXml.subset("virtualhost.test")));

        // Check specifically configured values
        AMQQueue aTest = vhost.getQueueRegistry().getQueue(new AMQShortString("atest"));
        assertEquals(4, aTest.getMaximumQueueDepth());
        assertEquals(5, aTest.getMaximumMessageSize());
        assertEquals(6, aTest.getMaximumMessageAge());

        // Check default values
        AMQQueue bTest = vhost.getQueueRegistry().getQueue(new AMQShortString("btest"));
        assertEquals(1, bTest.getMaximumQueueDepth());
        assertEquals(2, bTest.getMaximumMessageSize());
        assertEquals(3, bTest.getMaximumMessageAge());

    }

}
