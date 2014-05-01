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
package org.apache.qpid.server.queue;

import junit.framework.TestCase;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;

public class AMQQueueFactoryTest extends TestCase
{
    QueueRegistry _queueRegistry;
    VirtualHost _virtualHost;

    public void setUp()
    {
        ApplicationRegistry registry = (ApplicationRegistry) ApplicationRegistry.getInstance();

        _virtualHost = registry.getVirtualHostRegistry().getVirtualHost("test");

        _queueRegistry = _virtualHost.getQueueRegistry();

        assertEquals("Queues registered on an empty virtualhost", 0, _queueRegistry.getQueues().size());
    }

    public void tearDown()
    {
        assertEquals("Queue was not registered in virtualhost", 1, _queueRegistry.getQueues().size());
        ApplicationRegistry.remove();
    }


    public void testPriorityQueueRegistration()
    {
        FieldTable fieldTable = new FieldTable();
        fieldTable.put(new AMQShortString(AMQQueueFactory.X_QPID_PRIORITIES), 5);


        AMQQueue queue = AMQQueueFactory.createAMQQueueImpl(new AMQShortString("testPriorityQueue"), false, new AMQShortString("owner"), false,
                                           _virtualHost, fieldTable);

        assertEquals("Queue not a priorty queue", AMQPriorityQueue.class, queue.getClass());
    }


    public void testSimpleQueueRegistration()
    {
        AMQQueue queue = AMQQueueFactory.createAMQQueueImpl(new AMQShortString("testQueue"), false, new AMQShortString("owner"), false,
                                           _virtualHost, null);
        assertEquals("Queue not a simple queue", SimpleAMQQueue.class, queue.getClass());
    }
}
