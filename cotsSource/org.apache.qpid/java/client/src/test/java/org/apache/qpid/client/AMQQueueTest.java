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
package org.apache.qpid.client;

import org.apache.qpid.framing.AMQShortString;

import junit.framework.TestCase;

public class AMQQueueTest extends TestCase
{
    AMQShortString exchange = new AMQShortString("test.exchange");
    AMQShortString routingkey = new AMQShortString("test-route");
    AMQShortString qname = new AMQShortString("test-queue");
    AMQShortString[] oneBinding = new AMQShortString[]{new AMQShortString("bindingA")};
    AMQShortString[] bindings = new AMQShortString[]{new AMQShortString("bindingB"), 
                                                     new AMQShortString("bindingC")};

    public void testToURLNoBindings()
    {
        AMQQueue dest = new AMQQueue(exchange, routingkey, qname);
        String url = dest.toURL();
        assertEquals("direct://test.exchange/test-route/test-queue?routingkey='test-route'", url);
    }
}
