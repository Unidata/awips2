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
package org.apache.qpid.transport;

import junit.framework.TestCase;

/**
 * GenTest
 *
 */

public class GenTest extends TestCase
{

    public void testBooleans()
    {
        QueueDeclare qd = new QueueDeclare().queue("test-queue").durable(false);
        assertEquals(qd.getQueue(), "test-queue");
        assertFalse("durable should be false", qd.getDurable());
        qd.setDurable(true);
        assertTrue("durable should be true", qd.getDurable());
        qd.setDurable(false);
        assertFalse("durable should be false again", qd.getDurable());
    }

}
