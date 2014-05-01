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
package org.apache.qpid.framing.abstraction;

import junit.framework.TestCase;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.abstraction.MessagePublishInfoImpl;

public class MessagePublishInfoImplTest extends TestCase
{
    MessagePublishInfoImpl _mpi;
    final AMQShortString _exchange = new AMQShortString("exchange");
    final AMQShortString _routingKey = new AMQShortString("routingKey");

    public void setUp()
    {
        _mpi = new MessagePublishInfoImpl(_exchange, true, true, _routingKey);
    }

    /** Test that we can update the exchange value. */
    public void testExchange()
    {
        assertEquals(_exchange, _mpi.getExchange());
        AMQShortString newExchange = new AMQShortString("newExchange");
        //Check we can update the exchange
        _mpi.setExchange(newExchange);
        assertEquals(newExchange, _mpi.getExchange());
        //Ensure that the new exchange doesn't equal the old one
        assertFalse(_exchange.equals(_mpi.getExchange()));
    }

    /**
     * Check that the immedate value is set correctly and defaulted correctly
     */
    public void testIsImmediate()
    {
        //Check that the set value is correct
        assertTrue("Set value for immediate not as expected", _mpi.isImmediate());

        MessagePublishInfoImpl mpi = new MessagePublishInfoImpl();

        assertFalse("Default value for immediate should be false", mpi.isImmediate());

        mpi.setImmediate(true);
        
        assertTrue("Updated value for immediate not as expected", mpi.isImmediate());

    }

    /**
     * Check that the mandatory value is set correctly and defaulted correctly
     */
    public void testIsMandatory()
    {
        assertTrue("Set value for mandatory not as expected", _mpi.isMandatory());

        MessagePublishInfoImpl mpi = new MessagePublishInfoImpl();

        assertFalse("Default value for mandatory should be false", mpi.isMandatory());

        mpi.setMandatory(true);

        assertTrue("Updated value for mandatory not as expected", mpi.isMandatory());
    }

    /**
     * Check that the routingKey value is perserved
     */
    public void testRoutingKey()
    {
        assertEquals(_routingKey, _mpi.getRoutingKey());
        AMQShortString newRoutingKey = new AMQShortString("newRoutingKey");

        //Check we can update the routingKey
        _mpi.setRoutingKey(newRoutingKey);
        assertEquals(newRoutingKey, _mpi.getRoutingKey());
        //Ensure that the new routingKey doesn't equal the old one
        assertFalse(_routingKey.equals(_mpi.getRoutingKey()));

    }
}
