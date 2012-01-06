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

import junit.framework.TestCase;
import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.url.URLSyntaxException;

public class ConnectionFactoryTest extends TestCase
{

    //URL will be returned with the password field swapped for '********'
    // so ensure that these two strings are kept in sync.
    public static final String URL = "amqp://guest:guest@clientID/test?brokerlist='tcp://localhost:5672'";
    public static final String URL_STAR_PWD = "amqp://guest:********@clientID/test?brokerlist='tcp://localhost:5672'";

    public void testConnectionURLString()
    {
        AMQConnectionFactory factory = new AMQConnectionFactory();

        assertNull("ConnectionURL should have no value at start",
                   factory.getConnectionURL());

        try
        {
            factory.setConnectionURLString(URL);
        }
        catch (URLSyntaxException e)
        {
            fail(e.getMessage());
        }

        //URL will be returned with the password field swapped for '********'
        assertEquals("Connection URL not correctly set", URL_STAR_PWD, factory.getConnectionURLString());

        // Further test that the processed ConnectionURL is as expected after
        // the set call
        ConnectionURL connectionurl = factory.getConnectionURL();

        assertNull("Failover is set.", connectionurl.getFailoverMethod());
        assertEquals("guest", connectionurl.getUsername());
        assertEquals("guest", connectionurl.getPassword());
        assertEquals("clientID", connectionurl.getClientName());
        assertEquals("/test", connectionurl.getVirtualHost());

        assertEquals(1, connectionurl.getBrokerCount());

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertEquals("tcp", service.getTransport());
        assertEquals("localhost", service.getHost());
        assertEquals(5672, service.getPort());

    }
}
