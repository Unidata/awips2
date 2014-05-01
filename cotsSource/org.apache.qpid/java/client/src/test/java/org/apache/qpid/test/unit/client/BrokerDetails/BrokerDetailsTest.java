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
 * 
 */
package org.apache.qpid.test.unit.client.BrokerDetails;

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.apache.qpid.client.AMQBrokerDetails;
import org.apache.qpid.client.AMQConnectionURL;
import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.url.URLSyntaxException;

public class BrokerDetailsTest extends TestCase
{
    public void testMultiParameters() throws URLSyntaxException
    {
        String url = "tcp://localhost:5672?timeout='200',immediatedelivery='true'";

        AMQBrokerDetails broker = new AMQBrokerDetails(url);

        assertTrue(broker.getProperty("timeout").equals("200"));
        assertTrue(broker.getProperty("immediatedelivery").equals("true"));
    }

    public void testVMBroker() throws URLSyntaxException
    {
        String url = "vm://:2";

        AMQBrokerDetails broker = new AMQBrokerDetails(url);
        assertTrue(broker.getTransport().equals("vm"));
        assertEquals(broker.getPort(), 2);
    }

    public void testTransportsDefaultToTCP() throws URLSyntaxException
    {
        String url = "localhost:5672";

        AMQBrokerDetails broker = new AMQBrokerDetails(url);
        assertTrue(broker.getTransport().equals("tcp"));
    }

    public void testCheckDefaultPort() throws URLSyntaxException
    {
        String url = "tcp://localhost";

        AMQBrokerDetails broker = new AMQBrokerDetails(url);
        assertTrue(broker.getPort() == AMQBrokerDetails.DEFAULT_PORT);
    }

    public void testBothDefaults() throws URLSyntaxException
    {
        String url = "localhost";

        AMQBrokerDetails broker = new AMQBrokerDetails(url);

        assertTrue(broker.getTransport().equals("tcp"));
        assertTrue(broker.getPort() == AMQBrokerDetails.DEFAULT_PORT);
    }

    public void testWrongOptionSeparatorInBroker()
    {
        String url = "tcp://localhost:5672+option='value'";
        try
        {
            new AMQBrokerDetails(url);
        }
        catch (URLSyntaxException urise)
        {
            assertTrue(urise.getReason().equals("Illegal character in port number"));
        }

    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(BrokerDetailsTest.class);
    }
}
