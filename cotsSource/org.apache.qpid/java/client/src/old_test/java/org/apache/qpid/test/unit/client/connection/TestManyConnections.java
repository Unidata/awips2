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
package org.apache.qpid.test.unit.client.connection;

import org.apache.qpid.AMQException;
import org.apache.qpid.url.URLSyntaxException;
import org.apache.qpid.client.AMQConnection;
import org.apache.log4j.Logger;

import junit.framework.TestCase;

public class TestManyConnections extends TestCase
{
    private static final Logger _log = Logger.getLogger(TestManyConnections.class);

    private AMQConnection[] _connections;

    private void createConnection(int index, String brokerHosts, String clientID, String username, String password,
                                  String vpath) throws AMQException, URLSyntaxException
    {
        _connections[index] = new AMQConnection(brokerHosts, username, password,
                                                clientID, vpath);
    }

    private void createConnections(int count) throws AMQException, URLSyntaxException
    {
        _connections = new AMQConnection[count];
        long startTime = System.currentTimeMillis();
        for (int i = 0; i < count; i++)
        {
            createConnection(i, "vm://:1", "myClient" + i, "guest", "guest", "test");
        }
        long endTime = System.currentTimeMillis();
        _log.info("Time to create " + count + " connections: " + (endTime - startTime) +
                  "ms");
    }

    public void testCreate10Connections() throws AMQException, URLSyntaxException
    {
        createConnections(10);
    }

    public void testCreate50Connections() throws AMQException, URLSyntaxException
    {
        createConnections(50);
    }

    public void testCreate100Connections() throws AMQException, URLSyntaxException
    {
        createConnections(100);
    }

    public void testCreate250Connections() throws AMQException, URLSyntaxException
    {
        createConnections(250);
    }

    public void testCreate500Connections() throws AMQException, URLSyntaxException
    {
        createConnections(500);
    }

    public void testCreate1000Connections() throws AMQException, URLSyntaxException
    {
        createConnections(1000);
    }

    public void testCreate5000Connections() throws AMQException, URLSyntaxException
    {
        createConnections(5000);
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(TestManyConnections.class);
    }
}
