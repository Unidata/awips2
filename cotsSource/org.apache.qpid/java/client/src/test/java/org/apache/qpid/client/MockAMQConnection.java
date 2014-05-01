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

import org.apache.qpid.AMQException;
import org.apache.qpid.client.state.AMQState;
import org.apache.qpid.framing.ProtocolVersion;
import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.url.URLSyntaxException;

import java.io.IOException;

public class MockAMQConnection extends AMQConnection
{
    public MockAMQConnection(String broker, String username, String password, String clientName, String virtualHost)
            throws AMQException, URLSyntaxException
    {
        super(broker, username, password, clientName, virtualHost);
    }

    public MockAMQConnection(String broker, String username, String password, String clientName, String virtualHost, SSLConfiguration sslConfig)
            throws AMQException, URLSyntaxException
    {
        super(broker, username, password, clientName, virtualHost, sslConfig);
    }

    public MockAMQConnection(String host, int port, String username, String password, String clientName, String virtualHost)
            throws AMQException, URLSyntaxException
    {
        super(host, port, username, password, clientName, virtualHost);
    }

    public MockAMQConnection(String host, int port, String username, String password, String clientName, String virtualHost, SSLConfiguration sslConfig)
            throws AMQException, URLSyntaxException
    {
        super(host, port, username, password, clientName, virtualHost, sslConfig);
    }

    public MockAMQConnection(String host, int port, boolean useSSL, String username, String password, String clientName, String virtualHost, SSLConfiguration sslConfig)
            throws AMQException, URLSyntaxException
    {
        super(host, port, useSSL, username, password, clientName, virtualHost, sslConfig);
    }

    public MockAMQConnection(String connection)
            throws AMQException, URLSyntaxException
    {
        super(connection);
    }

    public MockAMQConnection(String connection, SSLConfiguration sslConfig)
            throws AMQException, URLSyntaxException
    {
        super(connection, sslConfig);
    }

    public MockAMQConnection(ConnectionURL connectionURL, SSLConfiguration sslConfig)
            throws AMQException
    {
        super(connectionURL, sslConfig);
    }

    protected MockAMQConnection(String username, String password, String clientName, String virtualHost)
    {
        super(username, password, clientName, virtualHost);
    }

    @Override
    public ProtocolVersion makeBrokerConnection(BrokerDetails brokerDetail) throws IOException
    {
        _connected = true;
        _protocolHandler.getStateManager().changeState(AMQState.CONNECTION_OPEN);
        return null;
    }
}
