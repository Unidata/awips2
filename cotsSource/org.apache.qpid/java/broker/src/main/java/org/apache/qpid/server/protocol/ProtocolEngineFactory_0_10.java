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
package org.apache.qpid.server.protocol;

import org.apache.qpid.protocol.ProtocolEngineFactory;
import org.apache.qpid.protocol.ProtocolEngine;
import org.apache.qpid.transport.NetworkDriver;
import org.apache.qpid.transport.Connection;
import org.apache.qpid.transport.ConnectionDelegate;
import org.apache.qpid.server.transport.ServerConnection;
import org.apache.qpid.server.protocol.ProtocolEngine_0_10;

public class ProtocolEngineFactory_0_10 implements ProtocolEngineFactory
{
    private ConnectionDelegate _delegate;

    public ProtocolEngineFactory_0_10(ConnectionDelegate delegate)
    {
        _delegate = delegate;
    }

    public ProtocolEngine newProtocolEngine(NetworkDriver networkDriver)
    {
        Connection conn = new ServerConnection();
        conn.setConnectionDelegate(_delegate);
        return new ProtocolEngine_0_10(conn, networkDriver);
    }
}
