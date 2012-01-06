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
package org.apache.qpid.server.connection;

import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.AMQException;
import org.apache.qpid.AMQConnectionException;
import org.apache.qpid.protocol.AMQConstant;

import java.util.concurrent.CopyOnWriteArrayList;
import java.util.List;

public class ConnectionRegistry implements IConnectionRegistry
{
    private List<AMQProtocolSession> _registry = new CopyOnWriteArrayList<AMQProtocolSession>();

    private VirtualHost _virtualHost;

    public ConnectionRegistry(VirtualHost virtualHost)
    {
        _virtualHost = virtualHost;
    }

    public void initialise()
    {

    }
    
    public void expireClosedChannels()
    {
        for (AMQProtocolSession connection : _registry)
        {
            connection.closeIfLingeringClosedChannels();
        }
    }

    /** Close all of the currently open connections. */
    public void close() throws AMQException
    {
        while (!_registry.isEmpty())
        {
            AMQProtocolSession connection = _registry.get(0);

            connection.closeConnection(0, new AMQConnectionException(AMQConstant.INTERNAL_ERROR, "Broker is shutting down",
                                                                  0, 0,
                                                                  connection.getProtocolOutputConverter().getProtocolMajorVersion(),
                                                                  connection.getProtocolOutputConverter().getProtocolMinorVersion(),
                                                                  (Throwable) null), true);
        }
    }

    public void registerConnection(AMQProtocolSession connnection)
    {
        _registry.add(connnection);
    }

    public void deregisterConnection(AMQProtocolSession connnection)
    {
        _registry.remove(connnection);
    }
}
