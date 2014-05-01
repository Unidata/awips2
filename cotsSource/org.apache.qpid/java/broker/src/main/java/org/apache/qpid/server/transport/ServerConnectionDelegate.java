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
package org.apache.qpid.server.transport;

import org.apache.qpid.transport.*;

import org.apache.qpid.server.registry.IApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;

import javax.security.sasl.SaslServer;
import javax.security.sasl.SaslException;
import java.util.*;


public class ServerConnectionDelegate extends ServerDelegate
{

    private String _localFQDN;
    private final IApplicationRegistry _appRegistry;


    public ServerConnectionDelegate(IApplicationRegistry appRegistry,
                                    String localFQDN)
    {
        this(Collections.EMPTY_MAP, Collections.singletonList((Object)"en_US"), appRegistry, localFQDN);
    }


    public ServerConnectionDelegate(Map<String, Object> properties,
                                    List<Object> locales,
                                    IApplicationRegistry appRegistry,
                                    String localFQDN)
    {
        super(properties, parseToList(appRegistry.getAuthenticationManager().getMechanisms()), locales);
        _appRegistry = appRegistry;
        _localFQDN = localFQDN;
    }

    private static List<Object> parseToList(String mechanisms)
    {
        List<Object> list = new ArrayList<Object>();
        StringTokenizer tokenizer = new StringTokenizer(mechanisms, " ");
        while(tokenizer.hasMoreTokens())
        {
            list.add(tokenizer.nextToken());
        }
        return list;
    }

    @Override public ServerSession getSession(Connection conn, SessionAttach atc)
    {

        SessionDelegate serverSessionDelegate = new ServerSessionDelegate(_appRegistry);

        ServerSession ssn = new ServerSession(conn, serverSessionDelegate,  new Binary(atc.getName()), 0);
        //ssn.setSessionListener(new Echo());
        return ssn;
    }




    @Override
    protected SaslServer createSaslServer(String mechanism) throws SaslException
    {
        return _appRegistry.getAuthenticationManager().createSaslServer(mechanism, _localFQDN);

    }


    @Override public void connectionOpen(Connection conn, ConnectionOpen open)
    {
        ServerConnection sconn = (ServerConnection) conn;

        VirtualHost vhost;
        String vhostName;
        if(open.hasVirtualHost())
        {
            vhostName = open.getVirtualHost();
        }
        else
        {
            vhostName = "";
        }
        vhost = _appRegistry.getVirtualHostRegistry().getVirtualHost(vhostName);

        if(vhost != null)
        {
            sconn.setVirtualHost(vhost);

            sconn.invoke(new ConnectionOpenOk(Collections.EMPTY_LIST));

            sconn.setState(Connection.State.OPEN);
        }
        else
        {
            sconn.invoke(new ConnectionClose(ConnectionCloseCode.INVALID_PATH, "Unknown vistrulhost '"+vhostName+"'"));
            sconn.setState(Connection.State.CLOSING);
        }

    }
}
