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

import static org.apache.qpid.transport.Connection.State.OPEN;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.security.sasl.Sasl;
import javax.security.sasl.SaslClient;
import javax.security.sasl.SaslException;

import org.apache.qpid.security.UsernamePasswordCallbackHandler;
import org.apache.qpid.transport.util.Logger;


/**
 * ClientDelegate
 *
 */

public class ClientDelegate extends ConnectionDelegate
{
    private static final Logger log = Logger.get(ClientDelegate.class);

    private String vhost;
    private String username;
    private String password;
    private String[] saslMechs;
    private String protocol;
    private String serverName;
    
    public ClientDelegate(String vhost, String username, String password,String saslMechs)
    {
        this.vhost = vhost;
        this.username = username;
        this.password = password;
        this.saslMechs = saslMechs.split(" ");
        
        // Looks kinda of silly but the Sun SASL Kerberos client uses the 
        // protocol + servername as the service key.
        this.protocol = System.getProperty("qpid.sasl_protocol","AMQP");
        this.serverName = System.getProperty("qpid.sasl_server_name","localhost");
    }

    public void init(Connection conn, ProtocolHeader hdr)
    {
        if (!(hdr.getMajor() == 0 && hdr.getMinor() == 10))
        {
            conn.exception(new ProtocolVersionException(hdr.getMajor(), hdr.getMinor()));
        }
    }

    @Override public void connectionStart(Connection conn, ConnectionStart start)
    {
        Map clientProperties = new HashMap();
        clientProperties.put("qpid.session_flow", 1);

        List<Object> mechanisms = start.getMechanisms();
        if (mechanisms == null || mechanisms.isEmpty())
        {
            conn.connectionStartOk
                (clientProperties, null, null, conn.getLocale());
            return;
        }

        String[] mechs = new String[mechanisms.size()];
        mechanisms.toArray(mechs);

        try
        {
            UsernamePasswordCallbackHandler handler =
                new UsernamePasswordCallbackHandler();
            handler.initialise(username, password);
            SaslClient sc = Sasl.createSaslClient
                (saslMechs, null, protocol, serverName, null, handler);
            conn.setSaslClient(sc);

            byte[] response = sc.hasInitialResponse() ?
                sc.evaluateChallenge(new byte[0]) : null;
            conn.connectionStartOk
                (clientProperties, sc.getMechanismName(), response,
                 conn.getLocale());
        }
        catch (SaslException e)
        {
            conn.exception(e);
        }
    }

    @Override public void connectionSecure(Connection conn, ConnectionSecure secure)
    {
        SaslClient sc = conn.getSaslClient();
        try
        {
            byte[] response = sc.evaluateChallenge(secure.getChallenge());
            conn.connectionSecureOk(response);
        }
        catch (SaslException e)
        {
            conn.exception(e);
        }
    }

    @Override public void connectionTune(Connection conn, ConnectionTune tune)
    {
        conn.setChannelMax(tune.getChannelMax());
        int hb_interval = calculateHeartbeatInterval(conn,
                                                     tune.getHeartbeatMin(),
                                                     tune.getHeartbeatMax()
                                                     );
        conn.connectionTuneOk(tune.getChannelMax(), 
                              tune.getMaxFrameSize(), 
                              hb_interval);
        conn.setIdleTimeout(hb_interval*1000);
        conn.connectionOpen(vhost, null, Option.INSIST);
    }

    @Override public void connectionOpenOk(Connection conn, ConnectionOpenOk ok)
    {
        conn.setState(OPEN);
    }

    @Override public void connectionRedirect(Connection conn, ConnectionRedirect redir)
    {
        throw new UnsupportedOperationException();
    }

    @Override public void connectionHeartbeat(Connection conn, ConnectionHeartbeat hearbeat)
    {
        conn.connectionHeartbeat();
    }

    /**
     * Currently the spec specified the min and max for heartbeat using secs
     */
    private int calculateHeartbeatInterval(Connection conn,int min, int max)
    {
        long l = conn.getIdleTimeout()/1000;
        if (l !=0 && l >= min && l <= max)
        {
            return (int)l;
        }
        else
        {
            log.warn("Ignoring the idle timeout %s set by the connection," +
            		" using the brokers max value %s", l,max);
            return max;
        }
    }
}
