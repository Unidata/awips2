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
package org.apache.qpid.server.security.auth.sasl.crammd5;

import javax.security.sasl.SaslServer;
import javax.security.sasl.SaslException;
import javax.security.sasl.Sasl;
import javax.security.sasl.SaslServerFactory;
import javax.security.auth.callback.CallbackHandler;
import java.util.Enumeration;
import java.util.Map;

public class CRAMMD5HexSaslServer implements SaslServer
{
    public static final String MECHANISM = "CRAM-MD5-HEX";

    private SaslServer _realServer;

    public CRAMMD5HexSaslServer(String mechanism, String protocol, String serverName, Map<String, ?> props,
                                   CallbackHandler cbh) throws SaslException
    {
        Enumeration factories = Sasl.getSaslServerFactories();

        while (factories.hasMoreElements())
        {
            SaslServerFactory factory = (SaslServerFactory) factories.nextElement();

            if (factory instanceof CRAMMD5HexServerFactory)
            {
                continue;
            }
            
            String[] mechs = factory.getMechanismNames(props);

            for (String mech : mechs)
            {
                if (mech.equals("CRAM-MD5"))
                {
                    _realServer = factory.createSaslServer("CRAM-MD5", protocol, serverName, props, cbh);
                    return;
                }
            }
        }

        throw new RuntimeException("No default SaslServer found for mechanism:" + "CRAM-MD5");
    }

    public String getMechanismName()
    {
        return MECHANISM;
    }

    public byte[] evaluateResponse(byte[] response) throws SaslException
    {
        return _realServer.evaluateResponse(response);
    }

    public boolean isComplete()
    {
        return _realServer.isComplete();
    }

    public String getAuthorizationID()
    {
        return _realServer.getAuthorizationID();
    }

    public byte[] unwrap(byte[] incoming, int offset, int len) throws SaslException
    {
        return _realServer.unwrap(incoming, offset, len);
    }

    public byte[] wrap(byte[] outgoing, int offset, int len) throws SaslException
    {
        return _realServer.wrap(outgoing, offset, len);
    }

    public Object getNegotiatedProperty(String propName)
    {
        return _realServer.getNegotiatedProperty(propName);
    }

    public void dispose() throws SaslException
    {
        _realServer.dispose();
    }
}
