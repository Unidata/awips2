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
package org.apache.qpid.client.security.amqplain;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.sasl.SaslClient;
import javax.security.sasl.SaslException;

import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.FieldTableFactory;

/**
 * Implements the "AMQPlain" authentication protocol that uses FieldTables to send username and pwd.
 *
 */
public class AmqPlainSaslClient implements SaslClient
{
    /**
     *  The name of this mechanism
     */
    public static final String MECHANISM = "AMQPLAIN";

    private CallbackHandler _cbh;

    public AmqPlainSaslClient(CallbackHandler cbh)
    {
        _cbh = cbh;
    }

    public String getMechanismName()
    {
        return "AMQPLAIN";
    }

    public boolean hasInitialResponse()
    {
        return true;
    }

    public byte[] evaluateChallenge(byte[] challenge) throws SaslException
    {
        // we do not care about the prompt or the default name
        NameCallback nameCallback = new NameCallback("prompt", "defaultName");
        PasswordCallback pwdCallback = new PasswordCallback("prompt", false);
        Callback[] callbacks = new Callback[]{nameCallback, pwdCallback};
        try
        {
            _cbh.handle(callbacks);
        }
        catch (Exception e)
        {
            throw new SaslException("Error handling SASL callbacks: " + e, e);
        }
        FieldTable table = FieldTableFactory.newFieldTable();
        table.setString("LOGIN", nameCallback.getName());
        table.setString("PASSWORD", new String(pwdCallback.getPassword()));
        return table.getDataAsBytes();
    }

    public boolean isComplete()
    {
        return true;
    }

    public byte[] unwrap(byte[] incoming, int offset, int len) throws SaslException
    {
        throw new SaslException("Not supported");
    }

    public byte[] wrap(byte[] outgoing, int offset, int len) throws SaslException
    {
        throw new SaslException("Not supported");
    }

    public Object getNegotiatedProperty(String propName)
    {
        return null;
    }

    public void dispose() throws SaslException
    {
        _cbh = null;
    }
}
