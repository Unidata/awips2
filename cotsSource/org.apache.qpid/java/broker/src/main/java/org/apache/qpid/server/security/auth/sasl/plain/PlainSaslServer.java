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
package org.apache.qpid.server.security.auth.sasl.plain;

import java.io.IOException;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;
import javax.security.sasl.AuthorizeCallback;
import javax.security.sasl.SaslException;
import javax.security.sasl.SaslServer;

public class PlainSaslServer implements SaslServer
{
    public static final String MECHANISM = "PLAIN";

    private CallbackHandler _cbh;

    private String _authorizationId;

    private boolean _complete = false;

    public PlainSaslServer(CallbackHandler cbh)
    {
        _cbh = cbh;
    }

    public String getMechanismName()
    {
        return MECHANISM;
    }

    public byte[] evaluateResponse(byte[] response) throws SaslException
    {
        try
        {
            int authzidNullPosition = findNullPosition(response, 0);
            if (authzidNullPosition < 0)
            {
                throw new SaslException("Invalid PLAIN encoding, authzid null terminator not found");
            }
            int authcidNullPosition = findNullPosition(response, authzidNullPosition + 1);
            if (authcidNullPosition < 0)
            {
                throw new SaslException("Invalid PLAIN encoding, authcid null terminator not found");
            }

            // we do not currently support authcid in any meaningful way
            // String authcid = new String(response, 0, authzidNullPosition, "utf8");
            String authzid = new String(response, authzidNullPosition + 1, authcidNullPosition - 1, "utf8");

            // we do not care about the prompt but it throws if null
            NameCallback nameCb = new NameCallback("prompt", authzid);
            PasswordCallback passwordCb = new PasswordCallback("prompt", false);
            // TODO: should not get pwd as a String but as a char array...
            int passwordLen = response.length - authcidNullPosition - 1;
            String pwd = new String(response, authcidNullPosition + 1, passwordLen, "utf8");
            AuthorizeCallback authzCb = new AuthorizeCallback(authzid, authzid);
            Callback[] callbacks = new Callback[]{nameCb, passwordCb, authzCb};
            _cbh.handle(callbacks);
            String storedPwd = new String(passwordCb.getPassword());
            if (storedPwd.equals(pwd))
            {
                _complete = true;
            }
            if (authzCb.isAuthorized() && _complete)
            {
                _authorizationId = authzCb.getAuthenticationID();
                return null;
            }
            else
            {
                throw new SaslException("Authentication failed");
            }
        }
        catch (IOException e)
        {
            throw new SaslException("Error processing data: " + e, e);
        }
        catch (UnsupportedCallbackException e)
        {
            throw new SaslException("Unable to obtain data from callback handler: " + e, e);
        }
    }

    private int findNullPosition(byte[] response, int startPosition)
    {
        int position = startPosition;
        while (position < response.length)
        {
            if (response[position] == (byte) 0)
            {
                return position;
            }
            position++;
        }
        return -1;
    }

    public boolean isComplete()
    {
        return _complete;
    }

    public String getAuthorizationID()
    {
        return _authorizationId;
    }

    public byte[] unwrap(byte[] incoming, int offset, int len) throws SaslException
    {
        throw new SaslException("Unsupported operation");
    }

    public byte[] wrap(byte[] outgoing, int offset, int len) throws SaslException
    {
        throw new SaslException("Unsupported operation");
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
