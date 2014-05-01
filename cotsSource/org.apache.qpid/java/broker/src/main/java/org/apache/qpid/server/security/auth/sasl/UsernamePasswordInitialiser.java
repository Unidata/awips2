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
package org.apache.qpid.server.security.auth.sasl;

import java.io.IOException;
import java.security.Principal;
import java.util.Map;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;
import javax.security.auth.login.AccountNotFoundException;
import javax.security.sasl.AuthorizeCallback;

import org.apache.commons.configuration.Configuration;

import org.apache.log4j.Logger;

import org.apache.qpid.server.security.auth.database.PrincipalDatabase;
import org.apache.qpid.server.security.auth.sasl.AuthenticationProviderInitialiser;
import org.apache.qpid.server.security.auth.sasl.UsernamePrincipal;

public abstract class UsernamePasswordInitialiser implements AuthenticationProviderInitialiser
{
    protected static final Logger _logger = Logger.getLogger(UsernamePasswordInitialiser.class);

    private ServerCallbackHandler _callbackHandler;

    private class ServerCallbackHandler implements CallbackHandler
    {
        private final PrincipalDatabase _principalDatabase;

        protected ServerCallbackHandler(PrincipalDatabase database)
        {
            _principalDatabase = database;
        }

        public void handle(Callback[] callbacks) throws IOException, UnsupportedCallbackException
        {
            Principal username = null;
            for (Callback callback : callbacks)
            {
                if (callback instanceof NameCallback)
                {
                    username = new UsernamePrincipal(((NameCallback) callback).getDefaultName());
                }
                else if (callback instanceof PasswordCallback)
                {
                    try
                    {
                        _principalDatabase.setPassword(username, (PasswordCallback) callback);
                    }
                    catch (AccountNotFoundException e)
                    {
                        // very annoyingly the callback handler does not throw anything more appropriate than
                        // IOException
                        IOException ioe = new IOException("Error looking up user " + e);
                        ioe.initCause(e);
                        throw ioe;
                    }
                }
                else if (callback instanceof AuthorizeCallback)
                {
                    ((AuthorizeCallback) callback).setAuthorized(true);
                }
                else
                {
                    throw new UnsupportedCallbackException(callback);
                }
            }
        }
    }

    public void initialise(String baseConfigPath, Configuration configuration,
        Map<String, PrincipalDatabase> principalDatabases) throws Exception
    {
        String principalDatabaseName = configuration.getString(baseConfigPath + ".principal-database");
        PrincipalDatabase db = principalDatabases.get(principalDatabaseName);

        initialise(db);
    }

    public void initialise(PrincipalDatabase db)
    {
        if (db == null)
        {
            throw new NullPointerException("Cannot initialise with a null Principal database.");
        }

        _callbackHandler = new ServerCallbackHandler(db);
    }

    public CallbackHandler getCallbackHandler()
    {
        return _callbackHandler;
    }

    public Map<String, ?> getProperties()
    {
        // there are no properties required for the CRAM-MD5 implementation
        return null;
    }
}
