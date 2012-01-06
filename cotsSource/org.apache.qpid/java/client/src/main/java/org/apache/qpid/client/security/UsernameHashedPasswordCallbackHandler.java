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
package org.apache.qpid.client.security;

import org.apache.qpid.client.protocol.AMQProtocolSession;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class UsernameHashedPasswordCallbackHandler implements AMQCallbackHandler
{
    private static final Logger _logger = LoggerFactory.getLogger(UsernameHashedPasswordCallbackHandler.class);

    private AMQProtocolSession _protocolSession;

    public void initialise(AMQProtocolSession protocolSession)
    {
        _protocolSession = protocolSession;
    }

    public void handle(Callback[] callbacks) throws IOException, UnsupportedCallbackException
    {
        for (int i = 0; i < callbacks.length; i++)
        {
            Callback cb = callbacks[i];
            if (cb instanceof NameCallback)
            {
                ((NameCallback) cb).setName(_protocolSession.getUsername());
            }
            else if (cb instanceof PasswordCallback)
            {
                try
                {
                    ((PasswordCallback) cb).setPassword(getHash(_protocolSession.getPassword()));
                }
                catch (NoSuchAlgorithmException e)
                {
                    UnsupportedCallbackException uce = new UnsupportedCallbackException(cb);
                    uce.initCause(e);
                    throw uce;
                }
            }
            else
            {
                throw new UnsupportedCallbackException(cb);
            }
        }
    }

    private char[] getHash(String text) throws NoSuchAlgorithmException, UnsupportedEncodingException
    {

        byte[] data = text.getBytes("utf-8");

        MessageDigest md = MessageDigest.getInstance("MD5");

        for (byte b : data)
        {
            md.update(b);
        }

        byte[] digest = md.digest();

        char[] hash = new char[digest.length];

        int index = 0;
        for (byte b : digest)
        {
            hash[index++] = (char) b;
        }

        return hash;
    }
}
