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

import java.io.IOException;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

import org.apache.qpid.client.protocol.AMQProtocolSession;

public class UsernamePasswordCallbackHandler implements AMQCallbackHandler
{
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
                ((NameCallback)cb).setName(_protocolSession.getUsername());
            }
            else if (cb instanceof PasswordCallback)
            {
                ((PasswordCallback)cb).setPassword(_protocolSession.getPassword().toCharArray());
            }
            else
            {
                throw new UnsupportedCallbackException(cb);
            }
        }
    }
}
