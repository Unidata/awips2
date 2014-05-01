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
package org.apache.qpid.server.security.auth;

import javax.security.sasl.SaslException;

public class AuthenticationResult
{
    public enum AuthenticationStatus
    {
        SUCCESS, CONTINUE, ERROR
    }

    public AuthenticationStatus status;
    public byte[] challenge;
    
    private Exception cause;

    public AuthenticationResult(AuthenticationStatus status)
    {
        this(null, status, null);
    }

    public AuthenticationResult(byte[] challenge, AuthenticationStatus status)
    {
        this(challenge, status, null);
    }

    public AuthenticationResult(AuthenticationStatus error, Exception cause)
    {
        this(null, error, cause);
    }

    public AuthenticationResult(byte[] challenge, AuthenticationStatus status, Exception cause)
    {
        this.status = status;
        this.challenge = challenge;
        this.cause = cause;
    }

    public Exception getCause()
    {
        return cause;
    }
}
