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
package org.apache.qpid.server.security.auth.manager;

import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.security.auth.AuthenticationResult;

import javax.security.sasl.SaslException;
import javax.security.sasl.SaslServer;

public interface AuthenticationManager
{
    String getMechanisms();

    SaslServer createSaslServer(String mechanism, String localFQDN) throws SaslException;

    AuthenticationResult authenticate(SaslServer server, byte[] response);

    void close();
}
