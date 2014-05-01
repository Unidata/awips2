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

import org.apache.qpid.server.security.auth.sasl.SaslServerTestCase;
import org.apache.qpid.server.security.auth.sasl.UsernamePasswordInitialiser;

public class PlainSaslServerTest extends SaslServerTestCase
{
    
    protected void setUp() throws Exception
    {
        UsernamePasswordInitialiser handler = new PlainInitialiser();
        handler.initialise(db);
        this.server = new PlainSaslServer(handler.getCallbackHandler());
        correctresponse = new byte[]{0x0, (byte) username.charAt(0), 0x0, (byte) password.charAt(0)};
        wrongresponse = new byte[]{0x0,(byte) username.charAt(0), 0x0, (byte) notpassword.charAt(0)};
    }
    
}
