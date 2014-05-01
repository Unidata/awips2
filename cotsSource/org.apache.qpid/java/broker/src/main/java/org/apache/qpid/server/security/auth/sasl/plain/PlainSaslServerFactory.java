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

import java.util.Map;

import javax.security.auth.callback.CallbackHandler;
import javax.security.sasl.Sasl;
import javax.security.sasl.SaslException;
import javax.security.sasl.SaslServer;
import javax.security.sasl.SaslServerFactory;

public class PlainSaslServerFactory implements SaslServerFactory
{
    public SaslServer createSaslServer(String mechanism, String protocol, String serverName, Map props,
                                       CallbackHandler cbh) throws SaslException
    {
        if (PlainSaslServer.MECHANISM.equals(mechanism))
        {
            return new PlainSaslServer(cbh);
        }
        else
        {
            return null;
        }
    }

    public String[] getMechanismNames(Map props)
    {
        if (props.containsKey(Sasl.POLICY_NOPLAINTEXT) ||
            props.containsKey(Sasl.POLICY_NODICTIONARY) ||
            props.containsKey(Sasl.POLICY_NOACTIVE))
        {
            // returned array must be non null according to interface documentation
            return new String[0];
        }
        else
        {
            return new String[]{PlainSaslServer.MECHANISM};
        }
    }
}
