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
package org.apache.qpid.server.security.auth.sasl.crammd5;

import org.apache.qpid.server.security.auth.sasl.UsernamePasswordInitialiser;
import org.apache.qpid.server.security.auth.database.PrincipalDatabase;

import javax.security.sasl.SaslServerFactory;

public class CRAMMD5Initialiser extends UsernamePasswordInitialiser
{
    private HashDirection _hashDirection;

    public enum HashDirection
    {
        INCOMMING, PASSWORD_FILE
    }


    public String getMechanismName()
    {
        return "CRAM-MD5";
    }

    public Class<? extends SaslServerFactory> getServerFactoryClassForJCARegistration()
    {
        // since the CRAM-MD5 provider is registered as part of the JDK, we do not
        // return the factory class here since we do not need to register it ourselves.
        if (_hashDirection == HashDirection.PASSWORD_FILE)
        {
            return null;
        }
        else
        {
            //fixme we need a server that will correctly has the incomming plain text for comparison to file.
            _logger.warn("we need a server that will correctly convert the incomming plain text for comparison to file.");
            return null;
        }
    }

    public void initialise(PrincipalDatabase passwordFile)
    {
        initialise(passwordFile, HashDirection.PASSWORD_FILE);
    }

    public void initialise(PrincipalDatabase passwordFile, HashDirection direction)
    {
        super.initialise(passwordFile);

        _hashDirection = direction;
    }

}
