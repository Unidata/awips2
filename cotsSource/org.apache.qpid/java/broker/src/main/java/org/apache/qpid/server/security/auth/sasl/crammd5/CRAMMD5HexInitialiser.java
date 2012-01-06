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

import org.apache.qpid.server.security.auth.database.PrincipalDatabase;
import org.apache.qpid.server.security.auth.sasl.UsernamePasswordInitialiser;
import org.apache.qpid.server.security.auth.sasl.AuthenticationProviderInitialiser;

import javax.security.sasl.SaslServerFactory;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.login.AccountNotFoundException;
import java.util.Map;
import java.util.List;
import java.security.Principal;
import java.io.IOException;

public class CRAMMD5HexInitialiser extends UsernamePasswordInitialiser
{
    public String getMechanismName()
    {
        return CRAMMD5HexSaslServer.MECHANISM;
    }

    public Class<? extends SaslServerFactory> getServerFactoryClassForJCARegistration()
    {
        return CRAMMD5HexServerFactory.class;
    }

    public Map<String, ?> getProperties()
    {
        return null;
    }

    public void initialise(PrincipalDatabase db)
    {
        super.initialise(new HexifyPrincipalDatabase(db));

    }

    private class HexifyPrincipalDatabase implements PrincipalDatabase
    {
        private PrincipalDatabase _realPricipalDatabase;

        HexifyPrincipalDatabase(PrincipalDatabase db)
        {
            _realPricipalDatabase = db;
        }

        private char[] toHex(char[] password)
        {
            StringBuilder sb = new StringBuilder();
            for (char c : password)
            {
                //toHexString does not prepend 0 so we have to
                if (((byte) c > -1) && (byte) c < 10)
                {
                    sb.append(0);
                }

                sb.append(Integer.toHexString(c & 0xFF));
            }

            //Extract the hex string as char[]
            char[] hex = new char[sb.length()];

            sb.getChars(0, sb.length(), hex, 0);

            return hex;
        }

        public void setPassword(Principal principal, PasswordCallback callback) throws IOException, AccountNotFoundException
        {
            //Let the read DB set the password
            _realPricipalDatabase.setPassword(principal, callback);

            //Retrieve the setpassword
            char[] plainPassword = callback.getPassword();

            char[] hexPassword = toHex(plainPassword);

            callback.setPassword(hexPassword);
        }

        // Simply delegate to the real PrincipalDB
        public boolean verifyPassword(String principal, char[] password) throws AccountNotFoundException
        {
            return _realPricipalDatabase.verifyPassword(principal, password);
        }

        public boolean updatePassword(Principal principal, char[] password) throws AccountNotFoundException
        {
            return _realPricipalDatabase.updatePassword(principal, password);
        }

        public boolean createPrincipal(Principal principal, char[] password)
        {
            return _realPricipalDatabase.createPrincipal(principal, password);
        }

        public boolean deletePrincipal(Principal principal) throws AccountNotFoundException
        {
            return _realPricipalDatabase.deletePrincipal(principal);
        }

        public Principal getUser(String username)
        {
            return _realPricipalDatabase.getUser(username);
        }

        public Map<String, AuthenticationProviderInitialiser> getMechanisms()
        {
            return _realPricipalDatabase.getMechanisms();
        }

        public List<Principal> getUsers()
        {
            return _realPricipalDatabase.getUsers();
        }
	
        public void reload() throws IOException
        {
            _realPricipalDatabase.reload();
        }
    }

}
