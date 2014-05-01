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
import java.util.List;
import java.util.Map;

import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.login.AccountNotFoundException;

import org.apache.qpid.server.security.auth.database.PrincipalDatabase;
import org.apache.qpid.server.security.auth.sasl.AuthenticationProviderInitialiser;

public class TestPrincipalDatabase implements PrincipalDatabase
{

    public boolean createPrincipal(Principal principal, char[] password)
    {
        // TODO Auto-generated method stub
        return false;
    }

    public boolean deletePrincipal(Principal principal) throws AccountNotFoundException
    {
        // TODO Auto-generated method stub
        return false;
    }

    public Map<String, AuthenticationProviderInitialiser> getMechanisms()
    {
        // TODO Auto-generated method stub
        return null;
    }

    public Principal getUser(String username)
    {
        // TODO Auto-generated method stub
        return null;
    }

    public List<Principal> getUsers()
    {
        // TODO Auto-generated method stub
        return null;
    }

    public void setPassword(Principal principal, PasswordCallback callback) throws IOException,
            AccountNotFoundException
    {
        callback.setPassword("p".toCharArray());
    }

    public boolean updatePassword(Principal principal, char[] password) throws AccountNotFoundException
    {
        // TODO Auto-generated method stub
        return false;
    }

    public boolean verifyPassword(String principal, char[] password) throws AccountNotFoundException
    {
        // TODO Auto-generated method stub
        return false;
    }

    public void reload() throws IOException
    {
        // TODO Auto-generated method stub
    }

}
