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
package org.apache.qpid.server.security.auth.database;

import org.apache.qpid.server.security.auth.sasl.AuthenticationProviderInitialiser;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.Principal;
import java.util.Map;
import java.util.List;

import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.login.AccountNotFoundException;

/** Represents a "user database" which is really a way of storing principals (i.e. usernames) and passwords. */
public interface PrincipalDatabase
{
    /**
     * Set the password for a given principal in the specified callback. This is used for certain SASL providers. The
     * user database implementation should look up the password in any way it chooses and set it in the callback by
     * calling its setPassword method.
     *
     * @param principal the principal
     * @param callback  the password callback that wants to receive the password
     *
     * @throws AccountNotFoundException if the account for specified principal could not be found
     * @throws IOException              if there was an error looking up the principal
     */
    void setPassword(Principal principal, PasswordCallback callback)
            throws IOException, AccountNotFoundException;

     /**
     * Used to verify that the presented Password is correct. Currently only used by Management Console
     * @param principal The principal to authenticate
     * @param password The password to check
     * @return true if password is correct
     * @throws AccountNotFoundException if the principal cannot be found
     */
    boolean verifyPassword(String principal, char[] password)
            throws AccountNotFoundException;

    /**
     * Update(Change) the password for the given principal
     * @param principal Who's password is to be changed
     * @param password The new password to use
     * @return True if change was successful
     * @throws AccountNotFoundException If the given principal doesn't exist in the Database
     */
    boolean updatePassword(Principal principal, char[] password)
            throws AccountNotFoundException;

    /**
     * Create a new principal in the database
     * @param principal The principal to create
     * @param password The password to set for the principal
     * @return True on a successful creation
     */
    boolean createPrincipal(Principal principal, char[] password);

    /**
     * Delete a principal
     * @param principal The principal to delete
     * @return True on a successful creation
     * @throws AccountNotFoundException If the given principal doesn't exist in the Database
     */
    boolean deletePrincipal(Principal principal)
            throws AccountNotFoundException;

    /**
     * Get the principal from the database with the given username
     * @param username of the principal to lookup
     * @return The Principal object for the given username or null if not found.
     */
    Principal getUser(String username);

    /**
     * Reload the database to its ensure contents are up to date
     * @throws IOException If there was an error reloading the database
     */
    void reload() throws IOException;

    public Map<String, AuthenticationProviderInitialiser> getMechanisms();


    List<Principal> getUsers();
}
