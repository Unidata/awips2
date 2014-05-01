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

import org.apache.log4j.Logger;

import java.security.Principal;

public class PlainUser implements Principal
{
    private String _name;
    private char[] _password;
    private boolean _modified = false;
    private boolean _deleted = false;

    PlainUser(String[] data)
    {
        if (data.length != 2)
        {
            throw new IllegalArgumentException("User Data should be length 2, username, password");
        }

        _name = data[0];

        _password = data[1].toCharArray();

    }

    public PlainUser(String name, char[] password)
    {
        _name = name;
        _password = password;
        _modified = true;
    }

    public String getName()
    {
        return _name;
    }

    public String toString()
    {
        return _name;
    }

    char[] getPassword()
    {
        return _password;
    }
    
    byte[] getPasswordBytes()
    {
        byte[] byteArray = new byte[_password.length];
        int index = 0;
        for (char c : _password)
        {
            byteArray[index++] = (byte) c;
        }
        return byteArray;
    }

    void setPassword(char[] password)
    {
        _password = password;
        _modified = true;
    }

    public boolean isModified()
    {
        return _modified;
    }

    public boolean isDeleted()
    {
        return _deleted;
    }

    public void delete()
    {
        _deleted = true;
    }

    public void saved()
    {
        _modified = false;
    }

}
