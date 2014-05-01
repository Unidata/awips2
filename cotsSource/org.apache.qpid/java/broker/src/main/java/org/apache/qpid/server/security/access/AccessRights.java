/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.    
 *
 * 
 */
package org.apache.qpid.server.security.access;

public class AccessRights
{
    public enum Rights
    {
        ANY,
        READ,
        WRITE,
        READWRITE
    }

    Rights _right;

    public AccessRights(Rights right)
    {
        _right = right;
    }

    public boolean allows(Rights rights)
    {
        switch (_right)
        {
            case ANY:
                return (rights.equals(Rights.WRITE)
                        || rights.equals(Rights.READ)
                        || rights.equals(Rights.READWRITE)
                        || rights.equals(Rights.ANY));
            case READ:
                return rights.equals(Rights.READ) || rights.equals(Rights.ANY);
            case WRITE:
                return rights.equals(Rights.WRITE) || rights.equals(Rights.ANY);
            case READWRITE:
                return true;
        }
        return false;
    }

    public Rights getRights()
    {
        return _right;
    }
}
