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

public class VirtualHostAccess
{
    private String _vhost;
    private AccessRights _rights;

    public VirtualHostAccess(String vhostaccess)
    {
        //format <vhost>(<rights>)
        int hostend = vhostaccess.indexOf('(');

        if (hostend == -1)
        {
            throw new IllegalArgumentException("VirtualHostAccess format string contains no access _rights");
        }

        _vhost = vhostaccess.substring(0, hostend);

        String rights = vhostaccess.substring(hostend);

        if (rights.indexOf('r') != -1)
        {
            if (rights.indexOf('w') != -1)
            {
                _rights = new AccessRights(AccessRights.Rights.READWRITE);
            }
            else
            {
                _rights = new AccessRights(AccessRights.Rights.READ);
            }
        }
        else if (rights.indexOf('w') != -1)
        {
            _rights = new AccessRights(AccessRights.Rights.WRITE);
        }
    }

    public AccessRights getAccessRights()
    {
        return _rights;
    }

    public String getVirtualHost()
    {
        return _vhost;
    }
}
