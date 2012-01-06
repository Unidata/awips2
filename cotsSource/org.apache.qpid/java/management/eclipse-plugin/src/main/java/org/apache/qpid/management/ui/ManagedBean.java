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
package org.apache.qpid.management.ui;

import static org.apache.qpid.management.ui.Constants.ADMIN_MBEAN_TYPE;
import static org.apache.qpid.management.ui.Constants.CONNECTION;
import static org.apache.qpid.management.ui.Constants.DEFAULT_VH;
import static org.apache.qpid.management.ui.Constants.EXCHANGE;
import static org.apache.qpid.management.ui.Constants.QUEUE;
import static org.apache.qpid.management.ui.Constants.VIRTUAL_HOST;

import java.util.HashMap;

import org.apache.qpid.management.common.mbeans.ManagedBroker;

/**
 * Class representing a managed bean on the managed server
 */
public abstract class ManagedBean extends ManagedObject
{
    private String _uniqueName = "";
    private String _domain = "";
    private String _type = "";
    private String _virtualHostName = null;
    private ManagedServer _server = null;
    private HashMap _properties = null;
    private int _version;

    public String getProperty(String key)
    {
        return (String) _properties.get(key);
    }

    public HashMap getProperties()
    {
        return _properties;
    }

    public void setProperties(HashMap properties)
    {
        this._properties = properties;
        setName(getProperty("name"));
        setType(getProperty("type"));
        setVersion(getProperty("version"));
        _virtualHostName = getProperty(VIRTUAL_HOST);
    }

    public void setVersion(String version)
    {
        try
        {
            _version = Integer.parseInt(version);
        }
        catch (NumberFormatException nfe)
        {
            _version = 1;
        }

    }

    public int getVersion()
    {
        return _version;
    }

    public String getDomain()
    {
        return _domain;
    }

    public void setDomain(String domain)
    {
        this._domain = domain;
    }

    public ManagedServer getServer()
    {
        return _server;
    }

    public void setServer(ManagedServer server)
    {
        this._server = server;
    }

    public String getType()
    {
        return _type;
    }

    public void setType(String type)
    {
        this._type = type;
    }

    public String getUniqueName()
    {
        return _uniqueName;
    }

    public void setUniqueName(String uniqueName)
    {
        this._uniqueName = uniqueName;
    }

    public String getVirtualHostName()
    {
        // To make it work with the broker with no virtual host implementation
        return _virtualHostName == null ? DEFAULT_VH : _virtualHostName;
    }

    /**
     * Returns mbean instance name. MBeans which have only one instance, the type attribute will be returned
     *
     * @return
     */
    public String getInstanceName()
    {
        if (getName() != null)
        {
            return getName();
        }
        else
        {
            return getType();
        }
    }

    public boolean isQueue()
    {
        return _type.endsWith(QUEUE);
    }

    public boolean isConnection()
    {
        return _type.endsWith(CONNECTION);
    }

    public boolean isExchange()
    {
        return _type.endsWith(EXCHANGE);
    }

    public boolean isVirtualHostManager()
    {
        return _type.endsWith(ManagedBroker.TYPE);
    }
    
    public boolean isTempQueue()
    {
        return (isQueue() && getName().startsWith("tmp_"));
    }

    public boolean isAdmin()
    {
        return _type.endsWith(ADMIN_MBEAN_TYPE);
    }
}
