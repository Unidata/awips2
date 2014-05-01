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
package org.apache.qpid.server.security.auth.database;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.qpid.server.configuration.ServerConfiguration;

import java.util.Map;
import java.util.Properties;
import java.util.HashMap;

public class PropertiesPrincipalDatabaseManager implements PrincipalDatabaseManager
{

    Map<String, PrincipalDatabase> _databases = new HashMap<String, PrincipalDatabase>();

    public PropertiesPrincipalDatabaseManager(String name, Properties users)
    {
        _databases.put(name, new PropertiesPrincipalDatabase(users));
    }

    public Map<String, PrincipalDatabase> getDatabases()
    {
        return _databases;
    }

    public void initialiseManagement(ServerConfiguration _configuration) throws ConfigurationException
    {
        //todo
    }
}
