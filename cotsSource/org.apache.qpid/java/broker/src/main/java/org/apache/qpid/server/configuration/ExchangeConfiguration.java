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
package org.apache.qpid.server.configuration;

import org.apache.commons.configuration.Configuration;


public class ExchangeConfiguration
{

    private Configuration _config;
    private String _name;

    public ExchangeConfiguration(String exchName, Configuration subset)
    {
        _name = exchName;
        _config = subset;
    }

    public String getName()
    {
        return _name;
    }

    public String getType()
    {
        return _config.getString("type","direct");
    }

    public boolean getDurable()
    {
        return _config.getBoolean("durable", false);
    }

    public boolean getAutoDelete()
    {
        return _config.getBoolean("autodelete",false);
    }

}
