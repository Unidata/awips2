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
package org.apache.qpid.transacted;

import org.apache.qpid.config.ConnectorConfig;
import org.apache.qpid.config.AbstractConfig;
import org.apache.qpid.config.Connector;

import javax.jms.Connection;

class Config extends AbstractConfig implements ConnectorConfig
{
    private String host = "localhost";
    private int port = 5672;
    private String factory;
    private boolean echo;
    private int batch = 100;
    private boolean persistent = true;

    Config(String[] argv)
    {
        setOptions(argv);
    }

    Connection createConnection() throws Exception
    {
        return new Connector().createConnection(this);
    }

    public boolean isEchoOn()
    {
        return echo;
    }

    public boolean usePersistentMessages()
    {
        return persistent;
    }

    public int getBatchSize()
    {
        return batch;
    }

    public String getHost()
    {
        return host;
    }

    public int getPort()
    {
        return port;
    }

    public String getFactory()
    {
        return factory;
    }

    public void setOption(String key, String value)
    {
        if("-host".equalsIgnoreCase(key))
        {
            host = value;
        }
        else if("-port".equalsIgnoreCase(key))
        {
            port = parseInt("Bad port number", value);
        }
        else if("-factory".equalsIgnoreCase(key))
        {
            factory = value;
        }
        else if("-echo".equalsIgnoreCase(key))
        {
            echo = "true".equalsIgnoreCase(value);
        }
        else if("-persistent".equalsIgnoreCase(key))
        {
            persistent = "true".equalsIgnoreCase(value);
        }
        else if("-batch".equalsIgnoreCase(key))
        {
            batch = parseInt("Bad batch size", value);
        }
        else
        {
            System.out.println("Ignoring nrecognised option " + key);
        }
    }

}
