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
package org.apache.qpid.config;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;

public class Connector
{
    public Connection createConnection(ConnectorConfig config) throws Exception
    {
        return getConnectionFactory(config).createConnection();
    }

    ConnectionFactory getConnectionFactory(ConnectorConfig config) throws Exception
    {
        String factory = config.getFactory();
        if(factory == null) factory = AMQConnectionFactoryInitialiser.class.getName();
        System.out.println("Using " + factory);
        return ((ConnectionFactoryInitialiser) Class.forName(factory).newInstance()).getFactory(config);
    }
}
