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
package org.apache.qpid;

import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;

import org.apache.qpid.management.common.JMXConnnectionFactory;

public class ConnectorFactory
{

    private static final long TIMEOUT = 30 * 1000;

    public static Connector getConnector(String host, String port, String username, String password) throws Exception
    {

        JMXConnector jmxc = null;
        MBeanServerConnection mbsc = null;
        try
        {
            jmxc = JMXConnnectionFactory.getJMXConnection(TIMEOUT, host, Integer.parseInt(port), username, password);
            mbsc = jmxc.getMBeanServerConnection();
        }
        catch (NumberFormatException e)
        {
            System.out.println("Illegal port entered:" + port);
            System.exit(1);
        }
        return new Connector(jmxc, mbsc);
    }
}
