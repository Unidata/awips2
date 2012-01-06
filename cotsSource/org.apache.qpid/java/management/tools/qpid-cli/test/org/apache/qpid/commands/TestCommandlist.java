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
package org.apache.qpid.commands;

import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;

import org.apache.qpid.ConnectionConstants;
import org.apache.qpid.Connector;
import org.apache.qpid.ConnectorFactory;
import org.apache.qpid.utils.CommandLineOptionParser;
import org.apache.qpid.utils.JMXinfo;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TestCommandlist
{
    /*
     * All the methods in Commandlist doesn't have any arguments and no return
     * type.
     */
    JMXinfo info = null;
    String command = "list -o queue -n ping -v test";
    Commandlist list = null;
    Connector conn = null;

    @Before
    public void startup() throws Exception
    {
        conn = ConnectorFactory.getConnector(ConnectionConstants.BROKER_HOSTNAME, ConnectionConstants.BROKER_PORT,
                ConnectionConstants.USERNAME, ConnectionConstants.PASSWORD);
        JMXConnector jmxc = conn.getConnector();
        MBeanServerConnection mbsc = conn.getMBeanServerConnection();
        CommandLineOptionParser parser = new CommandLineOptionParser(command.split(" "));
        info = new JMXinfo(jmxc, parser, mbsc);
        list = new Commandlist(info);

    }

    @Test
    public void TestSetQueryString()
    {
        list.execute();
        Assert.assertEquals(list.getObject(), "queue");
        Assert.assertEquals(list.getVirtualhost(), "test");
        Assert.assertEquals(list.getName(), "ping");
    }

    @After
    public void cleanup()
    {
        try
        {
            conn.getConnector().close();
        }
        catch (Exception ex)
        {
            ex.printStackTrace();
        }
    }

}
