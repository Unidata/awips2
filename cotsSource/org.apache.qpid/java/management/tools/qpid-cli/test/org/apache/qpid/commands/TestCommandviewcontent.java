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

import org.apache.qpid.utils.JMXinfo;
import org.apache.qpid.utils.CommandLineOptionParser;
import org.apache.qpid.Connector;
import org.apache.qpid.ConnectorFactory;
import org.apache.qpid.ConnectionConstants;
import org.junit.Before;
import org.junit.Test;
import org.junit.After;
import org.junit.Assert;

import javax.management.remote.JMXConnector;
import javax.management.MBeanServerConnection;

public class TestCommandviewcontent
{
    JMXinfo info = null;
    String command = "viewcontent -o queue -n ping -v test -id 10";
    Commandviewcontent viewcontent = null;
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
        viewcontent = new Commandviewcontent(info);

    }

    @Test
    public void TestSetQueryString()
    {
        viewcontent.execute();
        Assert.assertEquals(viewcontent.getObject(), "queue");
        Assert.assertEquals(viewcontent.getnumber(), 10);
        Assert.assertEquals(viewcontent.getName(), "ping");
        Assert.assertEquals(viewcontent.getVirtualhost(), "test");
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
