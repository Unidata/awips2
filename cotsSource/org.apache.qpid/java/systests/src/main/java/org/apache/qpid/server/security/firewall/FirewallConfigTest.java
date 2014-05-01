package org.apache.qpid.server.security.firewall;
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


import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.jms.Connection;
import javax.jms.JMSException;

import org.apache.qpid.client.AMQConnectionURL;
import org.apache.qpid.test.utils.QpidTestCase;

public class FirewallConfigTest extends QpidTestCase 
{

    private File tmpFile = null;
    @Override
    protected void setUp() throws Exception
    {
        // do setup
        final String QPID_HOME = System.getProperty("QPID_HOME");

        if (QPID_HOME == null)
        {
            fail("QPID_HOME not set");
        }

        // Setup initial config.
        _configFile = new File(QPID_HOME, "etc/config-systests-firewall.xml");
        tmpFile = File.createTempFile("config-systests-firewall", ".xml");
        setSystemProperty("QPID_FIREWALL_SETTINGS", tmpFile.getAbsolutePath());
        tmpFile.deleteOnExit();
    }

    private void writeFirewallFile(boolean allow, boolean inVhost) throws IOException
    {
        FileWriter out = new FileWriter(tmpFile);
        String ipAddr = "127.0.0.1"; // FIXME: get this from InetAddress.getLocalHost().getAddress() ?
        out.write("<broker>");
        if (inVhost) 
        {
            out.write("<virtualhosts><virtualhost><test>");
        }
        out.write("<security><firewall>");
        out.write("<rule access=\""+((allow) ? "allow" : "deny")+"\" network=\""+ipAddr +"\"/>");
        out.write("</firewall></security>");
        if (inVhost)
        {
            out.write("</test></virtualhost></virtualhosts>");
        }
        out.write("</broker>");
        out.close();
    }
    
    public void testVhostAllowBrokerDeny() throws Exception
    {
        if (_broker.equals(VM))
        {
            //No point running this test with an InVM broker as the
            //firewall plugin only functions for TCP connections.
            return;
        }

        _configFile = new File(System.getProperty("QPID_HOME"), "etc/config-systests-firewall-2.xml");
        
        super.setUp();
        
        Connection conn = null;
        try 
        {
            //Try to get a connection to the 'test2' vhost
            //This is expected to fail as it is denied at the broker level
            conn = getConnection(new AMQConnectionURL(
                    "amqp://username:password@clientid/test2?brokerlist='" + getBroker() + "'"));
            fail("We expected the connection to fail");
        } 
        catch (JMSException e)
        {
            //ignore
        }
        
        conn = null;
        try 
        {
            //Try to get a connection to the 'test' vhost
            //This is expected to succeed as it is allowed at the vhost level
            conn = getConnection();
        } 
        catch (JMSException e)
        {
            e.getLinkedException().printStackTrace();
            fail("The connection was expected to succeed: " + e.getMessage());
        }
    }
    
    public void testVhostDenyBrokerAllow() throws Exception
    {
        if (_broker.equals(VM))
        {
            //No point running this test with an InVM broker as the
            //firewall plugin only functions for TCP connections.
            return;
        }
        
        _configFile = new File(System.getProperty("QPID_HOME"), "etc/config-systests-firewall-3.xml");
        
        super.setUp();
        
        Connection conn = null;
        try 
        {
            //Try to get a connection to the 'test2' vhost
            //This is expected to fail as it is denied at the vhost level
            conn = getConnection(new AMQConnectionURL(
                    "amqp://username:password@clientid/test2?brokerlist='" + getBroker() + "'"));
        } 
        catch (JMSException e)
        {
            //ignore
        }

        conn = null;
        try 
        {
            //Try to get a connection to the 'test' vhost
            //This is expected to succeed as it is allowed at the broker level
            conn = getConnection();
        } 
        catch (JMSException e)
        {
            e.getLinkedException().printStackTrace();
            fail("The connection was expected to succeed: " + e.getMessage());
        }
    }
 
    public void testDenyOnRestart() throws Exception
    {
        testDeny(false, new Runnable() {

            public void run()
            {
                try
                {
                    restartBroker();
                } catch (Exception e)
                {
                    fail(e.getMessage());
                }
            }
        });
    }
    
    public void testDenyOnRestartInVhost() throws Exception
    {
        testDeny(true, new Runnable() {

            public void run()
            {
                try
                {
                    restartBroker();
                } catch (Exception e)
                {
                    fail(e.getMessage());
                }
            }
        });
    }
    
    public void testDenyOnReload() throws Exception
    {
        testDeny(false, new Runnable() {

            public void run()
            {
                try
                {
                    reloadBroker();
                } catch (Exception e)
                {
                    fail(e.getMessage());
                }
            }
        }
        );
    }
    
    public void testDenyOnReloadInVhost() throws Exception
    {
        testDeny(true, new Runnable() {

            public void run()
            {
                try
                {
                    reloadBroker();
                } catch (Exception e)
                {
                   fail(e.getMessage());
                }
            }
        }
        );
       
    }
    
    private void testDeny(boolean inVhost, Runnable restartOrReload) throws Exception
    {
        if (_broker.equals(VM))
        {
            // No point running this test in a vm broker
            return;
        }
        
        writeFirewallFile(false, inVhost);        
        super.setUp();
        
        Exception exception  = null;
        Connection conn = null;
        try 
        {
            conn = getConnection();
        } 
        catch (JMSException e)
        {
            exception = e;
        }
        assertNotNull(exception);
        
        // Check we can get a connection

        writeFirewallFile(true, inVhost);
        restartOrReload.run();
        
        exception = null;
        try 
        {
            conn = getConnection();
        } 
        catch (JMSException e)
        {
            exception = e;
        }
        assertNull(exception);
    }    
}
