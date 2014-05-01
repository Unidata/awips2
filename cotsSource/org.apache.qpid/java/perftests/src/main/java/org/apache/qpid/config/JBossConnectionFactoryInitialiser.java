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

import org.apache.qpid.config.ConnectionFactoryInitialiser;
import org.apache.qpid.config.ConnectorConfig;
import org.apache.qpid.client.JMSAMQException;

import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.MBeanException;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.naming.NameNotFoundException;
import java.util.Hashtable;

public class JBossConnectionFactoryInitialiser implements ConnectionFactoryInitialiser
{
    public ConnectionFactory getFactory(ConnectorConfig config) throws JMSException
    {
        ConnectionFactory cf = null;
        InitialContext ic = null;
        Hashtable ht = new Hashtable();
        ht.put(InitialContext.INITIAL_CONTEXT_FACTORY, "org.jnp.interfaces.NamingContextFactory");
        String jbossHost = System.getProperty("jboss.host", "eqd-lxamq01");
        String jbossPort = System.getProperty("jboss.port", "1099");
        ht.put(InitialContext.PROVIDER_URL, "jnp://" + jbossHost + ":" + jbossPort);
        ht.put(InitialContext.URL_PKG_PREFIXES, "org.jboss.naming:org.jnp.interfaces");

        try
        {
            ic = new InitialContext(ht);
            if (!doesDestinationExist("topictest.messages", ic))
            {
                deployTopic("topictest.messages", ic);
            }
            if (!doesDestinationExist("topictest.control", ic))
            {
                deployTopic("topictest.control", ic);
            }

            cf = (ConnectionFactory) ic.lookup("/ConnectionFactory");
            return cf;
        }
        catch (NamingException e)
        {
            throw new JMSAMQException("Unable to lookup object: " + e, e);
        }
        catch (Exception e)
        {
            throw new JMSAMQException("Error creating topic: " + e, e);
        }
    }

    private boolean doesDestinationExist(String name, InitialContext ic) throws Exception
    {
        try
        {
            ic.lookup("/" + name);
        }
        catch (NameNotFoundException e)
        {
            return false;
        }
        return true;
    }

    private void deployTopic(String name, InitialContext ic) throws Exception
    {
        MBeanServerConnection mBeanServer = lookupMBeanServerProxy(ic);

        ObjectName serverObjectName = new ObjectName("jboss.messaging:service=ServerPeer");

        String jndiName = "/" + name;
        try
        {
            mBeanServer.invoke(serverObjectName, "createTopic",
                               new Object[]{name, jndiName},
                               new String[]{"java.lang.String", "java.lang.String"});
        }
        catch (MBeanException e)
        {
            System.err.println("Error: " + e);
            System.err.println("Cause: " + e.getCause());
        }
    }

    private MBeanServerConnection lookupMBeanServerProxy(InitialContext ic) throws NamingException
    {
        return (MBeanServerConnection) ic.lookup("jmx/invoker/RMIAdaptor");
    }
}
