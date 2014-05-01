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
package org.apache.qpid.example.pubsub;

import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.util.Properties;

/**
 * This ConnectionSetup is a wrapper around JNDI it creates a number of entries.
 *
 * It is equivalent to a PropertyFile of value:
 *
 * connectionfactory.local=amqp://guest:guest@clientid/test?brokerlist='localhost'
 * connectionfactory.vm=amqp://guest:guest@clientid/test?brokerlist='vm://:1'
 *
 * queue.queue=example.MyQueue
 * topic.topic=example.hierarical.topic
 *
 */
public class ConnectionSetup
{
    final static String INITIAL_CONTEXT_FACTORY = "org.apache.qpid.jndi.PropertiesFileInitialContextFactory";

    final static String CONNECTION_JNDI_NAME = "local";
    final static String CONNECTION_NAME = "amqp://guest:guest@clientid/test?brokerlist='localhost'";

    public static final String QUEUE_JNDI_NAME = "queue";
    final static String QUEUE_NAME = "example.MyQueue";

    public static final String TOPIC_JNDI_NAME = "topic";
    final static String TOPIC_NAME = "example.hierarical.topic";

    private Context _ctx;

    public ConnectionSetup() throws NamingException
    {

        // Set the properties ...
        Properties properties = new Properties();
        properties.put(Context.INITIAL_CONTEXT_FACTORY, INITIAL_CONTEXT_FACTORY);
        properties.put("connectionfactory." + CONNECTION_JNDI_NAME, CONNECTION_NAME);
        properties.put("connectionfactory." + "vm", "amqp://guest:guest@clientid/test?brokerlist='vm://:1'");

        properties.put("queue." + QUEUE_JNDI_NAME, QUEUE_NAME);
        properties.put("topic." + TOPIC_JNDI_NAME, TOPIC_NAME);
        // Create the initial context
        _ctx = new InitialContext(properties);

    }

    public ConnectionSetup(Properties properties) throws NamingException
    {
        _ctx = new InitialContext(properties);
    }

    public ConnectionFactory getConnectionFactory()
    {

        // Perform the lookups
        try
        {
            return (ConnectionFactory) _ctx.lookup(CONNECTION_JNDI_NAME);
        }
        catch (NamingException e)
        {
            //ignore
        }
        return null;
    }

    public Destination getDestination(String jndiName)
    {
        // Perform the lookups
        try
        {
            return (Destination) _ctx.lookup(jndiName);
        }
        catch (ClassCastException cce)
        {
            //ignore
        }
        catch (NamingException ne)
        {
            //ignore
        }
        return null;
    }


    public void close()
    {
        try
        {
            _ctx.close();
        }
        catch (NamingException e)
        {
            //ignore
        }
    }
}
