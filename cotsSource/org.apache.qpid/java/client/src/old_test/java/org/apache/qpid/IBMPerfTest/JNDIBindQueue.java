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
package org.apache.qpid.IBMPerfTest;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.Session;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.io.File;
import java.util.Hashtable;

public class JNDIBindQueue
{
    public static final String DEFAULT_PROVIDER_FILE_PATH = System.getProperty("java.io.tmpdir") + File.separator + "IBMPerfTestsJNDI";
    public static final String PROVIDER_URL = "file://" + DEFAULT_PROVIDER_FILE_PATH;
    public static final String FSCONTEXT_FACTORY = "com.sun.jndi.fscontext.RefFSContextFactory";

    Connection _connection = null;
    Context _ctx = null;


    public JNDIBindQueue(String queueBinding, String queueName, String provider, String contextFactory)
    {
        // Set up the environment for creating the initial context
        Hashtable env = new Hashtable(11);
        env.put(Context.INITIAL_CONTEXT_FACTORY, contextFactory);

        env.put(Context.PROVIDER_URL, provider);

        try
        {
            // Create the initial context
            _ctx = new InitialContext(env);

            // Create the object to be bound

            try
            {
                _connection = new AMQConnection("amqp://guest:guest@clientid/test?brokerlist='tcp://localhost:5672'");
                System.out.println("Connected");
            }
            catch (Exception amqe)
            {
                System.out.println("Unable to create AMQConnectionFactory:" + amqe);
            }

            if (_connection != null)
            {
                bindQueue(queueName, queueBinding);
            }

            // Check that it is bound
            Object obj = _ctx.lookup(queueBinding);

            System.out.println("Bound Queue:" + ((AMQQueue) obj).toURL());

            System.out.println("JNDI FS Context:" + provider);

        }
        catch (NamingException e)
        {
            System.out.println("Operation failed: " + e);
        }
        finally
        {
            try
            {
                if (_connection != null)
                {
                    _connection.close();
                }
            }
            catch (JMSException closeE)
            {
                System.out.println("Connection closing failed: " + closeE);    
            }
        }


    }


    private void bindQueue(String queueName, String queueBinding) throws NamingException
    {

        try
        {
            Object obj = _ctx.lookup(queueBinding);

            if (obj != null)
            {
                System.out.println("Un-binding exisiting object");
                _ctx.unbind(queueBinding);
            }
        }
        catch (NamingException e)
        {

        }

        Queue queue = null;
        try
        {

            Session session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            if (session != null)
            {
                queue = ((AMQSession) session).createQueue(queueName);
            }
        }
        catch (JMSException jmse)
        {
            System.out.println("Unable to create Queue:" + jmse);
        }

        // Perform the bind
        _ctx.bind(queueBinding, queue);
    }


    public static void main(String[] args)
    {
        Logger.getRootLogger().setLevel(Level.OFF);

        String provider = JNDIBindQueue.PROVIDER_URL;
        String contextFactory = JNDIBindQueue.FSCONTEXT_FACTORY;

        if (args.length > 1)
        {
            String binding = args[0];
            String queueName = args[1];

            if (args.length > 2)
            {
                provider = args[2];

                if (args.length > 3)
                {
                    contextFactory = args[3];
                }
            }
            else
            {
                System.out.println("Using default File System Context Factory");
            }

            System.out.println("File System Context Factory\n" +
                               "Binding Queue:'" + queueName + "' to '" + binding + "'\n" +
                               "JNDI Provider URL:" + provider);

            if (provider.startsWith("file"))
            {
                File file = new File(provider.substring(provider.indexOf("://") + 3));

                if (file.exists() && !file.isDirectory())
                {
                    System.out.println("Couldn't make directory file already exists");
                    System.exit(1);
                }
                else
                {
                    if (!file.exists())
                    {
                        if (!file.mkdirs())
                        {
                            System.out.println("Couldn't make directory");
                            System.exit(1);
                        }
                    }
                }
            }


            new JNDIBindQueue(binding, queueName, provider, contextFactory);

        }
        else
        {
            System.out.println("Using Defaults: Usage:java JNDIBindQueue <Binding> <queue name> [<Provider URL> [<JNDI Context Factory>]]");
        }

    }


}
