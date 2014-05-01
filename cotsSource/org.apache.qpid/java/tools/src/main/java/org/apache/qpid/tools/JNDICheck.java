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

package org.apache.qpid.tools;

import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.jms.FailoverPolicy;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.util.Properties;
import java.util.Hashtable;
import java.util.Enumeration;
import java.util.List;
import java.util.LinkedList;
import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;

public class JNDICheck
{
    private static final String QUEUE = "queue.";
    private static final String TOPIC = "topic.";
    private static final String DESTINATION = "destination.";
    private static final String CONNECTION_FACTORY = "connectionfactory.";

    public static void main(String[] args)
    {

        if (args.length != 1)
        {
            usage();
        }

        String propertyFile = args[0];

        new JNDICheck(propertyFile);
    }

    private static void usage()
    {
        exit("Usage: JNDICheck <JNDI Config file>", 0);
    }

    private static void exit(String message, int exitCode)
    {
        System.err.println(message);
        System.exit(exitCode);
    }

    private static String JAVA_NAMING = "java.naming.factory.initial";

    Context _context = null;
    Hashtable _environment = null;

    public JNDICheck(String propertyFile)
    {

        // Load JNDI properties
        Properties properties = new Properties();

        try
        {
            properties.load(new FileInputStream(new File(propertyFile)));
        }
        catch (IOException e)
        {
            exit("Unable to open property file:" + propertyFile + ". Due to:" + e.getMessage(), 1);
        }

        //Create the initial context
        try
        {

            System.setProperty(JAVA_NAMING, properties.getProperty(JAVA_NAMING));

            _context = new InitialContext(properties);

            _environment = _context.getEnvironment();

            Enumeration keys = _environment.keys();

            List<String> queues = new LinkedList<String>();
            List<String> topics = new LinkedList<String>();
            List<String> destinations = new LinkedList<String>();
            List<String> connectionFactories = new LinkedList<String>();

            while (keys.hasMoreElements())
            {
                String key = keys.nextElement().toString();

                if (key.startsWith(QUEUE))
                {
                    queues.add(key);
                }
                else if (key.startsWith(TOPIC))
                {
                    topics.add(key);
                }
                else if (key.startsWith(DESTINATION))
                {
                    destinations.add(key);
                }
                else if (key.startsWith(CONNECTION_FACTORY))
                {
                    connectionFactories.add(key);
                }
            }

            printHeader(propertyFile);
            printEntries(QUEUE, queues);
            printEntries(TOPIC, topics);
            printEntries(DESTINATION, destinations);
            printEntries(CONNECTION_FACTORY, connectionFactories);

        }
        catch (NamingException e)
        {
            exit("Unable to load JNDI Context due to:" + e.getMessage(), 1);
        }

    }

    private void printHeader(String file)
    {
        print("JNDI file :" + file);
    }

    private void printEntries(String type, List<String> list)
    {
        if (list.size() > 0)
        {
            String name = type.substring(0, 1).toUpperCase() + type.substring(1, type.length() - 1);
            print(name + " elements in file:");
            printList(list);
            print("");
        }
    }

    private void printList(List<String> list)
    {
        for (String item : list)
        {
            String key = item.substring(item.indexOf('.') + 1);

            try
            {
                print(key, _context.lookup(key));
            }
            catch (NamingException e)
            {
                exit("Error: item " + key + " no longer in context.", 1);
            }
        }
    }

    private void print(String key, Object object)
    {
        if (object instanceof AMQDestination)
        {
            print(key + ":" + object);
        }
        else if (object instanceof AMQConnectionFactory)
        {
            AMQConnectionFactory factory = (AMQConnectionFactory) object;
            print(key + ":Connection");
            print("ConnectionURL:");
            print(factory.getConnectionURL().toString());
            print("FailoverPolicy");
            print(new FailoverPolicy(factory.getConnectionURL(),null).toString());
            print("");
        }
    }

    private void print(String msg)
    {
        System.out.println(msg);
    }

}
