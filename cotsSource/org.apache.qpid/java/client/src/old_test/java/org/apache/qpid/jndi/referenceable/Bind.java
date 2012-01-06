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
package org.apache.qpid.jndi.referenceable;

import org.apache.qpid.client.*;
import org.apache.qpid.AMQException;
import org.apache.qpid.url.URLSyntaxException;

import javax.jms.*;
import javax.naming.*;

import java.util.Properties;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.IOException;

/**
 * Binds a reference from a JNDI source.
 * Given a properties file with the JNDI information and a binding string.
 */
public class Bind
{
    private static final String USAGE="USAGE: java bind <JNDI Properties file> -cf <url> <binding> | -c <url> <binding> [-t <topic Name> <binding>] [-q <queue Name> <binding>]";
    public Bind(String propertiesFile, String bindingURL, Referenceable reference) throws NameAlreadyBoundException, NoInitialContextException
    {
        // Set up the environment for creating the initial context
        String qpid_home = System.getProperty("QPID_HOME");

        if (qpid_home == null || qpid_home.equals(""))
        {
            System.out.println("QPID_HOME is not set");
            System.exit(1);
        }

        if (qpid_home.charAt(qpid_home.length() - 1) != '/')
        {
            qpid_home += "/";
        }

        try
        {
            InputStream inputStream = new FileInputStream(qpid_home + propertiesFile);
            Properties properties = new Properties();
            properties.load(inputStream);

            // Create the initial context
            Context ctx = new InitialContext(properties);

            // Perform the binds
            ctx.bind(bindingURL, reference);

            // Close the context when we're done
            ctx.close();
        }
        catch (IOException ioe)
        {
            System.out.println("Unable to access properties file:" + propertiesFile + " Due to:" + ioe);
        }
        catch (NamingException e)
        {
            System.out.println("Operation failed: " + e);
            if (e instanceof NameAlreadyBoundException)
            {
                throw (NameAlreadyBoundException) e;
            }

            if (e instanceof NoInitialContextException)
            {
                throw (NoInitialContextException) e;
            }
        }

    }

    private static String parse(String[] args, int index, String what, String type)
    {
        try
        {
            return args[index];
        }
        catch (IndexOutOfBoundsException ioobe)
        {
            System.out.println("ERROR: No " + what + " specified for " + type + ".");
            System.out.println(USAGE);
            System.exit(1);
        }

        // The path is either return normally or exception.. which calls system exit so keep the compiler happy
        return "Never going to happen";
    }


    public static void main(String[] args) throws NameAlreadyBoundException, NoInitialContextException, URLSyntaxException, AMQException, JMSException
    {


        org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.OFF);

//        org.apache.log4j.Logger _logger = org.apache.log4j.Logger.getLogger(AMQConnection.class);
//        _logger.setLevel(org.apache.log4j.Level.OFF);

        boolean exit = false;

        String qpid_home = System.getProperty("QPID_HOME");

        if (qpid_home == null || qpid_home.equals(""))
        {
            System.out.println("QPID_HOME is not set");
            exit = true;
        }

        if (args.length <= 2)
        {
            System.out.println("At least a connection or connection factory must be requested to be bound.");
            exit = true;
        }
        else
        {
            if ((args.length - 1) % 3 != 0)
            {
                System.out.println("Not all values have full details");
                exit = true;
            }
        }
        if (exit)
        {
            System.out.println(USAGE);
            System.exit(1);
        }

        if (qpid_home.charAt(qpid_home.length() - 1) != '/')

        {
            qpid_home += "/";
        }

        AMQConnectionFactory cf = null;
        AMQConnection c = null;
        AMQSession session = null;
        Referenceable reference = null;

        for (int index = 1; index < args.length; index ++)
        {
            String obj = args[index];

            String what = "Invalid";
            String binding;

            if (obj.startsWith("-c"))
            {
                boolean isFactory = obj.contains("f");


                if (isFactory)
                {
                    what = "ConnectionFactory";
                }
                else
                {
                    what = "Factory";
                }

                String url = parse(args, ++index, "url", what);

                if (isFactory)
                {

                    cf = new AMQConnectionFactory(url);
                    reference = cf;
                }
                else
                {
                    c = new AMQConnection(url);
                    reference = c;
                }

            }

            if (obj.equals("-t") || obj.equals("-q"))
            {
                if (c == null)
                {
                    c = (AMQConnection) cf.createConnection();
                }

                if (session == null)
                {
                    session = (AMQSession) c.createSession(false, Session.AUTO_ACKNOWLEDGE);
                }

            }

            if (obj.equals("-t"))
            {

                String topicName = parse(args, ++index, "Topic Name", "Topic");
                reference = (AMQTopic) session.createTopic(topicName);
                what = "Topic";
            }
            else
            {
                if (obj.equals("-q"))
                {
                    String topicName = parse(args, ++index, "Queue Name", "Queue");
                    reference = (AMQQueue) session.createQueue(topicName);
                    what = "Queue";
                }
            }

            binding = parse(args, ++index, "binding", what);
            if (binding == null)
            {
                System.out.println(obj + " is not a known Object to bind.");
                System.exit(1);
            }
            else
            {
                System.out.print("Binding:" + reference + " to " + binding);
                try
                {
                    new Bind(args[0], binding, reference);
                    System.out.println(" ..Successful");

                }
                catch (NameAlreadyBoundException nabe)
                {
                    System.out.println("");
                    if (!obj.startsWith("-c") || index == args.length - 1)
                    {
                        throw nabe;
                    }
                    else
                    {
                        System.out.println("Continuing with other bindings using the same connection details");
                    }
                }
                finally
                {
                    if (!obj.startsWith("-c") || index == args.length - 1)
                    {
                        if (c != null)
                        {
                            c.close();
                        }
                    }
                }
            }
        }

        if (c != null)
        {
            c.close();
        }
    }
}
