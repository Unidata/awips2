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
package org.apache.qpid.server.queue;

import org.apache.log4j.Logger;
import org.apache.qpid.AMQChannelClosedException;
import org.apache.qpid.AMQConnectionClosedException;
import org.apache.qpid.AMQException;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.url.URLSyntaxException;
import org.apache.qpid.util.CommandLineParser;

import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import java.io.IOException;
import java.util.Properties;

public class PersistentTestManual
{
    private static final Logger _logger = Logger.getLogger(PersistentTestManual.class);


    private static final String QUEUE = "direct://amq.direct//PersistentTest-Queue2?durable='true',exclusive='true'";

    protected AMQConnection _connection;

    protected Session _session;

    protected Queue _queue;
    private Properties properties;

    private String _brokerDetails;
    private String _username;
    private String _password;
    private String _virtualpath;

    public PersistentTestManual(Properties overrides)
    {
        properties = new Properties(defaults);
        properties.putAll(overrides);

        _brokerDetails = properties.getProperty(BROKER_PROPNAME);
        _username = properties.getProperty(USERNAME_PROPNAME);
        _password = properties.getProperty(PASSWORD_PROPNAME);
        _virtualpath = properties.getProperty(VIRTUAL_HOST_PROPNAME);

        createConnection();
    }

    protected void createConnection()
    {
        try
        {
            _connection = new AMQConnection(_brokerDetails, _username, _password, "PersistentTest", _virtualpath);

            _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            _connection.start();
        }
        catch (Exception e)
        {
            _logger.error("Unable to create test class due to:" + e.getMessage(), e);
            System.exit(0);
        }
    }

    public void test() throws AMQException, URLSyntaxException
    {

        //Create the Durable Queue
        try
        {
            _session.createConsumer(_session.createQueue(QUEUE)).close();
        }
        catch (JMSException e)
        {
            _logger.error("Unable to create Queue due to:" + e.getMessage(), e);
            System.exit(0);
        }

        try
        {
            if (testQueue())
            {
                // close connection
                _connection.close();
                // wait
                System.out.println("Restart Broker Now");
                try
                {
                    System.in.read();
                }
                catch (IOException e)
                {
                    //
                }
                finally
                {
                    System.out.println("Continuing....");
                }

                //Test queue is still there.
                AMQConnection connection = new AMQConnection(_brokerDetails, _username, _password, "DifferentClientID", _virtualpath);

                AMQSession session = (AMQSession) connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

                try
                {
                    session.createConsumer(session.createQueue(QUEUE));
                    _logger.error("Create consumer succeeded." +
                                  " This shouldn't be allowed as this means the queue didn't exist when it should");

                    connection.close();

                    exit();
                }
                catch (JMSException e)
                {
                    try
                    {
                        connection.close();
                    }
                    catch (JMSException cce)
                    {
                        if (cce.getLinkedException() instanceof AMQConnectionClosedException)
                        {
                            _logger.error("Channel Close Bug still present QPID-432, should see an 'Error closing session'");
                        }
                        else
                        {
                            exit(cce);
                        }
                    }

                    if (e.getLinkedException() instanceof AMQChannelClosedException)
                    {
                        _logger.info("AMQChannelClosedException received as expected");
                    }
                    else
                    {
                        exit(e);
                    }
                }
            }
        }
        catch (JMSException e)
        {
            _logger.error("Unable to test Queue due to:" + e.getMessage(), e);
            System.exit(0);
        }
    }

    private void exit(JMSException e)
    {
        _logger.error("JMSException received:" + e.getMessage());
        e.printStackTrace();
        exit();
    }

    private void exit()
    {
        try
        {
            _connection.close();
        }
        catch (JMSException e)
        {
            //
        }
        System.exit(0);
    }

    private boolean testQueue() throws JMSException
    {
        String TEST_TEXT = "init";

        //Create a new session to send producer
        Session session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        Queue q = session.createQueue(QUEUE);
        MessageProducer producer = session.createProducer(q);

        producer.send(session.createTextMessage(TEST_TEXT));

        //create a new consumer on the original session
        TextMessage m = (TextMessage) _session.createConsumer(q).receive();


        if ((m != null) && m.getText().equals(TEST_TEXT))
        {
            return true;
        }
        else
        {
            _logger.error("Incorrect values returned from Queue Test:" + m);
            System.exit(0);
            return false;
        }
    }

    /** Holds the name of the property to get the test broker url from. */
    public static final String BROKER_PROPNAME = "broker";

    /** Holds the default broker url for the test. */
    public static final String BROKER_DEFAULT = "tcp://localhost:5672";

    /** Holds the name of the property to get the test broker virtual path. */
    public static final String VIRTUAL_HOST_PROPNAME = "virtualHost";

    /** Holds the default virtual path for the test. */
    public static final String VIRTUAL_HOST_DEFAULT = "";

    /** Holds the name of the property to get the broker access username from. */
    public static final String USERNAME_PROPNAME = "username";

    /** Holds the default broker log on username. */
    public static final String USERNAME_DEFAULT = "guest";

    /** Holds the name of the property to get the broker access password from. */
    public static final String PASSWORD_PROPNAME = "password";

    /** Holds the default broker log on password. */
    public static final String PASSWORD_DEFAULT = "guest";

    /** Holds the default configuration properties. */
    public static Properties defaults = new Properties();

    static
    {
        defaults.setProperty(BROKER_PROPNAME, BROKER_DEFAULT);
        defaults.setProperty(USERNAME_PROPNAME, USERNAME_DEFAULT);
        defaults.setProperty(PASSWORD_PROPNAME, PASSWORD_DEFAULT);
        defaults.setProperty(VIRTUAL_HOST_PROPNAME, VIRTUAL_HOST_DEFAULT);
    }

    public static void main(String[] args)
    {
        PersistentTestManual test;

        Properties options =
                CommandLineParser.processCommandLine(args, new CommandLineParser(new String[][]{}), System.getProperties());

        test = new PersistentTestManual(options);
        try
        {
            test.test();
            System.out.println("Test was successfull.");
        }
        catch (Exception e)
        {
            _logger.error("Unable to test due to:" + e.getMessage(), e);
        }
    }
}
