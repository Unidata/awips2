/*
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
 */
package org.apache.qpid.example.subscriber;

import org.apache.qpid.client.AMQConnectionFactory;

import javax.jms.*;
import javax.jms.Connection;
import javax.jms.MessageConsumer;
import javax.jms.Session;
import javax.naming.InitialContext;

import org.apache.qpid.example.shared.InitialContextHelper;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

/**
 * Subscriber which consumes messages from a queue
 */

public class Subscriber
{
    private static final Logger _log = LoggerFactory.getLogger(Subscriber.class);

    protected static Connection _connection;

    protected static MessageConsumer _consumer;

    protected static InitialContextHelper _contextHelper;

    protected static AMQConnectionFactory _connectionFactory;

    protected Destination _destination;

    public Subscriber()
    {
        try
        {
            //get an initial context from default properties
            _contextHelper = new InitialContextHelper(null);
            InitialContext ctx = _contextHelper.getInitialContext();

            //then create a connection using the AMQConnectionFactory
            _connectionFactory = (AMQConnectionFactory) ctx.lookup("local");

            //lookup queue from context
            _destination = (Destination) ctx.lookup("MyQueue");

        }
        catch (Exception e)
        {
            e.printStackTrace();
            _log.error("Exception", e);
        }
    }

    /**
     * Listener class that handles messages
     */
    public static class ExampleMessageListener implements MessageListener
    {
        private String _name;

        public ExampleMessageListener(String name)
        {
            _name = name;
        }

        /**
         * Listens for message callbacks, handles and then acknowledges them
         * @param message - the message received
         */
        public void onMessage(javax.jms.Message message)
        {
            _log.info(_name + " got message '" + message + "'");

            try
            {
                //NB: Handle your message appropriately for your application here
                //do some stuff

                _log.debug("Acknowledging recieved message");

                //Now acknowledge the message to clear it from our queue
                message.acknowledge();
            }
            catch(JMSException j)
            {
                _log.error("JMSException trying to acknowledge message receipt");
                j.printStackTrace();
            }
            catch(Exception e)
            {
                _log.error("Unexpected exception trying to handle message");
                e.printStackTrace();
            }
        }
    }

    /**
     * Subscribes to example Queue and attaches listener
     */
    public void subscribe()
    {
        _log.info("Starting subscription ...");

        try
        {
             _connection = _connectionFactory.createConnection();

            //Non transactional session using client acknowledgement
            Session session =  _connection.createSession(false, Session.CLIENT_ACKNOWLEDGE);

            //Create a consumer with a destination of our queue which will use defaults for prefetch etc
            _consumer = session.createConsumer(_destination);

            //give the message listener a name of it's own
            _consumer.setMessageListener(new ExampleMessageListener("MessageListener " + System.currentTimeMillis()));

            _connection.start();
        }
        catch (Throwable t)
        {
            _log.error("Fatal error: " + t);
            t.printStackTrace();
        }

        _log.info("Waiting for messages ...");

        //wait for messages and sleep to survive failover
        try
        {
            while(true)
            {
                Thread.sleep(Long.MAX_VALUE);
            }
        }
        catch (Exception e)
        {
            _log.warn("Exception while Subscriber sleeping",e);
        }
    }

    /**
     * Stop consuming and close connection
     */
    public void stop()
    {
        try
        {
            _consumer.close();
            _consumer = null;
            _connection.stop();
            _connection.close();
        }
        catch(JMSException j)
        {
            _log.error("JMSException trying to Subscriber.stop: " + j.getStackTrace());
        }
    }

}




