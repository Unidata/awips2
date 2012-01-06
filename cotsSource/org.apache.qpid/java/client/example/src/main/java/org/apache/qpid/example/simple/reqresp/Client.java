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

package org.apache.qpid.example.simple.reqresp;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;

public class Client implements MessageListener
{
    final String BROKER = "localhost";

    final String INITIAL_CONTEXT_FACTORY = "org.apache.qpid.jndi.PropertiesFileInitialContextFactory";

    final String CONNECTION_JNDI_NAME = "local";
    final String CONNECTION_NAME = "amqp://guest:guest@clientid/test?brokerlist='" + BROKER + "'";

    final String QUEUE_JNDI_NAME = "queue";
    final String QUEUE_NAME = "example.RequestQueue";


    private InitialContext _ctx;

    private CountDownLatch _shutdownHook = new CountDownLatch(1);

    public Client()
    {
        setupJNDI();

        Connection connection;
        Session session;
        Destination responseQueue;

        //Setup the connection. Create producer to sent message and consumer to receive the repsonse.
        MessageProducer _producer;
        try
        {
            connection = ((ConnectionFactory) lookupJNDI(CONNECTION_JNDI_NAME)).createConnection();

            session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            Destination requestQueue = (Queue) lookupJNDI(QUEUE_JNDI_NAME);

            closeJNDI();

            //Setup a message _producer to send message to the queue the server is consuming from
            _producer = session.createProducer(requestQueue);

            //Create a temporary queue that this client will listen for responses on then create a consumer
            //that consumes message from this temporary queue.
            responseQueue = session.createTemporaryQueue();

            MessageConsumer responseConsumer = session.createConsumer(responseQueue);

            //Set a listener to asynchronously deal with responses.
            responseConsumer.setMessageListener(this);

            // Now the connection is setup up start it.
            connection.start();
        }
        catch (JMSException e)
        {
            System.err.println("Unable to setup connection, client and producer on broker");
            return;
        }

        // Setup the message to send
        TextMessage txtMessage;
        try
        {
            //Now create the actual message you want to send
            txtMessage = session.createTextMessage("Request Process");

            //Set the reply to field to the temp queue you created above, this is the queue the server will respond to
            txtMessage.setJMSReplyTo(responseQueue);

            //Set a correlation ID so when you get a response you know which sent message the response is for
            //If there is never more than one outstanding message to the server then the
            //same correlation ID can be used for all the messages...if there is more than one outstanding
            //message to the server you would presumably want to associate the correlation ID with this message

            txtMessage.setJMSCorrelationID(txtMessage.getJMSMessageID());
        }
        catch (JMSException e)
        {
            System.err.println("Unable to create message");
            return;

        }

        try
        {
            _producer.send(txtMessage);
        }
        catch (JMSException e)
        {
            //Handle the exception appropriately
        }

        try
        {
            System.out.println("Sent Request Message ID :" + txtMessage.getJMSMessageID());
        }
        catch (JMSException e)
        {
            //Handle exception more appropriately.
        }

        //Wait for the return message to arrive
        try
        {
            _shutdownHook.await();
        }
        catch (InterruptedException e)
        {
            // Ignore this as we are quitting anyway.
        }

        //Close the connection
        try
        {
            connection.close();
        }
        catch (JMSException e)
        {
            System.err.println("A problem occured while shutting down the connection : " + e);
        }
    }


    /**
     * Implementation of the Message Listener interface.
     * This is where message will be asynchronously delivered.
     *
     * @param message
     */
    public void onMessage(Message message)
    {
        String messageText;
        try
        {
            if (message instanceof TextMessage)
            {
                TextMessage textMessage = (TextMessage) message;
                messageText = textMessage.getText();
                System.out.println("messageText = " + messageText);
                System.out.println("Correlation ID " + message.getJMSCorrelationID());

                _shutdownHook.countDown();
            }
            else
            {
                System.err.println("Unexpected message delivered");
            }
        }
        catch (JMSException e)
        {
            //Handle the exception appropriately
        }
    }

    /**
     * Lookup the specified name in the JNDI Context.
     *
     * @param name The string name of the object to lookup
     *
     * @return The object or null if nothing exists for specified name
     */
    private Object lookupJNDI(String name)
    {
        try
        {
            return _ctx.lookup(name);
        }
        catch (NamingException e)
        {
            System.err.println("Error looking up '" + name + "' in JNDI Context:" + e);
        }

        return null;
    }

    /**
     * Setup the JNDI context.
     *
     * In this case we are simply using a Properties object to store the pairing information.
     *
     * Further details can be found on the wiki site here:
     *
     * @see : http://cwiki.apache.org/qpid/how-to-use-jndi.html
     */
    private void setupJNDI()
    {
        // Set the properties ...
        Properties properties = new Properties();
        properties.put(Context.INITIAL_CONTEXT_FACTORY, INITIAL_CONTEXT_FACTORY);
        properties.put("connectionfactory." + CONNECTION_JNDI_NAME, CONNECTION_NAME);
        properties.put("queue." + QUEUE_JNDI_NAME, QUEUE_NAME);

        // Create the initial context
        Context ctx = null;
        try
        {
            _ctx = new InitialContext(properties);
        }
        catch (NamingException e)
        {
            System.err.println("Error Setting up JNDI Context:" + e);
        }
    }

    /** Close the JNDI Context to keep everything happy. */
    private void closeJNDI()
    {
        try
        {
            _ctx.close();
        }
        catch (NamingException e)
        {
            System.err.println("Unable to close JNDI Context : " + e);
        }
    }


    public static void main(String[] args)
    {
        new Client();
    }
}

