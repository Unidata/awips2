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
package org.apache.qpid.example.jmsexample.direct;

import java.util.Properties;

import javax.jms.BytesMessage;
import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.naming.Context;
import javax.naming.InitialContext;

/**
 * The example creates a MessageConsumer on the specified
 * Queue which is used to synchronously consume messages.
 */
public class Consumer
{
    /**
     * Used in log output.
     */
    private static final String CLASS = "Consumer";


    /**
     * Run the message consumer example.
     * @param args Command line arguments.
     */
    public static void main(String[] args)
    {
        Consumer syncConsumer = new Consumer();
        syncConsumer.runTest();
    }

    /**
     * Start the example.
     */
    private void runTest()
    {
        try
        {
            // Load JNDI properties
            Properties properties = new Properties();
            properties.load(this.getClass().getResourceAsStream("direct.properties"));

            //Create the initial context
            Context ctx = new InitialContext(properties);

            // look up destination
            Destination destination = (Destination)ctx.lookup("directQueue");

            // Lookup the connection factory
            ConnectionFactory conFac = (ConnectionFactory)ctx.lookup("qpidConnectionfactory");
            // create the connection
            Connection connection = conFac.createConnection();

            // As this application is using a MessageConsumer we need to set an ExceptionListener on the connection
            // so that errors raised within the JMS client library can be reported to the application
            System.out.println(
                    CLASS + ": Setting an ExceptionListener on the connection as sample uses a MessageConsumer");

            connection.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException jmse)
                {
                    // The connection may have broken invoke reconnect code if available.
                    // The connection may have broken invoke reconnect code if available.
                    System.err.println(CLASS + ": The sample received an exception through the ExceptionListener");
                    System.exit(0);
                }
            });

            // Create a session on the connection
            // This session is a default choice of non-transacted and uses the auto acknowledge feature of a session.
            System.out.println(CLASS + ": Creating a non-transacted, auto-acknowledged session");
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            // Create a MessageConsumer
            System.out.println(CLASS + ": Creating a MessageConsumer");
            MessageConsumer messageConsumer = session.createConsumer(destination);

            // Now the messageConsumer is set up we can start the connection
            System.out.println(CLASS + ": Starting connection so MessageConsumer can receive messages");
            connection.start();

            // Cycle round until all the messages are consumed.
            Message message;
            boolean end = false;
            while (!end)
            {
                message = messageConsumer.receive();
                String text;
                if (message instanceof TextMessage)
                {
                    text = ((TextMessage) message).getText();
                }
                else
                {
                    byte[] body = new byte[(int) ((BytesMessage) message).getBodyLength()];
                    ((BytesMessage) message).readBytes(body);
                    text = new String(body);
                }
                if (text.equals("That's all, folks!"))
                {
                    System.out.println(CLASS + ": Received final message " + text);
                    end = true;
                }
                else
                {
                    System.out.println(CLASS + ": Received  message:  " + text);
                }
            }

            // Close the connection to the server
            System.out.println(CLASS + ": Closing connection");
            connection.close();

            // Close the JNDI reference
            System.out.println(CLASS + ": Closing JNDI context");
            ctx.close();
        }
        catch (Exception exp)
        {
            System.err.println(CLASS + ": Caught an Exception: " + exp);
        }
    }
}
