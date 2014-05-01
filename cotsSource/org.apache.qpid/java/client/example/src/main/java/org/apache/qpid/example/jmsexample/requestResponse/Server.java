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
package org.apache.qpid.example.jmsexample.requestResponse;

import javax.jms.*;
import javax.naming.Context;
import javax.naming.InitialContext;
import java.util.Properties;

/**
 * The example creates a MessageConsumer on the specified
 * Destination which is used to synchronously consume messages. If a
 * received message has a ReplyTo header then a new response message is sent
 * to that specified destination.
 */
public class Server
{
    /* Used in log output. */
    private static final String CLASS="Server";


    /**
     * Run the message mirror example.
     *
     * @param args Command line arguments.
     */
    public static void main(String[] args)
    {
        Server server=new Server();
        server.runTest();
    }

    /**
     * Start the example.
     */
    private void runTest()
    {
        try
        {
            // Load JNDI properties
            Properties properties=new Properties();
            properties.load(this.getClass().getResourceAsStream("requestResponse.properties"));

            //Create the initial context
            Context ctx=new InitialContext(properties);

            // Lookup the connection factory
            ConnectionFactory conFac=(ConnectionFactory) ctx.lookup("qpidConnectionfactory");

            // create the connection
            Connection connection=conFac.createConnection();

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
            // This session is a default choice of non-transacted and uses
            // the auto acknowledge feature of a session.
            System.out.println(CLASS + ": Creating a non-transacted, auto-acknowledged session");

            Session session=connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            // Lookup the destination
            Queue destination = (Queue) ctx.lookup("requestQueue");


            // Create a MessageConsumer
            System.out.println(CLASS + ": Creating a MessageConsumer");
            MessageConsumer messageConsumer=session.createConsumer(destination);

            /**
             * Create a MessageProducer
             */
            System.out.println(CLASS + ": Creating a MessageProducer");
            MessageProducer messageProducer;

            // Now the messageConsumer is set up we can start the connection
            System.out.println(CLASS + ": Starting connection so MessageConsumer can receive messages");
            connection.start();

            // Cycle round until all the messages are consumed.
            Message requestMessage;
            TextMessage responseMessage;
            boolean end=false;
            while (!end)
            {
                System.out.println(CLASS + ": Receiving the message");

                requestMessage=messageConsumer.receive();

                String text;
                if (requestMessage instanceof TextMessage)
                {
                    text=((TextMessage) requestMessage).getText();
                }
                else
                {
                    byte[] body=new byte[(int) ((BytesMessage) requestMessage).getBodyLength()];
                    ((BytesMessage) requestMessage).readBytes(body);
                    text=new String(body);
                }

                // Now bounce the message if a ReplyTo header was set.
                if (requestMessage.getJMSReplyTo() != null)
                {
                    System.out.println(CLASS + ": Activating response queue listener");
                    responseMessage=session.createTextMessage();

                    responseMessage.setText(text.toUpperCase());
                    System.out.println(CLASS + ": \tResponse = " + responseMessage.getText());

                    messageProducer=session.createProducer(requestMessage.getJMSReplyTo());
                    messageProducer.send(responseMessage);
                }
                System.out.println();
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
            exp.printStackTrace();
            System.err.println(CLASS + ": Caught an Exception: " + exp);
        }
    }
}
