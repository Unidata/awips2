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
 * This example illustrates the use of the JMS utility class <code>QueueRequestor</code>
 * which provides a synchronous RPC-like abstraction using temporary destinations
 * to deliver responses back to the client.
 */
public class Client
{
    /* Used in log output. */
    private static final String CLASS="Client";


    /**
     * Run the message requestor example.
     *
     * @param args Command line arguments.
     */
    public static void main(String[] args)
    {
        Client requestor=new Client();
        requestor.runTest();
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
            ConnectionFactory conFac = (ConnectionFactory) ctx.lookup("qpidConnectionfactory");

            // create the connection
            QueueConnection connection = (QueueConnection) conFac.createConnection();

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

            // Create a session on the connection.
            System.out.println(CLASS + ": Creating a non-transacted, auto-acknowledged session");
            QueueSession session = connection.createQueueSession(false, Session.AUTO_ACKNOWLEDGE);

            // Lookup the destination
            Queue destination = (Queue) ctx.lookup("requestQueue");            

            // Create a QueueRequestor
            System.out.println(CLASS + ": Creating a QueueRequestor");

            QueueRequestor requestor = new QueueRequestor(session, destination);

            // Now start the connection
            System.out.println(CLASS + ": Starting connection");
            connection.start();

            // Create a message to send as a request for service
            TextMessage request;

            // Send some messages to the server's request queue
            String[] messages = {"Twas brillig, and the slithy toves",
                                "Did gire and gymble in the wabe.",
                                "All mimsy were the borogroves,",
                                "And the mome raths outgrabe."};

            // Get the number of times that this sample should request service
            for (String message : messages)
            {
                request = session.createTextMessage(message);
                sendReceive(request, requestor);                
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

    private void sendReceive(TextMessage request, QueueRequestor requestor) throws JMSException
    {
        Message response;
        response=requestor.request(request);
        System.out.println(CLASS + ": \tRequest Content= " + request.getText());
        // Print out the details of the response received
        String text;
        if (response instanceof TextMessage)
        {
            text=((TextMessage) response).getText();
        }
        else
        {
            byte[] body=new byte[(int) ((BytesMessage) response).getBodyLength()];
            ((BytesMessage) response).readBytes(body);
            text=new String(body);
        }
        System.out.println(CLASS + ": \tResponse Content= " + text);
    }
}

