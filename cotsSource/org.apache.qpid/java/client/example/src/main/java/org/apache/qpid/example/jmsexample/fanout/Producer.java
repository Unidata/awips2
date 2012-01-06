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
package org.apache.qpid.example.jmsexample.fanout;

import java.util.Properties;

import javax.jms.*;
import javax.naming.Context;
import javax.naming.InitialContext;

/**
 * Message producer example, sends message to a queue.
 */
public class Producer
{
    /* Used in log output. */
    private static final String CLASS = "Producer";

    /* The queue name  */
    private int numMessages = 10;
    private short deliveryMode = DeliveryMode.NON_PERSISTENT;


    /**
     * Run the message producer example.
     * @param args Command line arguments.
     */
    public static void main(String[] args)
    {
        Producer producer = new Producer();
        producer.runTest();
    }

    private void runTest()
    {
        try
        {

            Properties properties = new Properties();
            properties.load(this.getClass().getResourceAsStream("fanout.properties"));

            //Create the initial context
            Context ctx = new InitialContext(properties);

            Destination destination = (Destination)ctx.lookup("fanoutQueue");

            // Declare the connection
            ConnectionFactory conFac = (ConnectionFactory)ctx.lookup("qpidConnectionfactory");
            Connection connection = conFac.createConnection();

            // Create a session on the connection
            // This session is a default choice of non-transacted and uses the auto acknowledge feature of a session.
            System.out.println(CLASS + ": Creating a non-transacted, auto-acknowledged session");
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            // lookup the queue
            //Queue   destination = session.createQueue(_queueName);

            // Create a Message producer
            System.out.println(CLASS + ": Creating a Message Producer");
            MessageProducer messageProducer = session.createProducer(destination);

            // Create a Message
            TextMessage message;
            System.out.println(CLASS + ": Creating a TestMessage to send to the destination");

            // Loop to publish the requested number of messages.
            for (int i = 1; i < numMessages + 1; i++)
            {
                // NOTE: We have NOT HAD TO START THE CONNECTION TO BEGIN SENDING  messages,
                // this is different to the consumer end as a CONSUMERS CONNECTIONS MUST BE STARTED BEFORE RECEIVING.
                message = session.createTextMessage("Message " + i);
                System.out.println(CLASS + ": Sending message: " + i);
                messageProducer.send(message, deliveryMode, Message.DEFAULT_PRIORITY, Message.DEFAULT_TIME_TO_LIVE);
            }

            // And send a final message to indicate termination.
            message = session.createTextMessage("That's all, folks!");
            messageProducer.send(message, deliveryMode, Message.DEFAULT_PRIORITY, Message.DEFAULT_TIME_TO_LIVE);

            // Close the connection to the broker
            System.out.println(CLASS + ": Closing connection");
            connection.close();

            // Close the JNDI reference
            System.out.println(CLASS + ": Closing JNDI context");
            ctx.close();
        }
        catch (Exception exp)
        {
            System.err.println(CLASS + ": Caught an Exception: " + exp);
            exp.printStackTrace();
        }
    }
}
