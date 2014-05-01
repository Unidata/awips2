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
package org.apache.qpid.example.jmsexample.transacted;

import javax.jms.*;
import javax.naming.Context;
import javax.naming.InitialContext;
import java.util.Properties;

/**
 * Transactional message example sends a number of messages to a Queue
 * and then uses a transacted session to move them from the Queue to a Topic.
 * <p/>
 * <p>The program completes the following steps:
 * <ul>
 * <li>Publish the specified number of messages to the queue.</li>
 * <li>Within a transacted session consume all messages from the queue
 * and publish them to the topic.</li>
 * <li>By default commit the transacted session, unless the "<code>-rollback true</code>"
 * option is specified in which case roll it back.</li>
 * <li>Check for outstanding messages on the queue.</li>
 * <li>Check for outstanding messages on the topic.</li>
 * </ul>
 * <p/>
 */
public class QueueToTopic
{
    /* Used in log output. */
    private static final String CLASS="QueueToTopic";


    /* Specify if the transaction is committed */
    private boolean _commit;

    /**
     * Create a QueueToTopic client.
     *
     * @param commit Specifies if the transaction should be committed.
     */
    public QueueToTopic(boolean commit)
    {
        _commit=commit;
    }

    /**
     * Run the message mover example.
     *
     * @param args Command line arguments.
     */
    public static void main(String[] args)
    {
        boolean commit=true;
        if (args.length > 1)
        {
            if (args[0].equalsIgnoreCase("-rollback"))
            {
                commit=!Boolean.getBoolean(args[1]);
            }
        }
        QueueToTopic mover=new QueueToTopic(commit);
        mover.runTest();
    }

    private void runTest()
    {
        try
        {
            // Load JNDI properties
            Properties properties=new Properties();
            properties.load(this.getClass().getResourceAsStream("transacted.properties"));

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
                    System.err.println(CLASS + ": The sample received an exception through the ExceptionListener");
                    System.exit(0);
                }
            });

            // Start the connection
            connection.start();

            /**
             * Create nonTransactedSession. This non-transacted auto-ack session is used to create the MessageProducer
             * that is used to populate the queue and the MessageConsumer that is used to consume the messages
             * from the topic.
             */
            System.out.println(CLASS + ": Creating a non-transacted, auto-acknowledged session");
            Session nonTransactedSession=connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            // Lookup the queue
            Queue queue=(Queue) ctx.lookup("transactedQueue");

            // Lookup the topic
            Topic topic=(Topic) ctx.lookup("transactedTopic");

            // Make sure that the queue is empty
            System.out.print(CLASS + ": Purging messages from queue...");
            MessageConsumer queueMessageConsumer=nonTransactedSession.createConsumer(queue);
            Message purgedMessage;
            int numberPurged=-1;
            do
            {
                purgedMessage=queueMessageConsumer.receiveNoWait();
                numberPurged++;
            }
            while (purgedMessage != null);
            System.out.println(numberPurged + " message(s) purged.");

            // Create the MessageProducer for the queue
            System.out.println(CLASS + ": Creating a MessageProducer for the queue");
            MessageProducer messageProducer=nonTransactedSession.createProducer(queue);

            // Now create the MessageConsumer for the topic
            System.out.println(CLASS + ": Creating a MessageConsumer for the topic");
            MessageConsumer topicMessageConsumer=nonTransactedSession.createConsumer(topic);

            // Create a textMessage. We're using a TextMessage for this example.
            System.out.println(CLASS + ": Creating a TestMessage to send to the destination");
            TextMessage textMessage=nonTransactedSession.createTextMessage("Sample text message");

            // Loop to publish the requested number of messages to the queue.
            for (int i=1; i <= 5; i++)
            {
                messageProducer
                        .send(textMessage, DeliveryMode.PERSISTENT, Message.DEFAULT_PRIORITY,
                                Message.DEFAULT_TIME_TO_LIVE);

                // Print out details of textMessage just sent
                System.out.println(CLASS + ": Message sent: " + i + " " + textMessage.getJMSMessageID());
            }

            // Create a new transacted Session to move the messages from the queue to the topic
            Session transactedSession=connection.createSession(true, Session.SESSION_TRANSACTED);

            // Create a new message consumer from the queue
            MessageConsumer transactedConsumer=transactedSession.createConsumer(queue);

            // Create a new message producer for the topic
            MessageProducer transactedProducer=transactedSession.createProducer(topic);

            // Loop to consume the messages from the queue and publish them to the topic
            Message receivedMessage;
            for (int i=1; i <= 5; i++)
            {
                // Receive a message
                receivedMessage=transactedConsumer.receive();
                System.out.println(CLASS + ": Moving message: " + i + " " + receivedMessage.getJMSMessageID());
                // Publish it to the topic
                transactedProducer.send(receivedMessage);
            }

            // Either commit or rollback the transacted session based on the command line args.
            if (_commit)
            {
                System.out.println(CLASS + ": Committing transacted session.");
                transactedSession.commit();
            }
            else
            {
                System.out.println(CLASS + ": Rolling back transacted session.");
                transactedSession.rollback();
            }

            // Now consume any outstanding messages on the queue
            System.out.print(CLASS + ": Mopping up messages from queue");
            if (_commit)
            {
                System.out.print(" (expecting none)...");
            }
            else
            {
                System.out.print(" (expecting " + 5 + ")...");
            }

            Message moppedMessage;
            int numberMopped=0;
            do
            {
                moppedMessage=queueMessageConsumer.receiveNoWait();
                if (moppedMessage != null)
                {
                    numberMopped++;
                }
            }
            while (moppedMessage != null);
            System.out.println(numberMopped + " message(s) mopped.");

            // Now consume any outstanding messages for the topic subscriber
            System.out.print(CLASS + ": Mopping up messages from topic");

            if (_commit)
            {
                System.out.print(" (expecting " + 5 + ")...");
            }
            else
            {
                System.out.print(" (expecting none)...");
            }

            numberMopped=0;
            do
            {
                moppedMessage=topicMessageConsumer.receiveNoWait();
                if (moppedMessage != null)
                {
                    numberMopped++;
                }
            }
            while (moppedMessage != null);
            System.out.println(numberMopped + " message(s) mopped.");

            // Close the QueueConnection to the server
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
