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
package org.apache.qpid.test.unit.close;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.jndi.PropertiesFileInitialContextFactory;

import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.naming.InitialContext;
import java.util.ArrayList;

/**
 * QPID-1791
 *
 * The threading model in the Java broker (at least till 0.5) allows for the
 * close to be handled immediately even if the broker is still processing state
 * for that Session.
 *
 * This test verifys that QPID-1791 is has been handled.
 *
 * The problem was that the whilst the Session is busy processing Acks from the
 * client the Close frame jumps in and clears the unAcknowledgeMap in an
 * attempt to start processing them for closing the connection.
 *
 * If the session had a consumer consuming from a temporary queue. The closing
 * thread dequeues and deletes the message that were on the uncknowledgedMap.
 *
 * However, the Acking thread currently does:
 * queuEntry = unackedMap.get(messageID)
 *
 * dequeueAndDelete(queueEntry)
 *
 * unackedMap.remove(messageID)
 *
 * As a result the queueEntry is sitting in the unackedMap whilst it is being
 * dequeuedAndDeleted which leaves the opportunity for the close thread to
 * remove contents of the unackedMap for processing. The close thread will then
 * dequeueAndDelete all these values one of which the acking thread is currently
 * processing.
 *
 *
 * Test Approach
 *
 * Send a lot of persistent messages (5000), the goal of which is to fill the
 * pretch and to provide the broker with a lot of acks to process
 *
 * Using client ack and prefetch buffer of 5000 use receive to get 2500
 * Use AMQMessage.acknowledgeThis() to send a single ack frame back to the
 * broker per message so 2500 ack frames.
 * This will give the broker a lot to process,
 * Immediately send the consumer close after the acks are all gone.
 * This will cause the remaining 2500 prefetched messages plus any that have
 * not yet had their acks processed
 * to be collected by the requeue() process potentially
 */
public class VerifyAckingOkDuringClose
{

    static final int MESSAGE_SENT = 5000;

    public static void main(String[] args) throws Exception
    {
        //Check that we have the InitialContext Configured

        if (System.getProperty(InitialContext.INITIAL_CONTEXT_FACTORY) == null)
        {
            System.setProperty(InitialContext.INITIAL_CONTEXT_FACTORY, PropertiesFileInitialContextFactory.class.getName());
        }

        if (System.getProperty(InitialContext.PROVIDER_URL) == null)
        {
            System.err.println(InitialContext.PROVIDER_URL + ": Is not set and is required to contain a 'default' ConnectionFactory.");
            System.exit(1);
        }

        //Retreive the local factory from the properties file
        // when used with perftest.properties this will be localhost:5672
        AMQConnectionFactory factory = (AMQConnectionFactory) new InitialContext().lookup("default");

        AMQConnection connection = (AMQConnection) factory.createConnection("guest", "guest");

        //Use the AMQConnection Interface to set the prefetch to the number
        // we are sending
        Session session = connection.createSession(false,
                                                   Session.CLIENT_ACKNOWLEDGE,
                                                   MESSAGE_SENT);

        Queue queue = session.createTemporaryQueue();

        MessageConsumer consumer = session.createConsumer(queue);
        connection.start();
       
        MessageProducer producer = session.createProducer(queue);

        Message message = session.createTextMessage("Close");

        for (int i = 0; i < MESSAGE_SENT; i++)
        {
            message.setIntProperty("SequenceNumber", i);

            producer.send(message);
        }

        // Put a reasonable about of data on the queue.

        //Receive all the messags
        ArrayList<Message> received = new ArrayList<Message>();

        message = consumer.receive(2000);

        while (message != null)
        {
            received.add(message);
            message = consumer.receive(2000);
        }

        //Check we have all the messages
        if (received.size() != MESSAGE_SENT)
        {
            System.err.println("Test Failed Not all the messages received:" + received.size());
            System.exit(1);
        }

        //individually ack the first half then close
        for (int i = 0; i < MESSAGE_SENT / 2; i++)
        {
            ((org.apache.qpid.jms.Message) received.get(i)).acknowledgeThis();
        }

        // Close the Session to force a requeue on the server of the unackedMsgs

        System.out.println("Killing client to force requeue on broker");

        System.exit(1);
    }

}
