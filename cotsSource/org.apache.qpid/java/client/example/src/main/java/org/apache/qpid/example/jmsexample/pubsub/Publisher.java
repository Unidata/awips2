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
package org.apache.qpid.example.jmsexample.pubsub;


import javax.jms.*;
import javax.naming.Context;
import javax.naming.InitialContext;
import java.util.Properties;

/**
 * Publish messages to topics
 */
public class Publisher
{
    /* Used in log output. */
    private static final String CLASS="Publisher";

     /**
     * Run the message producer example.
     * @param args Command line arguments.
     */
    public static void main(String[] args)
    {
        Publisher publisher = new Publisher();
        publisher.runTest();
    }

    private void runTest()
    {
        try
        {
            Properties properties=new Properties();
            properties.load(this.getClass().getResourceAsStream("pubsub.properties"));

            //Create the initial context
            Context ctx=new InitialContext(properties);

            // Declare the connection
            ConnectionFactory conFac=(ConnectionFactory) ctx.lookup("qpidConnectionfactory");
            TopicConnection connection= (TopicConnection) conFac.createConnection();

            // Create a session on the connection
            // This session is a default choice of non-transacted and uses the auto acknowledge feature of a session.
            System.out.println(CLASS + ": Creating a non-transacted, auto-acknowledged session");
            TopicSession session=connection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);

            // Create a Message
            TextMessage message;
            System.out.println(CLASS + ": Creating a TestMessage to send to the topics");
            message=session.createTextMessage();

            // lookup the topics usa.weather
            Topic topic = (Topic)ctx.lookup("usa.weather");
            // Create a Message Publisher
            System.out.println(CLASS + ": Creating a Message Publisher for topic usa.weather");
            TopicPublisher messagePublisher=session.createPublisher(topic);
            publishMessages(message, messagePublisher);

            // lookup the topics usa.news
            topic = (Topic)ctx.lookup("usa.news");
            // Create a Message Publisher
            System.out.println(CLASS + ": Creating a Message Publisher for topic usa.news");
            messagePublisher=session.createPublisher(topic);
            publishMessages(message, messagePublisher);

            // lookup the topics europe.weather
            topic = (Topic)ctx.lookup("europe.weather");
            // Create a Message Publisher
            System.out.println(CLASS + ": Creating a Message Publisher for topic europe.weather");
            messagePublisher=session.createPublisher(topic);
            publishMessages(message, messagePublisher);

            // lookup the topics europe.news
            topic = (Topic)ctx.lookup("europe.news");
            // Create a Message Publisher
            System.out.println(CLASS + ": Creating a Message Publisher for topic europe.news");
            messagePublisher = session.createPublisher(topic);
            publishMessages(message, messagePublisher);

            // send the final message
            message=session.createTextMessage("That's all, folks!");
            topic = (Topic)ctx.lookup("control");
            // Create a Message Publisher
            messagePublisher = session.createPublisher(topic);
            messagePublisher
                    .send(message, DeliveryMode.PERSISTENT, Message.DEFAULT_PRIORITY, Message.DEFAULT_TIME_TO_LIVE);


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
        }
    }

    private void publishMessages(TextMessage message, TopicPublisher messagePublisher) throws JMSException
    {
        // Loop to publish 5 messages.
        for (int i=1; i <= 6; i++)
        {
            message.setText("message " + i);
            System.out.println(CLASS + ": Sending " + message.getText());
            messagePublisher
                    .send(message, DeliveryMode.PERSISTENT, Message.DEFAULT_PRIORITY, Message.DEFAULT_TIME_TO_LIVE);
        }
    }
}
