/* Licensed to the Apache Software Foundation (ASF) under one
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
package org.apache.qpid.test.unit.ct;

import javax.jms.*;

import org.apache.qpid.test.utils.QpidTestCase;

/**
 *   Crash Recovery tests for durable subscription
 *
 */
public class DurableSubscriberTest extends QpidTestCase
{
    private final String _topicName = "durableSubscriberTopic";

    /**
     * test strategy:
     * create and register a durable subscriber then close it
     * create a publisher and send a persistant message followed by a non persistant message
     * crash and restart the broker
     * recreate the durable subscriber and check that only the first message is received
     */
    public void testDurSubRestoredAfterNonPersistentMessageSent() throws Exception
    {
        if (!isBroker08())
        {
            TopicConnectionFactory factory = getConnectionFactory();
            Topic topic = (Topic) getInitialContext().lookup(_topicName);
            //create and register a durable subscriber then close it
            TopicConnection durConnection = factory.createTopicConnection("guest", "guest");
            TopicSession durSession = durConnection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            TopicSubscriber durSub1 = durSession.createDurableSubscriber(topic, "dursub");
            durConnection.start();
            durSub1.close();
            durSession.close();
            durConnection.stop();

            //create a publisher and send a persistant message followed by a non persistant message
            TopicConnection pubConnection = factory.createTopicConnection("guest", "guest");
            TopicSession pubSession = pubConnection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            TopicPublisher publisher = pubSession.createPublisher(topic);
            Message message = pubSession.createMessage();
            message.setIntProperty("count", 1);
            publisher.publish(message, javax.jms.DeliveryMode.PERSISTENT, javax.jms.Message.DEFAULT_PRIORITY,
                              javax.jms.Message.DEFAULT_TIME_TO_LIVE);
            message.setIntProperty("count", 2);
            publisher.publish(message, javax.jms.DeliveryMode.NON_PERSISTENT, javax.jms.Message.DEFAULT_PRIORITY,
                              javax.jms.Message.DEFAULT_TIME_TO_LIVE);
            publisher.close();
            pubSession.close();
            //now stop the server
            try
            {
                restartBroker();
            }
            catch (Exception e)
            {
                System.out.println("problems shutting down arjuna-ms");
                throw e;
            }
            //now recreate the durable subscriber and check the received messages
            factory = getConnectionFactory();
            topic = (Topic) getInitialContext().lookup(_topicName);
            TopicConnection durConnection2 = factory.createTopicConnection("guest", "guest");
            TopicSession durSession2 = durConnection2.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            TopicSubscriber durSub2 = durSession2.createDurableSubscriber(topic, "dursub");
            durConnection2.start();
            Message m1 = durSub2.receive(1000);
            if (m1 == null)
            {
                assertTrue("testDurSubRestoredAfterNonPersistentMessageSent test failed. no message was returned",
                           false);
            }
            assertTrue("testDurSubRestoredAfterNonPersistentMessageSent test failed. Wrong message was returned.",
                       m1.getIntProperty("count") == 1);
            durSession2.unsubscribe("dursub");
            durConnection2.close();
        }
    }

    /**
     * create and register a durable subscriber with a message selector and then close it
     * crash the broker
     * create a publisher and send  5 right messages and 5 wrong messages
     * recreate the durable subscriber and check the received the 5 expected messages
     */
    public void testDurSubRestoresMessageSelector() throws Exception
    {
        if (!isBroker08())
        {
            TopicConnectionFactory factory = getConnectionFactory();
            Topic topic = (Topic) getInitialContext().lookup(_topicName);
            //create and register a durable subscriber with a message selector and then close it
            TopicConnection durConnection = factory.createTopicConnection("guest", "guest");
            TopicSession durSession = durConnection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            TopicSubscriber durSub1 = durSession.createDurableSubscriber(topic, "dursub", "testprop='true'", false);
            durConnection.start();
            durSub1.close();
            durSession.close();
            durConnection.stop();
            //now stop the server
            try
            {
                restartBroker();
            }
            catch (Exception e)
            {
                System.out.println("problems shutting down arjuna-ms");
                throw e;
            }
            topic = (Topic) getInitialContext().lookup(_topicName);
            factory = getConnectionFactory();
            TopicConnection pubConnection = factory.createTopicConnection("guest", "guest");
            TopicSession pubSession = pubConnection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            TopicPublisher publisher = pubSession.createPublisher(topic);
            for (int i = 0; i < 5; i++)
            {
                Message message = pubSession.createMessage();
                message.setStringProperty("testprop", "true");
                publisher.publish(message);
                message = pubSession.createMessage();
                message.setStringProperty("testprop", "false");
                publisher.publish(message);
            }
            publisher.close();
            pubSession.close();

            //now recreate the durable subscriber and check the received messages
            TopicConnection durConnection2 = factory.createTopicConnection("guest", "guest");
            TopicSession durSession2 = durConnection2.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            TopicSubscriber durSub2 = durSession2.createDurableSubscriber(topic, "dursub");
            durConnection2.start();
            for (int i = 0; i < 5; i++)
            {
                Message message = durSub2.receive(1000);
                if (message == null)
                {
                    assertTrue("testDurSubRestoresMessageSelector test failed. no message was returned", false);
                }
                else
                {
                    assertTrue("testDurSubRestoresMessageSelector test failed. message selector not reset",
                               message.getStringProperty("testprop").equals("true"));
                }
            }
            durSession2.unsubscribe("dursub");
            durConnection2.close();
        }
    }
}

