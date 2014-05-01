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
package org.apache.qpid.pubsub1;

import org.apache.log4j.Logger;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.url.URLSyntaxException;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.jms.MessageProducer;
import org.apache.qpid.jms.Session;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;
import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * A client that behaves as follows:
 * <ul><li>Connects to a queue, whose name is specified as a cmd-line argument</li>
 * <li>Creates a temporary queue</li>
 * <li>Creates messages containing a property that is the name of the temporary queue</li>
 * <li>Fires off a message on the original queue and waits for a response on the temporary queue</li>
 * </ul>
 *
 */
public class TestPublisher
{
    private static final Logger _log = Logger.getLogger(TestPublisher.class);

    private AMQConnection _connection;

    private Session _session;

    private class CallbackHandler implements MessageListener
    {
        private int _expectedMessageCount;

        private int _actualMessageCount;

        private long _startTime;

        public CallbackHandler(int expectedMessageCount, long startTime)
        {
            _expectedMessageCount = expectedMessageCount;
            _startTime = startTime;
        }

        public void onMessage(Message m)
        {
            if (_log.isDebugEnabled())
            {
                _log.debug("Message received: " + m);
            }
            _actualMessageCount++;
            if (_actualMessageCount%1000 == 0)
            {
                _log.info("Received message count: " + _actualMessageCount);
            }
            /*if (!"henson".equals(m.toString()))
           {
               _log.error("AbstractJMSMessage response not correct: expected 'henson' but got " + m.toString());
           }
           else
           {
               if (_log.isDebugEnabled())
               {
                   _log.debug("AbstractJMSMessage " + m + " received");
               }
               else
               {
                   _log.info("AbstractJMSMessage received");
               }
           } */

            if (_actualMessageCount == _expectedMessageCount)
            {
                long timeTaken = System.currentTimeMillis() - _startTime;
                System.out.println("Total time taken to receive " + _expectedMessageCount+ " messages was " +
                                   timeTaken + "ms, equivalent to " +
                                   (_expectedMessageCount/(timeTaken/1000.0)) + " messages per second");
            }
        }
    }

    public TestPublisher(String host, int port, String clientID, String commandQueueName,
                                   final int messageCount) throws AMQException, URLSyntaxException
    {
        try
        {
            createConnection(host, port, clientID);
            
            _session =  _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
            AMQTopic destination = new AMQTopic(_session.getDefaultTopicExchangeName(), new AMQShortString(commandQueueName));
            MessageProducer producer = (MessageProducer) _session.createProducer(destination);

            _connection.start();
            //TextMessage msg = _session.createTextMessage(tempDestination.getQueueName() + "/Presented to in conjunction with Mahnah Mahnah and the Snowths");
            final long startTime = System.currentTimeMillis();

            for (int i = 0; i < messageCount; i++)
            {
                TextMessage msg = _session.createTextMessage(destination.getTopicName() + "/Presented to in conjunction with Mahnah Mahnah and the Snowths: " + i);

                //msg.setIntProperty("a",i % 2);
                //msg.setIntProperty("b",i % 4);

                producer.send(msg);
            }
            _log.info("Finished sending " + messageCount + " messages");
        }
        catch (JMSException e)
        {
            e.printStackTrace();
        }
    }

    private void createConnection(String host, int port, String clientID) throws AMQException, URLSyntaxException
    {
        _connection = new AMQConnection(host, port, "guest", "guest",
                                        clientID, "/test");
    }

    /**
     *
     * @param args argument 1 if present specifies the name of the temporary queue to create. Leaving it blank
     * means the server will allocate a name.
     */
    public static void main(String[] args) throws URLSyntaxException
    {
        if (args.length == 0)
        {
            System.err.println("Usage: TestPublisher <host> <port> <command queue name> <number of messages>");
        }
        try
        {
            int port = Integer.parseInt(args[1]);
            InetAddress address = InetAddress.getLocalHost();
            String clientID = address.getHostName() + System.currentTimeMillis();
            TestPublisher client = new TestPublisher(args[0], port, clientID, args[2], Integer.parseInt(args[3]));
        }
        catch (UnknownHostException e)
        {
            e.printStackTrace();
        }
        catch (AMQException e)
        {
            System.err.println("Error in client: " + e);
            e.printStackTrace();
        }

        //System.exit(0);
    }
}
