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
package org.apache.qpid.fragmentation;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.jms.Session;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.log4j.Logger;

import javax.jms.*;
import java.net.InetAddress;

public class TestLargeSubscriber
{
    private static final Logger _logger = Logger.getLogger(TestLargeSubscriber.class);

    private static MessageProducer _destinationProducer;

    private static String _destinationName;

    public static void main(String[] args)
    {
        _logger.info("Starting...");

        final String host;
        final int port;
        final String username;
        final String password;
        final String virtualPath;
        final int numExpectedMessages;
        if (args.length == 0)
        {
            host = "localhost";
            port = 5672;
            username = "guest";
            password = "guest";
            virtualPath = "/test";
            numExpectedMessages = 100;
        }
        else if (args.length == 6)
        {
            host = args[0];
            port  = Integer.parseInt(args[1]);
            username = args[2];
            password = args[3];
            virtualPath = args[4];
            numExpectedMessages = Integer.parseInt(args[5]);
        }
        else
        {
            System.out.println("Usage: host port username password virtual-path expectedMessageCount");
            System.exit(1);
            throw new RuntimeException("cannot be reached");
        }

        try
        {
            InetAddress address = InetAddress.getLocalHost();
            AMQConnection con = new AMQConnection(host, port, username, password,
                                                  address.getHostName(), virtualPath);
            final AMQSession session = (AMQSession) con.createSession(false, Session.AUTO_ACKNOWLEDGE);

            final int expectedMessageCount = numExpectedMessages;

            MessageConsumer consumer = session.createConsumer(new AMQTopic(session.getDefaultTopicExchangeName(),
                                                                           new AMQShortString("large")),
                                                              100, true, false, null);

            consumer.setMessageListener(new MessageListener()
            {
                private int _messageCount;

                private long _startTime = 0;

                public void onMessage(Message message)
                {
                    validateMessage(message);
                    if (_messageCount++ == 0)
                    {
                        _startTime = System.currentTimeMillis();
                    }
                    if (_logger.isInfoEnabled())
                    {
                        _logger.info("Got message '" + message + "'");
                    }
                    if (_messageCount == expectedMessageCount)
                    {
                        long totalTime = System.currentTimeMillis() - _startTime;
                        _logger.error("Total time to receive " + _messageCount + " messages was " +
                                      totalTime + "ms. Rate is " + (_messageCount/(totalTime/1000.0)));
                    }
                }

                private void validateMessage(Message message)
                {
                    if (!(message instanceof BytesMessage))
                    {
                        _logger.error("Message is not of correct type - should be BytesMessage and is " +
                                      message.getClass());
                    }
                    BytesMessage bm = (BytesMessage) message;
                    final int expectedSize = 1024 * 187; // 187k
                    try
                    {
                        if (bm.getBodyLength() != expectedSize)
                        {
                            _logger.error("Message is not correct length - should be  " + expectedSize + " and is " +
                                          bm.getBodyLength());
                        }
                    }
                    catch (JMSException e)
                    {
                        _logger.error("Failed to validate message: " + e, e);
                    }
                    try
                    {
                        byte[] data = new byte[(int)bm.getBodyLength()];
                        bm.readBytes(data);
                        for (int i = 0; i < data.length; i++)
                        {
                            if (data[i] != (byte)(i%25))
                            {
                                _logger.error("byte " + i + " of message is wrong - should be " + i%25 + " but is " +
                                              data[i]);
                            }
                        }
                        _logger.info("***** Validated message successfully");
                    }
                    catch (JMSException e)
                    {
                        _logger.error("Failed to validate message: " + e, e);
                    }
                }
            });
            con.start();
        }
        catch (Throwable t)
        {
            System.err.println("Fatal error: " + t);
            t.printStackTrace();
        }

        System.out.println("Waiting...");
    }
}

