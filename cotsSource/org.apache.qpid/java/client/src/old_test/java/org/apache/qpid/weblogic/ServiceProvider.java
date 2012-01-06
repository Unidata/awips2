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
package org.apache.qpid.weblogic;

import org.apache.log4j.Logger;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQDestination;

import javax.jms.*;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.naming.Context;
import java.net.InetAddress;
import java.util.Hashtable;

public class ServiceProvider
{
    private static final String JNDI_FACTORY = "weblogic.jndi.WLInitialContextFactory";
    private static final String JMS_FACTORY = "transientJMSConnectionFactory";

    private static final Logger _logger = Logger.getLogger(ServiceProvider.class);

    private static MessageProducer _destinationProducer;

    private static Queue _destinationQ;

    public static void main(String[] args)
    {
        _logger.info("Starting...");

        if (args.length != 2)
        {
            System.out.println("Usage: <WLS URI> <service queue>");
            System.exit(1);
        }
        try
        {
            String url = args[0];
            String receiveQueue = args[1];

            final InitialContext ctx = getInitialContext(url);

            QueueConnectionFactory qconFactory = (QueueConnectionFactory) ctx.lookup(JMS_FACTORY);
            QueueConnection qcon = qconFactory.createQueueConnection();
            final QueueSession qsession = qcon.createQueueSession(false, Session.CLIENT_ACKNOWLEDGE);
            Queue receiveQ = (Queue) ctx.lookup(receiveQueue);

            _logger.info("Service (queue) name is '" + receiveQ + "'...");

            String selector = (args.length > 2 && args[2] != null && args[2].length() > 1) ? args[2] : null;

            _logger.info("Message selector is <" + selector + ">...");

            MessageConsumer consumer = qsession.createConsumer(receiveQ, selector);

            consumer.setMessageListener(new MessageListener()
            {
                private int _messageCount;

                public void onMessage(javax.jms.Message message)
                {
                    //_logger.info("Got message '" + message + "'");

                    TextMessage tm = (TextMessage) message;

                    try
                    {
                        Queue responseQueue = (Queue)tm.getJMSReplyTo();
                        if (!responseQueue.equals(_destinationQ))
                        {
                            _destinationQ = responseQueue;
                            _logger.info("Creating destination for " + responseQueue);

                            try
                            {
                                _destinationProducer = qsession.createProducer(_destinationQ);
                                _destinationProducer.setDeliveryMode(DeliveryMode.NON_PERSISTENT);
                            }
                            catch (JMSException e)
                            {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                        }
                        _messageCount++;
                        if (_messageCount % 1000 == 0)
                        {
                            _logger.info("Received message total: " + _messageCount);
                            _logger.info("Sending response to '" + responseQueue + "'");
                        }

                        String payload = "This is a response: sing together: 'Mahnah mahnah...'" + tm.getText();
                        TextMessage msg = qsession.createTextMessage(payload);
                        if (tm.propertyExists("timeSent"))
                        {
                            _logger.info("timeSent property set on message");
                            final long timeSent = tm.getLongProperty("timeSent");
                            msg.setLongProperty("timeSent", timeSent);
                            _logger.info("time taken to go from service request to provider is: " + (System.currentTimeMillis() - timeSent));
                        }
                        _destinationProducer.send(msg);
                        if (_messageCount % 1000 == 0)
                        {
                            tm.acknowledge();
                            _logger.info("Sent response to '" + responseQueue + "'");
                        }
                    }
                    catch (JMSException e)
                    {
                        _logger.error("Error sending message: " + e, e);
                    }
                }
            });
            qcon.start();
        }
        catch (Throwable t)
        {
            System.err.println("Fatal error: " + t);
            t.printStackTrace();
        }


        System.out.println("Waiting...");
    }

    private static InitialContext getInitialContext(String url) throws NamingException
    {
        Hashtable env = new Hashtable();
        env.put(Context.INITIAL_CONTEXT_FACTORY, JNDI_FACTORY);
        env.put(Context.PROVIDER_URL, url);
        return new InitialContext(env);
    }
}
