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

package org.apache.qpid.test.utils;

import org.apache.log4j.Logger;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.client.AMQConnectionURL;
import org.apache.qpid.client.JMSAMQException;
import org.apache.qpid.url.URLSyntaxException;

import javax.jms.Connection;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;

/**
 * @todo This was originally cut and paste from the client module leading to a duplicate class, then altered very
 *       slightly. To avoid the duplicate class the name was altered slightly to have 'Helper' on the end in order
 *       to distinguish it from the original. Delete this class and use the original instead, just upgrade it to
 *       provide the new features needed.
 */
public class QpidClientConnectionHelper implements ExceptionListener
{

    private static final Logger _logger = Logger.getLogger(QpidClientConnectionHelper.class);

    private boolean transacted = true;
    private int ackMode = Session.CLIENT_ACKNOWLEDGE;
    private Connection connection;

    private String virtualHost;
    private String brokerlist;
    private int prefetch;
    protected Session session;
    protected boolean connected;

    public QpidClientConnectionHelper(String broker)
    {
        super();
        setVirtualHost("/test");
        setBrokerList(broker);
        setPrefetch(5000);
    }

    public void connect() throws JMSException
    {
        if (!connected)
        {
            /*
             * amqp://[user:pass@][clientid]/virtualhost?
             * brokerlist='[transport://]host[:port][?option='value'[&option='value']];'
             * [&failover='method[?option='value'[&option='value']]']
             * [&option='value']"
             */
            String brokerUrl = "amqp://guest:guest@" + virtualHost + "?brokerlist='" + brokerlist + "'";
            try
            {
                AMQConnectionFactory factory = new AMQConnectionFactory(new AMQConnectionURL(brokerUrl));
                _logger.info("connecting to Qpid :" + brokerUrl);
                connection = factory.createConnection();

                // register exception listener
                connection.setExceptionListener(this);

                session = ((AMQConnection) connection).createSession(transacted, ackMode, prefetch);

                _logger.info("starting connection");
                connection.start();

                connected = true;
            }
            catch (URLSyntaxException e)
            {
                throw new JMSAMQException("URL syntax error in [" + brokerUrl + "]: " + e.getMessage(), e);
            }
        }
    }

    public void disconnect() throws JMSException
    {
        if (connected)
        {
            session.commit();
            session.close();
            connection.close();
            connected = false;
            _logger.info("disconnected");
        }
    }

    public void disconnectWithoutCommit() throws JMSException
    {
        if (connected)
        {
            session.close();
            connection.close();
            connected = false;
            _logger.info("disconnected without commit");
        }
    }

    public String getBrokerList()
    {
        return brokerlist;
    }

    public void setBrokerList(String brokerlist)
    {
        this.brokerlist = brokerlist;
    }

    public String getVirtualHost()
    {
        return virtualHost;
    }

    public void setVirtualHost(String virtualHost)
    {
        this.virtualHost = virtualHost;
    }

    public void setPrefetch(int prefetch)
    {
        this.prefetch = prefetch;
    }

    /** override as necessary */
    public void onException(JMSException exception)
    {
        _logger.info("ExceptionListener event: error " + exception.getErrorCode() + ", message: " + exception.getMessage());
    }

    public boolean isConnected()
    {
        return connected;
    }

    public Session getSession()
    {
        return session;
    }

    /**
     * Put a String as a text messages, repeat n times. A null payload will result in a null message.
     *
     * @param queueName The queue name to put to
     * @param payload   the content of the payload
     * @param copies    the number of messages to put
     *
     * @throws javax.jms.JMSException any exception that occurs
     */
    public void put(String queueName, String payload, int copies, int deliveryMode) throws JMSException
    {
        if (!connected)
        {
            connect();
        }

        _logger.info("putting to queue " + queueName);
        Queue queue = session.createQueue(queueName);

        final MessageProducer sender = session.createProducer(queue);

        sender.setDeliveryMode(deliveryMode);

        for (int i = 0; i < copies; i++)
        {
            Message m = session.createTextMessage(payload + i);
            m.setIntProperty("index", i + 1);
            sender.send(m);
        }

        session.commit();
        sender.close();
        _logger.info("put " + copies + " copies");
    }

    /**
     * GET the top message on a queue. Consumes the message. Accepts timeout value.
     *
     * @param queueName   The quename to get from
     * @param readTimeout The timeout to use
     *
     * @return the content of the text message if any
     *
     * @throws javax.jms.JMSException any exception that occured
     */
    public Message getNextMessage(String queueName, long readTimeout) throws JMSException
    {
        if (!connected)
        {
            connect();
        }

        Queue queue = session.createQueue(queueName);

        final MessageConsumer consumer = session.createConsumer(queue);

        Message message = consumer.receive(readTimeout);
        session.commit();
        consumer.close();

        Message result;

        // all messages we consume should be TextMessages
        if (message instanceof TextMessage)
        {
            result = ((TextMessage) message);
        }
        else if (null == message)
        {
            result = null;
        }
        else
        {
            _logger.info("warning: received non-text message");
            result = message;
        }

        return result;
    }

    /**
     * GET the top message on a queue. Consumes the message.
     *
     * @param queueName The Queuename to get from
     *
     * @return The string content of the text message, if any received
     *
     * @throws javax.jms.JMSException any exception that occurs
     */
    public Message getNextMessage(String queueName) throws JMSException
    {
        return getNextMessage(queueName, 0);
    }

    /**
     * Completely clears a queue. For readTimeout behaviour see Javadocs for javax.jms.MessageConsumer.
     *
     * @param queueName   The Queue name to consume from
     * @param readTimeout The timeout for each consume
     *
     * @throws javax.jms.JMSException Any exception that occurs during the consume
     * @throws InterruptedException   If the consume thread was interrupted during a consume.
     */
    public void consume(String queueName, int readTimeout) throws JMSException, InterruptedException
    {
        if (!connected)
        {
            connect();
        }

        _logger.info("consuming queue " + queueName);
        Queue queue = session.createQueue(queueName);

        final MessageConsumer consumer = session.createConsumer(queue);
        int messagesReceived = 0;

        _logger.info("consuming...");
        while ((consumer.receive(readTimeout)) != null)
        {
            messagesReceived++;
        }

        session.commit();
        consumer.close();
        _logger.info("consumed: " + messagesReceived);
    }
}
