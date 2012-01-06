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
package org.apache.qpid.test.unit.client.forwardall;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.test.utils.QpidTestCase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;

/**
 * Declare a private temporary response queue,
 * send a message to amq.direct with a well known routing key with the
 * private response queue as the reply-to destination
 * consume responses.
 */
public class Client implements MessageListener
{
    private static final Logger _logger = LoggerFactory.getLogger(Client.class);

    private final AMQConnection _connection;
    private final AMQSession _session;
    private final int _expected;
    private int _count;
    private static QpidTestCase _qct;

    Client(String broker, int expected) throws Exception
    {
        this(connect(broker), expected);
    }

    public static void setQTC(QpidTestCase qtc)
    {
        _qct = qtc;
    }
    Client(AMQConnection connection, int expected) throws Exception
    {
        _connection = connection;
        _expected = expected;
        _session = (AMQSession) _connection.createSession(true, AMQSession.NO_ACKNOWLEDGE);
        AMQQueue response =
            new AMQQueue(_connection.getDefaultQueueExchangeName(), new AMQShortString("ResponseQueue"), true);
        _session.createConsumer(response).setMessageListener(this);
        _connection.start();
      //  AMQQueue service = new SpecialQueue(_connection, "ServiceQueue");
        AMQQueue service  = (AMQQueue)  _session.createQueue("ServiceQueue") ;
        Message request = _session.createTextMessage("Request!");
        request.setJMSReplyTo(response);
        MessageProducer prod = _session.createProducer(service);
        prod.send(request);
        _session.commit();
    }

    void shutdownWhenComplete() throws Exception
    {
        waitUntilComplete();
        _connection.close();
    }

    public synchronized void onMessage(Message response)
    {

        _logger.info("Received " + (++_count) + " of " + _expected + " responses.");
        if (_count == _expected)
        {

            notifyAll();
        }
        try
        {
            _session.commit();
        }
        catch (JMSException e)
        {
            
        }

    }

    synchronized void waitUntilComplete() throws Exception
    {

        if (_count < _expected)
        {
            wait(60000);
        }

        if (_count < _expected)
        {
            throw new Exception("Didn't receive all messages... got " + _count + " expected " + _expected);
        }
    }

    static AMQConnection connect(String broker) throws Exception
    {
        //return new AMQConnection(broker, "guest", "guest", "Client" + System.currentTimeMillis(), "test");
         return (AMQConnection) _qct.getConnection("guest", "guest") ;
    }

    public static void main(String[] argv) throws Exception
    {
        final String connectionString;
        final int expected;
        if (argv.length == 0)
        {
            connectionString = "localhost:5672";
            expected = 100;
        }
        else
        {
            connectionString = argv[0];
            expected = Integer.parseInt(argv[1]);
        }

        new Client(connect(connectionString), expected).shutdownWhenComplete();
    }
}
