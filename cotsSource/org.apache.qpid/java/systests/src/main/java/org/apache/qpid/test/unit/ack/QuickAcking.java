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
package org.apache.qpid.test.unit.ack;

import edu.emory.mathcs.backport.java.util.concurrent.CountDownLatch;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.jms.ConnectionListener;
import org.apache.qpid.test.utils.QpidTestCase;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;

/**
 * This is a quick manual test to validate acking after failover with a
 * transacted session.
 *
 * Start an external broker then run this test. Std Err will print.
 * Sent Message: 1
 * Received Message: 1
 *
 * You can then restart the external broker, which will cause failover, which
 * will be complete when the following appears.
 *
 * Failover Complete
 *
 * A second message send/receive cycle is then done to validate that the
 * connection/session are still working.
 *
 */
public class QuickAcking extends QpidTestCase implements ConnectionListener
{
    protected AMQConnection _connection;
    protected Queue _queue;
    protected Session _session;
    protected MessageConsumer _consumer;
    private CountDownLatch _failedOver;
    private static final String INDEX = "INDEX";
    private int _count = 0;

    public void setUp()
    {
        // Prevent broker startup. Broker must be run manually.
    }

    public void test() throws Exception
    {
        _failedOver = new CountDownLatch(1);

        _connection = new AMQConnection("amqp://guest:guest@client/test?brokerlist='localhost?retries='20'&connectdelay='2000''");

        _session = _connection.createSession(true, Session.SESSION_TRANSACTED);
        _queue = _session.createQueue("QAtest");
        _consumer = _session.createConsumer(_queue);
        _connection.setConnectionListener(this);
        _connection.start();

        sendAndReceive();

        _failedOver.await();

        sendAndReceive();

    }

    private void sendAndReceive()
            throws Exception
    {
        sendMessage();

        Message message = _consumer.receive();

        if (message.getIntProperty(INDEX) != _count)
        {
            throw new Exception("Incorrect message recieved:" + _count);
        }

        if (_session.getTransacted())
        {
            _session.commit();
        }
        System.err.println("Recevied Message:" + _count);
    }

    private void sendMessage() throws JMSException
    {
        MessageProducer producer = _session.createProducer(_queue);
        Message message = _session.createMessage();
        _count++;
        message.setIntProperty(INDEX, _count);

        producer.send(message);
        if (_session.getTransacted())
        {
            _session.commit();
        }
        producer.close();

        System.err.println("Sent Message:" + _count);
    }

    public void bytesSent(long count)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void bytesReceived(long count)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean preFailover(boolean redirect)
    {
        return true;
    }

    public boolean preResubscribe()
    {
        return true;
    }

    public void failoverComplete()
    {
        System.err.println("Failover Complete");
        _failedOver.countDown();
    }
}
