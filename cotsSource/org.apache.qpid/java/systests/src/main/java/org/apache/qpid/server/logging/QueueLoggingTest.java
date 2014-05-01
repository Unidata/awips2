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
package org.apache.qpid.server.logging;

import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.server.logging.subjects.AbstractTestLogSubject;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.AMQException;

import javax.jms.Connection;
import javax.jms.Session;
import javax.jms.Queue;
import javax.jms.JMSException;
import javax.naming.NamingException;
import java.util.List;
import java.io.IOException;

/**
 * The Queue test suite validates that the follow log messages as specified in
 * the Functional Specification.
 *
 * This suite of tests validate that the Queue messages occur correctly and
 * according to the following format:
 *
 * QUE-1002 : Deleted
 */
public class QueueLoggingTest extends AbstractTestLogging
{
    protected Connection _connection;
    protected Session _session;
    private static final String QUEUE_PREFIX = "QUE-";

    public void setUp() throws Exception
    {
        super.setUp();
        //Remove broker startup logging messages
        _monitor.reset();
        
        _connection = getConnection();
        _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
    }

    /**
     *  Description:
     *  An explict QueueDelete request must result in a QUE-1002 Deleted message
     *  being logged. This can be done via an explict AMQP QueueDelete method.
     * Input:
     *
     *  1. Running Broker
     *  2. Queue created on the broker with no subscribers
     *  3. Client requests the queue be deleted via a QueueDelete
     *  Output:
     *
     *  <date> QUE-1002 : Deleted
     *
     *  Validation Steps:
     *
     *  4. The QUE ID is correct
     *
     * @throws java.io.IOException
     * @throws javax.jms.JMSException
     * @throws javax.naming.NamingException
     */
    public void testQueueDelete() throws NamingException, JMSException, IOException, FailoverException, AMQException
    {
        // To force a queue Creation Event we need to create a consumer.
        Queue queue = _session.createQueue(getTestQueueName());

        _session.createConsumer(queue);

        // Delete Queue
        ((AMQSession)_session).sendQueueDelete(new AMQShortString(queue.getQueueName()));

        //Perform a synchronous action to ensure that the above log will be on disk
        _session.close();

        // Validation
        List<String> results = _monitor.findMatches(QUEUE_PREFIX);

        // Only 1 Queue message should hav been logged
        assertEquals("Result set size not as expected", 2, results.size());

        String log = getLog(results.get(0));

        // Message Should be a QUE-1001
        validateMessageID("QUE-1001", log);        

        String createdQueueName = AbstractTestLogSubject.getSlice("qu",  fromSubject(log));

        log = getLog(results.get(1));
        // Message Should be a QUE-1002
        validateMessageID("QUE-1002", log);

        assertEquals("Log Message is incorrect ", "Deleted", getMessageString(fromMessage(log)));

        assertEquals("Queue Delete not for created queue:", createdQueueName,
                     AbstractTestLogSubject.getSlice("qu", fromSubject(log)));
    }


    /**
        *  Description:
        *  An explict QueueDelete request must result in a QUE-1002 Deleted message
        *  being logged. This can be done via an explict AMQP QueueDelete method.
        * Input:
        *
        *  1. Running Broker
        *  2. Queue created on the broker with no subscribers
        *  3. Client creates a temporary queue then disconnects
        *  Output:
        *
        *  <date> QUE-1002 : Deleted
        *
        *  Validation Steps:
        *
        *  4. The QUE ID is correct
        *
        * @throws java.io.IOException
        * @throws javax.jms.JMSException
        * @throws javax.naming.NamingException
        */
       public void testQueueAutoDelete() throws NamingException, JMSException, IOException
       {
           // Create a temporary queue so that when we consume from it and
           // then close the consumer it will be autoDeleted.
           _session.createConsumer(_session.createTemporaryQueue()).close();

           // Validation
           List<String> results = _monitor.findMatches(QUEUE_PREFIX);

           // Only 1 Queue message should hav been logged
           assertEquals("Result set size not as expected", 2, results.size());

           String log = getLog(results.get(0));

           // Message Should be a QUE-1001
           validateMessageID("QUE-1001", log);

           String createdQueueName = AbstractTestLogSubject.getSlice("qu",  fromSubject(log));

           log = getLog(results.get(1));
           // Message Should be a QUE-1002
           validateMessageID("QUE-1002", log);

           assertEquals("Log Message is incorrect ", "Deleted", getMessageString(fromMessage(log)));

           assertEquals("Queue Delete not for created queue:", createdQueueName,
                        AbstractTestLogSubject.getSlice("qu", fromSubject(log)));

       }

}