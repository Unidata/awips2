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

import org.apache.qpid.AMQException;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.framing.AMQShortString;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.Session;
import javax.naming.NamingException;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The Queue test suite validates that the follow log messages as specified in
 * the Functional Specification.
 *
 * This suite of tests validate that the Queue messages occur correctly and
 * according to the following format:
 *
 * QUE-1001 : Create : [AutoDelete] [Durable|Transient] [Priority:<levels>] [Owner:<name>]
 */
public class DurableQueueLoggingTest extends AbstractTestLogging
{

    protected String DURABLE = "Durable";
    protected String TRANSIENT = "Transient";
    protected boolean _durable;

    protected Connection _connection;
    protected Session _session;
    private static final String QUEUE_PREFIX = "QUE-";

    public void setUp() throws Exception
    {
        super.setUp();
        //Ensure we only have logs from our test
        _monitor.reset();

        _connection = getConnection();
        _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        _durable = true;
    }

    /**
     * Description:
     * When a simple transient queue is created then a QUE-1001 create message
     * is expected to be logged.
     * Input:
     * 1. Running broker
     * 2. Persistent Queue is created from a client
     * Output:
     *
     * <date> QUE-1001 : Create : Owner: '<name>' Durable
     *
     * Validation Steps:
     * 3. The QUE ID is correct
     * 4. The Durable tag is present in the message
     * 5. The Owner is as expected
     *
     * @throws javax.jms.JMSException
     * @throws javax.naming.NamingException
     * @throws java.io.IOException
     */
    public void testQueueCreateDurableExclusive() throws NamingException, JMSException, IOException
    {
        String queueName= getTestQueueName();
        // To force a queue Creation Event we need to create a consumer.
        Queue queue = (Queue) _session.createQueue("direct://amq.direct/" + queueName + "/" + queueName + "?durable='" + _durable + "'&exclusive='true'");

        _session.createConsumer(queue);

        // Validation
        List<String> results = _monitor.findMatches(QUEUE_PREFIX);

        // Only 1 Queue message should hav been logged
        assertEquals("Result set size not as expected", 1, results.size());

        String log = getLog(results.get(0));

        // Message Should be a QUE-1001
        validateMessageID("QUE-1001", log);

        // Queue is Durable
        assertEquals(DURABLE + " keyword not correct in log entry",
                     _durable, fromMessage(log).contains(DURABLE));

        assertEquals(TRANSIENT + " keyword not correct in log entry.",
                     !_durable, fromMessage(log).contains(TRANSIENT));

        assertTrue("Queue does not have correct owner value:" + fromMessage(log),
                   fromMessage(log).contains("Owner: " + _connection.getClientID()));
    }

    /**
     * Description:
     * When a simple transient queue is created then a QUE-1001 create message
     * is expected to be logged.
     * Input:
     * 1. Running broker
     * 2. Persistent Queue is created from a client
     * Output:
     *
     * <date> QUE-1001 : Create : Owner: '<name>' Durable
     *
     * Validation Steps:
     * 3. The QUE ID is correct
     * 4. The Durable tag is present in the message
     * 5. The Owner is as expected
     *
     * @throws javax.jms.JMSException
     * @throws javax.naming.NamingException
     * @throws java.io.IOException
     */
    public void testQueueCreateDurable() throws NamingException, JMSException, IOException
    {
        String queueName = getTestQueueName();

        // To force a queue Creation Event we need to create a consumer.
        Queue queue = (Queue) _session.createQueue("direct://amq.direct/" + queueName + "/" + queueName + "?durable='" + _durable + "'");

        _session.createConsumer(queue);

        // Validation
        List<String> results = _monitor.findMatches(QUEUE_PREFIX);

        // Only 1 Queue message should hav been logged
        assertEquals("Result set size not as expected", 1, results.size());

        String log = getLog(results.get(0));

        // Message Should be a QUE-1001
        validateMessageID("QUE-1001", log);

        // Queue is Durable
        assertEquals(DURABLE + " keyword not correct in log entry",
                     _durable, fromMessage(log).contains(DURABLE));

        assertEquals(TRANSIENT + " keyword not correct in log entry.",
                     !_durable, fromMessage(log).contains(TRANSIENT));

        assertFalse("Queue should not contain Owner tag:" + fromMessage(log),
                   fromMessage(log).contains("Owner"));
    }

    /**
     * Description:
     * When a simple transient queue is created then a QUE-1001 create message
     * is expected to be logged.
     * Input:
     * 1. Running broker
     * 2. AutoDelete Persistent Queue is created from a client
     * Output:
     *
     * <date> QUE-1001 : Create : Owner: '<name>' AutoDelete Durable
     *
     * Validation Steps:
     * 3. The QUE ID is correct
     * 4. The Durable tag is present in the message
     * 5. The Owner is as expected
     * 6. The AutoDelete tag is present in the message
     *
     * @throws javax.jms.JMSException
     * @throws javax.naming.NamingException
     * @throws java.io.IOException
     */
    public void testQueueCreatePersistentAutoDelete() throws NamingException, JMSException, IOException
    {
        String queueName = getTestQueueName();
        // To force a queue Creation Event we need to create a consumer.
        Queue queue = (Queue) _session.createQueue("direct://amq.direct/"+queueName+"/"+queueName+"?durable='"+_durable+"'&autodelete='true'");

        _session.createConsumer(queue);

        // Validation
        List<String> results = _monitor.findMatches(QUEUE_PREFIX);

        // Only 1 Queue message should hav been logged
        assertEquals("Result set size not as expected", 1, results.size());

        String log = getLog(results.get(0));

        // Message Should be a QUE-1001
        validateMessageID("QUE-1001", log);

        // Queue is Durable
        assertEquals(DURABLE + " keyword not correct in log entry",
                     _durable, fromMessage(log).contains(DURABLE));

        assertEquals(TRANSIENT + " keyword not correct in log entry.",
                     !_durable, fromMessage(log).contains(TRANSIENT));

        // Queue is AutoDelete
        assertTrue("Queue does not have the AutoDelete keyword in log:" + fromMessage(log),
                   fromMessage(log).contains("AutoDelete"));

        assertFalse("Queue should not contain Owner tag:" + fromMessage(log),
                   fromMessage(log).contains("Owner"));
    }

    /**
     * Description:
     * When a simple transient queue is created then a QUE-1001 create message
     * is expected to be logged.
     * Input:
     * 1. Running broker
     * 2. Persistent Queue is created from a client
     * Output:
     *
     * <date> QUE-1001 : Create : Owner: '<name>' Durable Priority:<levels>
     *
     * Validation Steps:
     * 3. The QUE ID is correct
     * 4. The Durable tag is present in the message
     * 5. The Owner is as expected
     * 6. The Priority level is correctly set
     *
     * @throws javax.jms.JMSException
     * @throws javax.naming.NamingException
     * @throws java.io.IOException
     */
    public void testCreateQueuePersistentPriority() throws NamingException, JMSException, IOException, AMQException
    {
        // To Create a Priority queue we need to use AMQSession specific code
        int PRIORITIES = 6;
        final Map<String, Object> arguments = new HashMap<String, Object>();
        arguments.put("x-qpid-priorities", PRIORITIES);
        // Need to create a queue that does not exist so use test name
        final String queueName = getTestQueueName();
        ((AMQSession) _session).createQueue(new AMQShortString(queueName), false, _durable, false, arguments);

        Queue queue = (Queue) _session.createQueue("direct://amq.direct/"+queueName+"/"+queueName+"?durable='"+_durable+"'&autodelete='false'");


        //Need to create a Consumer to ensure that the log has had time to write
        // as the above Create is Asynchronous
        _session.createConsumer(queue);

        // Validation
        List<String> results = _monitor.findMatches(QUEUE_PREFIX);

        // Only 1 Queue message should hav been logged
        assertEquals("Result set size not as expected", 1, results.size());

        String log = getLog(results.get(0));

        // Message Should be a QUE-1001
        validateMessageID("QUE-1001", log);

        // Queue is Durable
        assertEquals(DURABLE + " keyword not correct in log entry",
                     _durable, fromMessage(log).contains(DURABLE));

        assertEquals(TRANSIENT + " keyword not correct in log entry.",
                     !_durable, fromMessage(log).contains(TRANSIENT));

        // Queue is AutoDelete
        assertTrue("Queue does not have the right Priority value keyword in log:" + fromMessage(log),
                   fromMessage(log).contains("Priority: " + PRIORITIES));

        assertFalse("Queue should not contain Owner tag:" + fromMessage(log),
                   fromMessage(log).contains("Owner"));
    }

    /**
     * Description:
     * When a simple transient queue is created then a QUE-1001 create message
     * is expected to be logged.
     * Input:
     * 1. Running broker
     * 2. AutoDelete Persistent Queue is created from a client
     * Output:
     *
     * <date> QUE-1001 : Create : Owner: '<name>' Durable Priority:<levels>
     *
     * Validation Steps:
     * 3. The QUE ID is correct
     * 4. The Durable tag is present in the message
     * 5. The Owner is as expected
     * 6. The AutoDelete tag is present in the message
     * 7. The Priority level is correctly set
     *
     * @throws javax.jms.JMSException
     * @throws javax.naming.NamingException
     * @throws java.io.IOException
     */
    public void testCreateQueuePersistentAutoDeletePriority() throws NamingException, JMSException, IOException, AMQException
    {
        // To Create a Priority queue we need to use AMQSession specific code
        int PRIORITIES = 6;
        final Map<String, Object> arguments = new HashMap<String, Object>();
        arguments.put("x-qpid-priorities", PRIORITIES);
        // Need to create a queue that does not exist so use test name
        final String queueName = getTestQueueName() + "-autoDeletePriority";
        ((AMQSession) _session).createQueue(new AMQShortString(queueName), true, _durable, false, arguments);

        Queue queue = (Queue) _session.createQueue("direct://amq.direct/"+queueName+"/"+queueName+"?durable='"+_durable+"'&autodelete='true'");


        //Need to create a Consumer to ensure that the log has had time to write
        // as the above Create is Asynchronous
        _session.createConsumer(queue);

        // Validation
        List<String> results = _monitor.findMatches(QUEUE_PREFIX);

        // Only 1 Queue message should hav been logged
        assertEquals("Result set size not as expected", 1, results.size());

        String log = getLog(results.get(0));

        // Message Should be a QUE-1001
        validateMessageID("QUE-1001", log);

        // Queue is Durable
        assertEquals(DURABLE + " keyword not correct in log entry",
                     _durable, fromMessage(log).contains(DURABLE));

        assertEquals(TRANSIENT + " keyword not correct in log entry.",
                     !_durable, fromMessage(log).contains(TRANSIENT));

        // Queue is AutoDelete
        assertTrue("Queue does not have the right Priority value keyword in log:" + fromMessage(log),
                   fromMessage(log).contains("Priority: " + PRIORITIES));

        // Queue is AutoDelete
        assertTrue("Queue does not have the AutoDelete keyword in log:" + fromMessage(log),
                   fromMessage(log).contains("AutoDelete"));

        assertFalse("Queue should not contain Owner tag:" + fromMessage(log),
                   fromMessage(log).contains("Owner"));
    }

}
