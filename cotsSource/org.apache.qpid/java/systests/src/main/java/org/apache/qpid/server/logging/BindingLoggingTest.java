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

import org.apache.qpid.server.logging.subjects.AbstractTestLogSubject;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.Topic;
import java.io.IOException;
import java.util.List;

/**
 * Binding
 *
 * The Binding test suite validates that the follow log messages as specified in the Functional Specification.
 *
 * This suite of tests validate that the Binding messages occur correctly and according to the following format:
 *
 * BND-1001 : Create [: Arguments : <key=value>]
 * BND-1002 : Deleted
 */
public class BindingLoggingTest extends AbstractTestLogging
{

    static final String BND_PREFIX = "BND-";

    Connection _connection;
    Session _session;
    Queue _queue;
    Topic _topic;

    @Override
    public void setUp() throws Exception
    {
        super.setUp();
        //Ignore broker startup messages
        _monitor.reset();

        _connection = getConnection();

        _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        _queue = _session.createQueue(getName());
        _topic = (Topic) getInitialContext().lookup(TOPIC);
    }

    private void validateLogMessage(String log, String messageID, String exchange, String message)
    {
        validateMessageID(messageID, log);

        String subject = fromSubject(log);

        assertEquals("Queue not correct.", getName(),
                     AbstractTestLogSubject.getSlice("qu", subject));
        assertEquals("Routing Key not correct.", getName(),
                     AbstractTestLogSubject.getSlice("rk", subject));
        assertEquals("Virtualhost not correct.", "/test",
                     AbstractTestLogSubject.getSlice("vh", subject));
        assertEquals("Exchange not correct.", exchange,
                     AbstractTestLogSubject.getSlice("ex", subject));

        assertEquals("Log Message not as expected", message, getMessageString(fromMessage(log)));
    }

    /**
     * testBindingCreate
     *
     * Description:
     * The binding of a Queue and an Exchange is done via a Binding. When this Binding is created a BND-1001 Create message will be logged.
     * Input:
     *
     * 1. Running Broker
     * 2. New Client requests that a Queue is bound to a new exchange.
     * Output:
     *
     * <date> BND-1001 : Create
     *
     * Validation Steps:
     * 3. The BND ID is correct
     * 4. This will be the first message for the given binding
     */
    public void testBindingCreate() throws JMSException, IOException
    {
        _session.createConsumer(_queue).close();

        List<String> results = _monitor.findMatches(BND_PREFIX);

        // We will have two binds as we bind all queues to the default exchange
        assertEquals("Result set larger than expected.", 2, results.size());

        String exchange = "direct/<<default>>";
        String messageID = "BND-1001";
        String message = "Create";

        validateLogMessage(getLog(results.get(0)), messageID, exchange, message);

        exchange = "direct/amq.direct";
        validateLogMessage(getLog(results.get(1)), messageID, exchange, message);
    }

    /**
     * Description:
     * A Binding can be made with a set of arguments. When this occurs we logged the key,value pairs as part of the Binding log message. When the subscriber with a JMS Selector consumes from an exclusive queue such as a topic. The binding is made with the JMS Selector as an argument.
     * Input:
     *
     * 1. Running Broker
     * 2. Java Client consumes from a topic with a JMS selector.
     * Output:
     *
     * <date> BND-1001 : Create : Arguments : <key=value>
     *
     * Validation Steps:
     * 3. The BND ID is correct
     * 4. The JMS Selector argument is present in the message
     * 5. This will be the first message for the given binding
     */
    public void testBindingCreateWithArguments() throws JMSException, IOException
    {
        final String SELECTOR = "Selector='True'";

        _session.createDurableSubscriber(_topic, getName(), SELECTOR, false).close();

        List<String> results = _monitor.findMatches(BND_PREFIX);

        // We will have two binds as we bind all queues to the default exchange
        assertEquals("Result set larger than expected.", 2, results.size());

        String log = getLog(results.get(0));

        //Verify the first entry is the default binding
        validateMessageID("BND-1001", log);

        String subject = fromSubject(log);

        assertEquals("Queue not correct.", "clientid:" + getName(),
                     AbstractTestLogSubject.getSlice("qu", subject));
        // NOTE default binding is the queue name
        assertEquals("Routing Key not correct.", "clientid:" + getName(),
                     AbstractTestLogSubject.getSlice("rk", subject));
        assertEquals("Virtualhost not correct.", "/test",
                     AbstractTestLogSubject.getSlice("vh", subject));
        assertEquals("Exchange not correct.", "direct/<<default>>",
                     AbstractTestLogSubject.getSlice("ex", fromSubject(log)));

        String message = getMessageString(log);

        //Default binding will be without the selector
        assertTrue("JMSSelector identified in binding:"+message, !message.contains("jms-selector"));

        // Perform full testing on the second non default binding
        log = getLog(results.get(1));
        validateMessageID("BND-1001", log);

        subject = fromSubject(log);

        assertEquals("Queue not correct.", "clientid:" + getName(),
                     AbstractTestLogSubject.getSlice("qu", subject));
        assertEquals("Routing Key not correct.", "topic",
                     AbstractTestLogSubject.getSlice("rk", subject));
        assertEquals("Virtualhost not correct.", "/test",
                     AbstractTestLogSubject.getSlice("vh", subject));
        assertEquals("Exchange not correct.", "topic/amq.topic",
                     AbstractTestLogSubject.getSlice("ex", subject));

        message = getMessageString(log);

        assertTrue("JMSSelector not identified in binding:"+message, message.contains("jms-selector"));
        assertTrue("Selector not part of binding.:"+message, message.contains(SELECTOR));

    }

    /**
     * Description:
     * Bindings can be deleted so that a queue can be rebound with a different set of values.
     * Input:
     *
     * 1. Running Broker
     * 2. AMQP UnBind Request is made
     * Output:
     *
     * <date> BND-1002 : Deleted
     *
     * Validation Steps:
     * 3. The BND ID is correct
     * 4. There must have been a BND-1001 Create message first.
     * 5. This will be the last message for the given binding
     */
    public void testBindingDelete() throws JMSException, IOException
    {
        //Closing a consumer on a temporary queue will cause it to autodelete
        // and so unbind.
        _session.createConsumer(_session.createTemporaryQueue()).close();

        List<String> results = _monitor.findMatches(BND_PREFIX);

        // We will have two binds as we bind all queues to the default exchange
        assertEquals("Result not as expected." + results, 4, results.size());


        String messageID = "BND-1001";
        String message = "Create";

        String log = getLog(results.get(0));
        validateMessageID(messageID, log);
        assertEquals("Log Message not as expected", message, getMessageString(fromMessage(log)));

        log = getLog(results.get(1));
        validateMessageID(messageID, log);
        assertEquals("Log Message not as expected", message, getMessageString(fromMessage(log)));


        String DEFAULT = "direct/<<default>>";
        String DIRECT = "direct/amq.direct";

        messageID = "BND-1002";
        message = "Deleted";

        log = getLog(results.get(2));
        validateMessageID(messageID, log);

        String subject = fromSubject(log);

        assertTrue("Routing Key does not start with TempQueue:"+AbstractTestLogSubject.getSlice("rk", subject),
                     AbstractTestLogSubject.getSlice("rk", subject).startsWith("TempQueue"));
        assertEquals("Virtualhost not correct.", "/test",
                     AbstractTestLogSubject.getSlice("vh", subject));

        boolean defaultFirst = DEFAULT.equals(AbstractTestLogSubject.getSlice("ex", subject));
        boolean directFirst = DIRECT.equals(AbstractTestLogSubject.getSlice("ex", subject));

        assertEquals("Log Message not as expected", message, getMessageString(fromMessage(log)));

        log = getLog(results.get(3));

        validateMessageID(messageID, log);

        subject = fromSubject(log);

        assertTrue("Routing Key does not start with TempQueue:"+AbstractTestLogSubject.getSlice("rk", subject),
                     AbstractTestLogSubject.getSlice("rk", subject).startsWith("TempQueue"));
        assertEquals("Virtualhost not correct.", "/test",
                     AbstractTestLogSubject.getSlice("vh", subject));

        if (!defaultFirst)
        {
            assertEquals(DEFAULT, AbstractTestLogSubject.getSlice("ex", subject));
            assertTrue("First Exchange Log was not a direct exchange delete",directFirst);
        }
        else
        {
            assertEquals(DIRECT, AbstractTestLogSubject.getSlice("ex", subject));
            assertTrue("First Exchange Log was not a default exchange delete",defaultFirst);
        }

        assertEquals("Log Message not as expected", message, getMessageString(fromMessage(log)));

    }
}
