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

import junit.framework.AssertionFailedError;
import org.apache.qpid.client.AMQConnection;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.Topic;
import javax.jms.Message;
import java.io.IOException;
import java.util.List;

/**
 * Subscription
 *
 * The Subscription test suite validates that the follow log messages as specified in the Functional Specification.
 *
 * This suite of tests validate that the Subscription messages occur correctly and according to the following format:
 *
 * SUB-1001 : Create : [Durable] [Arguments : <key=value>]
 * SUB-1002 : Close
 * SUB-1003 : State : <state>
 */
public class SubscriptionLoggingTest extends AbstractTestLogging
{
    static final String SUB_PREFIX = "SUB-";

    Connection _connection;
    Session _session;
    Queue _queue;
    Topic _topic;

    @Override
    public void setUp() throws Exception
    {
        super.setUp();
        //Remove broker startup logging messages
        _monitor.reset();

        _connection = getConnection();

        _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        _queue = (Queue) getInitialContext().lookup(QUEUE);
        _topic = (Topic) getInitialContext().lookup(TOPIC);
    }

    /**
     * Description:
     * When a Subscription is created it will be logged. This test validates that Subscribing to a transient queue is correctly logged.
     * Input:
     *
     * 1. Running Broker
     * 2. Create a new Subscription to a transient queue/topic.
     * Output:          6
     *
     * <date> SUB-1001 : Create
     *
     * Validation Steps:
     * 3. The SUB ID is correct
     *
     * @throws java.io.IOException    - if there is a problem getting the matches
     * @throws javax.jms.JMSException - if there is a problem creating the consumer
     */
    public void testSubscriptionCreate() throws JMSException, IOException
    {
        _session.createConsumer(_queue);

        //Validate

        List<String> results = _monitor.findMatches(SUB_PREFIX);

        assertEquals("Result set larger than expected.", 1, results.size());

        String log = getLog(results.get(0));

        validateMessageID("SUB-1001", log);

        assertEquals("Log Message not as expected", "Create", getMessageString(fromMessage(log)));
    }

    /**
     * Description:
     * The creation of a Durable Subscription, such as a JMS DurableTopicSubscriber will result in an extra Durable tag being included in the Create log message
     * Input:
     *
     * 1. Running Broker
     * 2. Creation of a JMS DurableTopicSubiber
     * Output:
     *
     * <date> SUB-1001 : Create : Durable
     *
     * Validation Steps:
     * 3. The SUB ID is correct
     * 4. The Durable tag is present in the message
     * NOTE: A Subscription is not Durable, the queue it consumes from is.
     *
     * @throws java.io.IOException    - if there is a problem getting the matches
     * @throws javax.jms.JMSException - if there is a problem creating the consumer
     */
    public void testSubscriptionCreateDurable() throws JMSException, IOException
    {
        _session.createDurableSubscriber(_topic, getName());

        //Validate

        List<String> results = _monitor.findMatches(SUB_PREFIX);

        assertEquals("Result set larger than expected.", 1, results.size());

        String log = getLog(results.get(0));

        validateMessageID("SUB-1001", log);

        String message = getMessageString(fromMessage(log));
        assertTrue("Durable not on log message:" + message, message.contains("Durable"));
    }

    /**
     * Description:
     * The creation of a QueueBrowser will provides a number arguments and so should form part of the SUB-1001 Create message.
     * Input:
     *
     * 1. Running Broker
     * 2. Java Client creates a QueueBroweser
     * Output:
     *
     * <date> SUB-1001 : Create : Arguments : <key=value>
     *
     * Validation Steps:
     * 3. The SUB ID is correct
     * 4. The Arguments are present in the message
     * 5. Arguments keys include AutoClose and Browser.
     *
     * @throws java.io.IOException    - if there is a problem getting the matches
     * @throws javax.jms.JMSException - if there is a problem creating the consumer
     */
    public void testSubscriptionCreateQueueBrowser() throws JMSException, IOException
    {
        _session.createBrowser(_queue);

        //Validate
        List<String> results = _monitor.findMatches(SUB_PREFIX);

        assertEquals("Result set larger than expected.", 2, results.size());

        String log = getLog(results.get(0));

        validateMessageID("SUB-1001", log);

        String message = getMessageString(fromMessage(log));
        assertTrue("Browser not on log message:" + message, message.contains("Browser"));
        assertTrue("AutoClose not on log message:" + message, message.contains("AutoClose"));

        // Beacause it is an auto close and we have no messages on the queue we
        // will get a close message
        log = getLog(results.get(1));
        validateMessageID("SUB-1002", log);

    }

    /**
     * Description:
     * The creation of a Subscriber with a JMS Selector will result in the Argument field being populated. These argument key/value pairs are then shown in the log message.
     * Input:
     *
     * 1. Running Broker
     * 2. Subscriber created with a JMS Selector.
     * Output:
     *
     * <date> SUB-1001 : Create : Arguments : <key=value>
     *
     * Validation Steps:
     * 3. The SUB ID is correct
     * 4. Argument tag is present in the message
     *
     * @throws java.io.IOException    - if there is a problem getting the matches
     * @throws javax.jms.JMSException - if there is a problem creating the consumer
     */
    public void testSubscriptionCreateWithArguments() throws JMSException, IOException
    {
        final String SELECTOR = "Selector='True'";
        _session.createConsumer(_queue, SELECTOR);

        //Validate

        List<String> results = _monitor.findMatches(SUB_PREFIX);

        assertEquals("Result set larger than expected.", 1, results.size());

        String log = getLog(results.get(0));

        validateMessageID("SUB-1001", log);

        String message = getMessageString(fromMessage(log));
        assertTrue("Selector not on log message:" + message, message.contains(SELECTOR));
    }

    /**
     * Description:
     * The final combination of SUB-1001 Create messages involves the creation of a Durable Subscription that also contains a set of Arguments, such as those provided via a JMS Selector.
     * Input:
     *
     * 1. Running Broker
     * 2. Java Client creates a Durable Subscription with Selector
     * Output:
     *
     * <date> SUB-1001 : Create : Durable Arguments : <key=value>
     *
     * Validation Steps:
     * 3. The SUB ID is correct
     * 4. The tag Durable is present in the message
     * 5. The Arguments are present in the message
     *
     * @throws java.io.IOException    - if there is a problem getting the matches
     * @throws javax.jms.JMSException - if there is a problem creating the consumer
     */
    public void testSubscriptionCreateDurableWithArguments() throws JMSException, IOException
    {
        final String SELECTOR = "Selector='True'";
        _session.createDurableSubscriber(_topic, getName(), SELECTOR, false);

        //Validate

        List<String> results = _monitor.findMatches(SUB_PREFIX);

        assertEquals("Result set larger than expected.", 1, results.size());

        String log = getLog(results.get(0));

        validateMessageID("SUB-1001", log);

        String message = getMessageString(fromMessage(log));
        assertTrue("Durable not on log message:" + message, message.contains("Durable"));
        assertTrue("Selector not on log message:" + message, message.contains(SELECTOR));
    }

    /**
     * Description:
     * When a Subscription is closed it will log this so that it can be correlated with the Create.
     * Input:
     *
     * 1. Running Broker
     * 2. Client with a subscription.
     * 3. The subscription is then closed.
     * Output:
     *
     * <date> SUB-1002 : Close
     *
     * Validation Steps:
     * 1. The SUB ID is correct
     * 2. There must be a SUB-1001 Create message preceding this message
     * 3. This must be the last message from the given Subscription
     *
     * @throws java.io.IOException    - if there is a problem getting the matches
     * @throws javax.jms.JMSException - if there is a problem creating the consumer
     */
    public void testSubscriptionClose() throws JMSException, IOException
    {
        _session.createConsumer(_queue).close();

        //Validate
        List<String> results = _monitor.findMatches(SUB_PREFIX);

        //3
        assertEquals("Result set larger than expected.", 2, results.size());

        // 2
        String log = getLog(results.get(0));
        validateMessageID("SUB-1001", log);
        // 1
        log = getLog(results.get(1));
        validateMessageID("SUB-1002", log);

        String message = getMessageString(fromMessage(log));
        assertEquals("Log message is not close", "Close", message);

    }

    /**
     * Description:
     * When a Subscription fills its prefetch it will become suspended. This
     * will be logged as a SUB-1003 message.
     * Input:
     *
     * 1. Running broker
     * 2. Message Producer to put more data on the queue than the client's prefetch
     * 3. Client that ensures that its prefetch becomes full
     * Output:
     *
     * <date> SUB-1003 : State : <state>
     *
     * Validation Steps:
     * 1. The SUB ID is correct
     * 2. The state is correct
     *
     * @throws java.io.IOException    - if there is a problem getting the matches
     * @throws javax.jms.JMSException - if there is a problem creating the consumer
     */
    public void testSubscriptionSuspend() throws Exception, IOException
    {
        //Close session with large prefetch
        _connection.createSession(false, Session.AUTO_ACKNOWLEDGE).close();

        int PREFETCH = 15;

        //Create new session with small prefetch
        _session = ((AMQConnection) _connection).createSession(true, Session.SESSION_TRANSACTED, PREFETCH);

        MessageConsumer consumer = _session.createConsumer(_queue);

        _connection.start();

        //Start the dispatcher & Unflow the channel.
        consumer.receiveNoWait();

        //Fill the prefetch and two extra so that our receive bellow allows the
        // subscription to become active
        // Previously we set this to 17 so that it would return to a suspended
        // state. However, testing has shown that the state change can occur
        // sufficiently quickly that logging does not occur consistently enough
        // for testing.
        int SEND_COUNT = 16;
        sendMessage(_session, _queue, SEND_COUNT);
        _session.commit();
        // Retreive the first message, and start the flow of messages
        Message msg = consumer.receive(1000);
        assertNotNull("First message not retreived", msg);
        _session.commit();
        
        // Drain the queue to ensure there is time for the ACTIVE log message
        // Check that we can received all the messages
        int receivedCount = 0;
        while (msg != null)
        {
            receivedCount++;
            msg = consumer.receive(1000);
            _session.commit();
        }

        //Validate we received all the messages
        assertEquals("Not all sent messages received.", SEND_COUNT, receivedCount);

        // Fill the queue again to suspend the consumer
        sendMessage(_session, _queue, SEND_COUNT);
        _session.commit();

        //Validate
        List<String> results = _monitor.findMatches("SUB-1003");

        try
        {
            // Validation expects three messages.
            // The Actor can be any one of the following depending on the exactly what is going on on the broker.
            // Ideally we would test that we can get all of them but setting up
            // the timing to do this in a consistent way is not benefitial.
            // Ensuring the State is as expected is sufficient.
// INFO - MESSAGE [vh(/test)/qu(example.queue)] [sub:6(qu(example.queue))] SUB-1003 : State :
// INFO - MESSAGE [con:6(guest@anonymous(26562441)/test)/ch:3] [sub:6(qu(example.queue))] SUB-1003 : State :
// INFO - MESSAGE [sub:6(vh(test)/qu(example.queue))] [sub:6(qu(example.queue))] SUB-1003 : State :

            assertEquals("Result set not expected size:", 3, results.size());

            // Validate Initial Suspension
            String expectedState = "SUSPENDED";
            String log = getLog(results.get(0));
            validateSubscriptionState(log, expectedState);

            // After being suspended the subscription should become active.
            expectedState = "ACTIVE";
            log = getLog(results.get(1));
            validateSubscriptionState(log, expectedState);

            // Validate that it was re-suspended
            expectedState = "SUSPENDED";
            log = getLog(results.get(2));
            validateSubscriptionState(log, expectedState);
            // We only need validate the state.
        }
        catch (AssertionFailedError afe)
        {
            System.err.println("Log Dump:");
            for (String log : results)
            {
                System.err.println(log);
            }
            throw afe;
        }
        _connection.close();

        //Ensure the queue is drained before the test ends
        drainQueue(_queue);

    }

    /**
     * Validate that the given log statement is a well formatted SUB-1003
     * message. That means the ID and expected state are correct.
     *
     * @param log           the log to test
     * @param expectedState the state that should be logged.
     */
    private void validateSubscriptionState(String log, String expectedState)
    {
        validateMessageID("SUB-1003", log);
        String logMessage = getMessageString(fromMessage(log));
        assertTrue("Log Message does not start with 'State'" + logMessage,
                   logMessage.startsWith("State"));

        assertTrue("Log Message does not have expected State of '"
                   + expectedState + "'" + logMessage,
                   logMessage.endsWith(expectedState));
    }

}
