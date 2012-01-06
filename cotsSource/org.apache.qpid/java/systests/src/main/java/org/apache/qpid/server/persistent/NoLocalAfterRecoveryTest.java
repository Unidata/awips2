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
package org.apache.qpid.server.persistent;

import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.jms.ConnectionListener;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.registry.ConfigurationFileApplicationRegistry;
import org.apache.qpid.server.store.DerbyMessageStore;
import org.apache.commons.configuration.XMLConfiguration;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.Session;
import javax.jms.Topic;
import javax.jms.TopicSubscriber;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import java.util.concurrent.CountDownLatch;
import java.io.File;

/**
 * QPID-1813 : We do not store the client id with a message so on store restart
 * that information is lost and we are unable to perform no local checks.
 *
 * QPID-1813 highlights the lack of testing here as the broker will NPE as it
 * assumes that the client id of the publisher will always exist
 */
public class NoLocalAfterRecoveryTest extends QpidTestCase implements ConnectionListener
{
    protected final String MY_TOPIC_SUBSCRIPTION_NAME = this.getName();
    protected static final int SEND_COUNT = 10;
    private CountDownLatch _failoverComplete = new CountDownLatch(1);

    protected ConnectionURL _connectionURL;

    @Override
    protected void setUp() throws Exception
    {

        XMLConfiguration configuration = new XMLConfiguration(_configFile);
        configuration.setProperty("virtualhosts.virtualhost.test.store.class", "org.apache.qpid.server.store.DerbyMessageStore");
        configuration.setProperty("virtualhosts.virtualhost.test.store."+ DerbyMessageStore.ENVIRONMENT_PATH_PROPERTY,
                                  System.getProperty("QPID_WORK", System.getProperty("java.io.tmpdir")) + File.separator + "derbyDB-NoLocalAfterRecoveryTest");

        File tmpFile = File.createTempFile("configFile", "test");
        tmpFile.deleteOnExit();
        configuration.save(tmpFile);

        _configFile = tmpFile;
        _connectionURL = getConnectionURL();

        BrokerDetails details = _connectionURL.getBrokerDetails(0);

        // Due to the problem with SingleServer delaying on all connection
        // attempts. So using a high retry value.
        if (_broker.equals(VM))
        {
            // Local testing suggests InVM restart takes under a second
            details.setProperty(BrokerDetails.OPTIONS_RETRY, "5");
            details.setProperty(BrokerDetails.OPTIONS_CONNECT_DELAY, "200");
        }
        else
        {
            // This will attempt to failover for 3 seconds.
            // Local testing suggests failover takes 2 seconds
            details.setProperty(BrokerDetails.OPTIONS_RETRY, "10");
            details.setProperty(BrokerDetails.OPTIONS_CONNECT_DELAY, "500");
        }

        super.setUp();        
    }

    public void test() throws Exception
    {

        Connection connection = getConnection(_connectionURL);
        Session session = connection.createSession(true, Session.SESSION_TRANSACTED);

        Topic topic = (Topic) getInitialContext().lookup("topic");

        TopicSubscriber noLocalSubscriber = session.
                createDurableSubscriber(topic, MY_TOPIC_SUBSCRIPTION_NAME + "-NoLocal",
                                        null, true);

        TopicSubscriber normalSubscriber = session.
                createDurableSubscriber(topic, MY_TOPIC_SUBSCRIPTION_NAME + "-Normal",
                                        null, false);

        List<Message> sent = sendMessage(session, topic, SEND_COUNT);

        session.commit();

        assertEquals("Incorrect number of messages sent",
                     SEND_COUNT, sent.size());


        // Check messages can be received as expected.
        connection.start();

        assertTrue("No Local Subscriber is not a no-local subscriber",
                   noLocalSubscriber.getNoLocal());

        assertFalse("Normal Subscriber is a no-local subscriber",
                    normalSubscriber.getNoLocal());


        List<Message> received = receiveMessage(noLocalSubscriber, SEND_COUNT);
        assertEquals("No Local Subscriber Received messages", 0, received.size());

        received = receiveMessage(normalSubscriber, SEND_COUNT);
        assertEquals("Normal Subscriber Received no messages",
                     SEND_COUNT, received.size());


        ((AMQConnection)connection).setConnectionListener(this);

        restartBroker();


        //Await
        if (!_failoverComplete.await(4000L, TimeUnit.MILLISECONDS))
        {
            fail("Failover Failed to compelete");
        }

        session.rollback();

        //Failover will restablish our clients
        assertTrue("No Local Subscriber is not a no-local subscriber",
                   noLocalSubscriber.getNoLocal());

        assertFalse("Normal Subscriber is a no-local subscriber",
                    normalSubscriber.getNoLocal());


        // NOTE : here that the NO-local subscriber actually now gets ALL the
        // messages as the connection has failed and they are consuming on a
        // different connnection to the one that was published on.
        received = receiveMessage(noLocalSubscriber, SEND_COUNT);
        assertEquals("No Local Subscriber Received messages", SEND_COUNT, received.size());

        received = receiveMessage(normalSubscriber, SEND_COUNT);
        assertEquals("Normal Subscriber Received no messages",
                     SEND_COUNT, received.size());

        //leave the store in a clean state.
        session.commit();
    }

    protected List<Message> assertReceiveMessage(MessageConsumer messageConsumer,
                                                 int count) throws JMSException
    {

        List<Message> receivedMessages = new ArrayList<Message>(count);
        for (int i = 0; i < count; i++)
        {
            Message received = messageConsumer.receive(1000);

            if (received != null)
            {
                receivedMessages.add(received);
            }
            else
            {
                fail("Only "
                     + receivedMessages.size() + "/" + count + " received.");
            }
        }

        return receivedMessages;
    }

    protected List<Message> receiveMessage(MessageConsumer messageConsumer,
                                           int count) throws JMSException
    {

        List<Message> receivedMessages = new ArrayList<Message>(count);
        for (int i = 0; i < count; i++)
        {
            Message received = messageConsumer.receive(1000);

            if (received != null)
            {
                receivedMessages.add(received);
            }
            else
            {
                break;
            }
        }

        return receivedMessages;
    }

    public void bytesSent(long count)
    {

    }

    public void bytesReceived(long count)
    {

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
        _failoverComplete.countDown();
    }
}
