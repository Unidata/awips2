package org.apache.qpid.server.queue;
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


import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.server.configuration.VirtualHostConfiguration;
import org.apache.qpid.server.exchange.DirectExchange;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.store.TestableMemoryMessageStore;
import org.apache.qpid.server.store.StoredMessage;
import org.apache.qpid.server.subscription.MockSubscription;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.virtualhost.VirtualHostImpl;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.txn.AutoCommitTransaction;
import org.apache.qpid.server.txn.ServerTransaction;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.server.message.MessageMetaData;

public class SimpleAMQQueueTest extends TestCase
{

    protected SimpleAMQQueue _queue;
    protected VirtualHost _virtualHost;
    protected TestableMemoryMessageStore _store = new TestableMemoryMessageStore();
    protected AMQShortString _qname = new AMQShortString("qname");
    protected AMQShortString _owner = new AMQShortString("owner");
    protected AMQShortString _routingKey = new AMQShortString("routing key");
    protected DirectExchange _exchange = new DirectExchange();
    protected MockSubscription _subscription = new MockSubscription();
    protected FieldTable _arguments = null;

    MessagePublishInfo info = new MessagePublishInfo()
    {

        public AMQShortString getExchange()
        {
            return null;
        }

        public void setExchange(AMQShortString exchange)
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public boolean isImmediate()
        {
            return false;
        }

        public boolean isMandatory()
        {
            return false;
        }

        public AMQShortString getRoutingKey()
        {
            return null;
        }
    };

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        //Create Application Registry for test
        ApplicationRegistry applicationRegistry = (ApplicationRegistry)ApplicationRegistry.getInstance();

        PropertiesConfiguration env = new PropertiesConfiguration();
        _virtualHost = new VirtualHostImpl(new VirtualHostConfiguration(getClass().getName(), env), _store);
        applicationRegistry.getVirtualHostRegistry().registerVirtualHost(_virtualHost);

        _queue = (SimpleAMQQueue) AMQQueueFactory.createAMQQueueImpl(_qname, false, _owner, false, _virtualHost, _arguments);
    }

    @Override
    protected void tearDown()
    {
        _queue.stop();
        ApplicationRegistry.remove();
    }

    public void testCreateQueue() throws AMQException
    {
        _queue.stop();
        try {
            _queue = (SimpleAMQQueue) AMQQueueFactory.createAMQQueueImpl(null, false, _owner, false, _virtualHost, _arguments );
            assertNull("Queue was created", _queue);
        }
        catch (IllegalArgumentException e)
        {
            assertTrue("Exception was not about missing name",
                            e.getMessage().contains("name"));
        }

        try {
            _queue = new SimpleAMQQueue(_qname, false, _owner, false, null);
            assertNull("Queue was created", _queue);
        }
        catch (IllegalArgumentException e)
        {
            assertTrue("Exception was not about missing vhost",
                    e.getMessage().contains("Host"));
        }

        _queue = (SimpleAMQQueue) AMQQueueFactory.createAMQQueueImpl(_qname, false, _owner, false,
                                                                _virtualHost, _arguments);
        assertNotNull("Queue was not created", _queue);
    }

    public void testGetVirtualHost()
    {
        assertEquals("Virtual host was wrong", _virtualHost, _queue.getVirtualHost());
    }

    public void testBinding()
    {
        try
        {
            _queue.bind(_exchange, _routingKey, null);
            assertTrue("Routing key was not bound",
                            _exchange.getBindings().containsKey(_routingKey));
            assertEquals("Queue was not bound to key",
                        _exchange.getBindings().get(_routingKey).get(0),
                        _queue);
            assertEquals("Exchange binding count", 1,
                    _queue.getExchangeBindings().size());
            assertEquals("Wrong exchange bound", _routingKey,
                    _queue.getExchangeBindings().get(0).getRoutingKey());
            assertEquals("Wrong exchange bound", _exchange,
                    _queue.getExchangeBindings().get(0).getExchange());

            _queue.unBind(_exchange, _routingKey, null);
            assertFalse("Routing key was still bound",
                    _exchange.getBindings().containsKey(_routingKey));
            assertNull("Routing key was not empty",
                    _exchange.getBindings().get(_routingKey));
        }
        catch (AMQException e)
        {
            assertNull("Unexpected exception", e);
        }
    }

    public void testSubscription() throws AMQException
    {
        // Check adding a subscription adds it to the queue
        _queue.registerSubscription(_subscription, false);
        assertEquals("Subscription did not get queue", _queue,
                      _subscription.getQueue());
        assertEquals("Queue does not have consumer", 1,
                     _queue.getConsumerCount());
        assertEquals("Queue does not have active consumer", 1,
                _queue.getActiveConsumerCount());

        // Check sending a message ends up with the subscriber
        AMQMessage messageA = createMessage(new Long(24));
        _queue.enqueue(messageA);
        assertEquals(messageA, _subscription.getQueueContext().getLastSeenEntry().getMessage());

        // Check removing the subscription removes it's information from the queue
        _queue.unregisterSubscription(_subscription);
        assertTrue("Subscription still had queue", _subscription.isClosed());
        assertFalse("Queue still has consumer", 1 == _queue.getConsumerCount());
        assertFalse("Queue still has active consumer",
                1 == _queue.getActiveConsumerCount());

        AMQMessage messageB = createMessage(new Long (25));
        _queue.enqueue(messageB);
         assertNull(_subscription.getQueueContext());

    }

    public void testQueueNoSubscriber() throws AMQException, InterruptedException
    {
        AMQMessage messageA = createMessage(new Long(24));
        _queue.enqueue(messageA);
        _queue.registerSubscription(_subscription, false);
        Thread.sleep(150);
        assertEquals(messageA, _subscription.getQueueContext().getLastSeenEntry().getMessage());
    }

    public void testExclusiveConsumer() throws AMQException
    {
        // Check adding an exclusive subscription adds it to the queue
        _queue.registerSubscription(_subscription, true);
        assertEquals("Subscription did not get queue", _queue,
                _subscription.getQueue());
        assertEquals("Queue does not have consumer", 1,
                _queue.getConsumerCount());
        assertEquals("Queue does not have active consumer", 1,
                _queue.getActiveConsumerCount());

        // Check sending a message ends up with the subscriber
        AMQMessage messageA = createMessage(new Long(24));
        _queue.enqueue(messageA);
        assertEquals(messageA, _subscription.getQueueContext().getLastSeenEntry().getMessage());

        // Check we cannot add a second subscriber to the queue
        Subscription subB = new MockSubscription();
        Exception ex = null;
        try
        {
            _queue.registerSubscription(subB, false);
        }
        catch (AMQException e)
        {
           ex = e;
        }
        assertNotNull(ex);
        assertTrue(ex instanceof AMQException);

        // Check we cannot add an exclusive subscriber to a queue with an
        // existing subscription
        _queue.unregisterSubscription(_subscription);
        _queue.registerSubscription(_subscription, false);
        try
        {
            _queue.registerSubscription(subB, true);
        }
        catch (AMQException e)
        {
           ex = e;
        }
        assertNotNull(ex);
    }

    public void testAutoDeleteQueue() throws Exception
    {
       _queue.stop();
       _queue = new SimpleAMQQueue(_qname, false, null, true, _virtualHost);
       _queue.setDeleteOnNoConsumers(true);
       _queue.registerSubscription(_subscription, false);
       AMQMessage message = createMessage(new Long(25));
       _queue.enqueue(message);
       _queue.unregisterSubscription(_subscription);
       assertTrue("Queue was not deleted when subscription was removed",
                  _queue.isDeleted());
    }

    public void testResend() throws Exception
    {
        _queue.registerSubscription(_subscription, false);
        Long id = new Long(26);
        AMQMessage message = createMessage(id);
        _queue.enqueue(message);
        QueueEntry entry = _subscription.getQueueContext().getLastSeenEntry();
        entry.setRedelivered();
        _queue.resend(entry, _subscription);

    }

    public void testGetFirstMessageId() throws Exception
    {
        // Create message
        Long messageId = new Long(23);
        AMQMessage message = createMessage(messageId);

        // Put message on queue
        _queue.enqueue(message);
        // Get message id
        Long testmsgid = _queue.getMessagesOnTheQueue(1).get(0);

        // Check message id
        assertEquals("Message ID was wrong", messageId, testmsgid);
    }

    public void testGetFirstFiveMessageIds() throws Exception
    {
        for (int i = 0 ; i < 5; i++)
        {
            // Create message
            Long messageId = new Long(i);
            AMQMessage message = createMessage(messageId);
            // Put message on queue
            _queue.enqueue(message);
        }
        // Get message ids
        List<Long> msgids = _queue.getMessagesOnTheQueue(5);

        // Check message id
        for (int i = 0; i < 5; i++)
        {
            Long messageId = new Long(i);
            assertEquals("Message ID was wrong", messageId, msgids.get(i));
        }
    }

    public void testGetLastFiveMessageIds() throws Exception
    {
        for (int i = 0 ; i < 10; i++)
        {
            // Create message
            Long messageId = new Long(i);
            AMQMessage message = createMessage(messageId);
            // Put message on queue
            _queue.enqueue(message);
        }
        // Get message ids
        List<Long> msgids = _queue.getMessagesOnTheQueue(5, 5);

        // Check message id
        for (int i = 0; i < 5; i++)
        {
            Long messageId = new Long(i+5);
            assertEquals("Message ID was wrong", messageId, msgids.get(i));
        }
    }

    public void testGetMessagesRangeOnTheQueue() throws Exception
    {
        for (int i = 1 ; i <= 10; i++)
        {
            // Create message
            Long messageId = new Long(i);
            AMQMessage message = createMessage(messageId);
            // Put message on queue
            _queue.enqueue(message);
        }

        // Get non-existent 0th QueueEntry & check returned list was empty
        // (the position parameters in this method are indexed from 1)
        List<QueueEntry> entries = _queue.getMessagesRangeOnTheQueue(0, 0);
        assertTrue(entries.size() == 0);

        // Check that when 'from' is 0 it is ignored and the range continues from 1
        entries = _queue.getMessagesRangeOnTheQueue(0, 2);
        assertTrue(entries.size() == 2);
        long msgID = entries.get(0).getMessage().getMessageNumber();
        assertEquals("Message ID was wrong", msgID, 1L);
        msgID = entries.get(1).getMessage().getMessageNumber();
        assertEquals("Message ID was wrong", msgID, 2L);

        // Check that when 'from' is greater than 'to' the returned list is empty
        entries = _queue.getMessagesRangeOnTheQueue(5, 4);
        assertTrue(entries.size() == 0);

        // Get first QueueEntry & check id
        entries = _queue.getMessagesRangeOnTheQueue(1, 1);
        assertTrue(entries.size() == 1);
        msgID = entries.get(0).getMessage().getMessageNumber();
        assertEquals("Message ID was wrong", msgID, 1L);

        // Get 5th,6th,7th entries and check id's
        entries = _queue.getMessagesRangeOnTheQueue(5, 7);
        assertTrue(entries.size() == 3);
        msgID = entries.get(0).getMessage().getMessageNumber();
        assertEquals("Message ID was wrong", msgID, 5L);
        msgID = entries.get(1).getMessage().getMessageNumber();
        assertEquals("Message ID was wrong", msgID, 6L);
        msgID = entries.get(2).getMessage().getMessageNumber();
        assertEquals("Message ID was wrong", msgID, 7L);

        // Get 10th QueueEntry & check id
        entries = _queue.getMessagesRangeOnTheQueue(10, 10);
        assertTrue(entries.size() == 1);
        msgID = entries.get(0).getMessage().getMessageNumber();
        assertEquals("Message ID was wrong", msgID, 10L);

        // Get non-existent 11th QueueEntry & check returned set was empty
        entries = _queue.getMessagesRangeOnTheQueue(11, 11);
        assertTrue(entries.size() == 0);

        // Get 9th,10th, and non-existent 11th entries & check result is of size 2 with correct IDs
        entries = _queue.getMessagesRangeOnTheQueue(9, 11);
        assertTrue(entries.size() == 2);
        msgID = entries.get(0).getMessage().getMessageNumber();
        assertEquals("Message ID was wrong", msgID, 9L);
        msgID = entries.get(1).getMessage().getMessageNumber();
        assertEquals("Message ID was wrong", msgID, 10L);
    }

    public void testEnqueueDequeueOfPersistentMessageToNonDurableQueue() throws AMQException
    {
        // Create IncomingMessage and nondurable queue
        final IncomingMessage msg = new IncomingMessage(info);
        ContentHeaderBody contentHeaderBody = new ContentHeaderBody();
        contentHeaderBody.properties = new BasicContentHeaderProperties();
        ((BasicContentHeaderProperties) contentHeaderBody.properties).setDeliveryMode((byte) 2);
        msg.setContentHeaderBody(contentHeaderBody);

        final ArrayList<AMQQueue> qs = new ArrayList<AMQQueue>();

        // Send persistent message

        qs.add(_queue);
        MessageMetaData metaData = msg.headersReceived();
        StoredMessage handle = _store.addMessage(metaData);
        msg.setStoredMessage(handle);


        ServerTransaction txn = new AutoCommitTransaction(_store);

        txn.enqueue(qs, msg, new ServerTransaction.Action()
                                    {
                                        public void postCommit()
                                        {
                                            msg.enqueue(qs);
                                        }

                                        public void onRollback()
                                        {
                                        }
                                    });



        // Check that it is enqueued
        AMQQueue data = _store.getMessages().get(1L);
        assertNull(data);

        // Dequeue message
        MockQueueEntry entry = new MockQueueEntry();
        AMQMessage amqmsg = new AMQMessage(handle);

        entry.setMessage(amqmsg);
        _queue.dequeue(entry);

        // Check that it is dequeued
        data = _store.getMessages().get(1L);
        assertNull(data);
    }


    public class TestMessage extends AMQMessage
    {
        private final long _tag;
        private int _count;

        TestMessage(long tag, long messageId, MessagePublishInfo publishBody)
                throws AMQException
        {
            this(tag, messageId, publishBody, new ContentHeaderBody(1, 1, new BasicContentHeaderProperties(), 0));

        }
        TestMessage(long tag, long messageId, MessagePublishInfo publishBody, ContentHeaderBody chb)
                throws AMQException
        {
            super(new MockStoredMessage(messageId, publishBody, chb));
            _tag = tag;
        }

        public boolean incrementReference()
        {
            _count++;
            return true;
        }

        public void decrementReference()
        {
            _count--;
        }

        void assertCountEquals(int expected)
        {
            assertEquals("Wrong count for message with tag " + _tag, expected, _count);
        }
    }

    protected AMQMessage createMessage(Long id) throws AMQException
    {
        AMQMessage messageA = new TestMessage(id, id, info);
        return messageA;
    }
}
