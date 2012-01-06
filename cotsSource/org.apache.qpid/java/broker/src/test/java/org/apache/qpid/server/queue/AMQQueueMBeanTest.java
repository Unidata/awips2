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
package org.apache.qpid.server.queue;

import junit.framework.TestCase;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.ContentBody;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.framing.abstraction.ContentChunk;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.util.TestApplicationRegistry;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.server.message.MessageMetaData;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.subscription.SubscriptionFactory;
import org.apache.qpid.server.subscription.SubscriptionFactoryImpl;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.protocol.InternalTestProtocolSession;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.registry.IApplicationRegistry;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.server.store.MemoryMessageStore;
import org.apache.qpid.server.store.TestableMemoryMessageStore;
import org.apache.mina.common.ByteBuffer;
import org.apache.commons.configuration.PropertiesConfiguration;

import javax.management.JMException;

import java.util.ArrayList;

/**
 * Test class to test AMQQueueMBean attribtues and operations
 */
public class AMQQueueMBeanTest extends TestCase
{
    private static long MESSAGE_SIZE = 1000;
    private AMQQueue _queue;
    private AMQQueueMBean _queueMBean;
    private MessageStore _messageStore;
    private VirtualHost _virtualHost;
    private AMQProtocolSession _protocolSession;
    private static final SubscriptionFactoryImpl SUBSCRIPTION_FACTORY = SubscriptionFactoryImpl.INSTANCE;

    public void testMessageCountTransient() throws Exception
    {
        int messageCount = 10;
        sendMessages(messageCount, false);
        assertTrue(_queueMBean.getMessageCount() == messageCount);
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);
        long queueDepth = (messageCount * MESSAGE_SIZE);
        assertTrue(_queueMBean.getQueueDepth() == queueDepth);

        _queueMBean.deleteMessageFromTop();
        assertTrue(_queueMBean.getMessageCount() == (messageCount - 1));
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);

        _queueMBean.clearQueue();
        assertEquals(0,(int)_queueMBean.getMessageCount());
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);

        //Ensure that the data has been removed from the Store
        verifyBrokerState();
    }

    public void testMessageCountPersistent() throws Exception
    {
        int messageCount = 10;
        sendMessages(messageCount, true);
        assertEquals("", messageCount, _queueMBean.getMessageCount().intValue());
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);
        long queueDepth = (messageCount * MESSAGE_SIZE);
        assertTrue(_queueMBean.getQueueDepth() == queueDepth);

        _queueMBean.deleteMessageFromTop();
        assertTrue(_queueMBean.getMessageCount() == (messageCount - 1));
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);

        _queueMBean.clearQueue();
        assertTrue(_queueMBean.getMessageCount() == 0);
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);

        //Ensure that the data has been removed from the Store
        verifyBrokerState();
    }

    public void testDeleteMessages() throws Exception
    {
        int messageCount = 10;
        sendMessages(messageCount, true);
        assertEquals("", messageCount, _queueMBean.getMessageCount().intValue());
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);
        long queueDepth = (messageCount * MESSAGE_SIZE);
        assertTrue(_queueMBean.getQueueDepth() == queueDepth);

        //delete first message
        _queueMBean.deleteMessages(1L,1L);
        assertTrue(_queueMBean.getMessageCount() == (messageCount - 1));
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);
        try
        {
            _queueMBean.viewMessageContent(1L);
            fail("Message should no longer be on the queue");
        }
        catch(Exception e)
        {

        }

        //delete last message, leaving 2nd to 9th
        _queueMBean.deleteMessages(10L,10L);
        assertTrue(_queueMBean.getMessageCount() == (messageCount - 2));
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);
        try
        {
            _queueMBean.viewMessageContent(10L);
            fail("Message should no longer be on the queue");
        }
        catch(Exception e)
        {

        }

        //delete remaining messages, leaving none
        _queueMBean.deleteMessages(2L,9L);
        assertTrue(_queueMBean.getMessageCount() == (0));
        assertTrue(_queueMBean.getReceivedMessageCount() == messageCount);

        //Ensure that the data has been removed from the Store
        verifyBrokerState();
    }

    // todo: collect to a general testing class -duplicated from Systest/MessageReturntest
    private void verifyBrokerState()
    {

        TestableMemoryMessageStore store = (TestableMemoryMessageStore)_virtualHost.getMessageStore();

        // Unlike MessageReturnTest there is no need for a delay as there this thread does the clean up.

        assertEquals("Store should have no messages:" + store.getMessageCount(), 0, store.getMessageCount());
    }

    public void testConsumerCount() throws AMQException
    {

        assertTrue(_queue.getActiveConsumerCount() == 0);
        assertTrue(_queueMBean.getActiveConsumerCount() == 0);


        InternalTestProtocolSession protocolSession = new InternalTestProtocolSession(_virtualHost);

        AMQChannel channel = new AMQChannel(protocolSession, 1, _messageStore);
        protocolSession.addChannel(channel);

        Subscription subscription =
                SUBSCRIPTION_FACTORY.createSubscription(channel.getChannelId(), protocolSession, new AMQShortString("test"), false, null, false, channel.getCreditManager());

        _queue.registerSubscription(subscription, false);
        assertEquals(1,(int)_queueMBean.getActiveConsumerCount());


        SubscriptionFactory subscriptionFactory = SUBSCRIPTION_FACTORY;
        Subscription s1 = subscriptionFactory.createSubscription(channel.getChannelId(),
                                                                 protocolSession,
                                                                 new AMQShortString("S1"),
                                                                 false,
                                                                 null,
                                                                 true,
                channel.getCreditManager());

        Subscription s2 = subscriptionFactory.createSubscription(channel.getChannelId(),
                                                                 protocolSession,
                                                                 new AMQShortString("S2"),
                                                                 false,
                                                                 null,
                                                                 true,
                channel.getCreditManager());
        _queue.registerSubscription(s1,false);
        _queue.registerSubscription(s2,false);
        assertTrue(_queueMBean.getActiveConsumerCount() == 3);
        assertTrue(_queueMBean.getConsumerCount() == 3);

        s1.close();
        assertEquals(2, (int) _queueMBean.getActiveConsumerCount());
        assertTrue(_queueMBean.getConsumerCount() == 3);
    }

    public void testGeneralProperties()
    {
        long maxQueueDepth = 1000; // in bytes
        _queueMBean.setMaximumMessageCount(50000l);
        _queueMBean.setMaximumMessageSize(2000l);
        _queueMBean.setMaximumQueueDepth(maxQueueDepth);

        assertTrue(_queueMBean.getMaximumMessageCount() == 50000);
        assertTrue(_queueMBean.getMaximumMessageSize() == 2000);
        assertTrue(_queueMBean.getMaximumQueueDepth() == (maxQueueDepth));

        assertTrue(_queueMBean.getName().equals("testQueue"));
        assertFalse(_queueMBean.isAutoDelete());
        assertFalse(_queueMBean.isDurable());
    }

    public void testExceptions() throws Exception
    {
        try
        {
            _queueMBean.viewMessages(0L, 3L);
            fail();
        }
        catch (JMException ex)
        {

        }

        try
        {
            _queueMBean.viewMessages(2L, 1L);
            fail();
        }
        catch (JMException ex)
        {

        }

        try
        {
            _queueMBean.viewMessages(-1L, 1L);
            fail();
        }
        catch (JMException ex)
        {

        }

        try
        {
            long end = Integer.MAX_VALUE;
            end+=2;
            _queueMBean.viewMessages(1L, end);
            fail("Expected Exception due to oversized(> 2^31) message range");
        }
        catch (JMException ex)
        {

        }

        IncomingMessage msg = message(false, false);
        _queue.clearQueue();
        ArrayList<AMQQueue> qs = new ArrayList<AMQQueue>();
        qs.add(_queue);
        msg.enqueue(qs);
        MessageMetaData mmd = msg.headersReceived();
        msg.setStoredMessage(_messageStore.addMessage(mmd));
        long id = msg.getMessageNumber();

        msg.addContentBodyFrame(new ContentChunk()
        {
            ByteBuffer _data = ByteBuffer.allocate((int)MESSAGE_SIZE);

            {
                _data.limit((int)MESSAGE_SIZE);
            }

            public int getSize()
            {
                return (int) MESSAGE_SIZE;
            }

            public ByteBuffer getData()
            {
                return _data;
            }

            public void reduceToFit()
            {

            }
        });

        AMQMessage m = new AMQMessage(msg.getStoredMessage());
        for(AMQQueue q : msg.getDestinationQueues())
        {
            q.enqueue(m);
        }
//        _queue.process(_storeContext, new QueueEntry(_queue, msg), false);
        _queueMBean.viewMessageContent(id);
        try
        {
            _queueMBean.viewMessageContent(id + 1);
            fail();
        }
        catch (JMException ex)
        {

        }
    }
    
    public void testFlowControlProperties() throws Exception
    {
        assertTrue(_queueMBean.getCapacity() == 0);
        assertTrue(_queueMBean.getFlowResumeCapacity() == 0);
        assertFalse(_queueMBean.isFlowOverfull());
        
        //capacity currently 0, try setting FlowResumeCapacity above this
        try
        {
            _queueMBean.setFlowResumeCapacity(1L);
            fail("Should have failed to allow setting FlowResumeCapacity above Capacity");
        }
        catch (IllegalArgumentException ex)
        {
            //expected exception
            assertTrue(_queueMBean.getFlowResumeCapacity() == 0);
        }
        
        //add a message to the queue
        sendMessages(1, true);

        //(FlowResume)Capacity currently 0, set both to 2
        _queueMBean.setCapacity(2L);
        assertTrue(_queueMBean.getCapacity() == 2L);
        _queueMBean.setFlowResumeCapacity(2L);
        assertTrue(_queueMBean.getFlowResumeCapacity() == 2L);
        
        //Try setting Capacity below FlowResumeCapacity
        try
        {
            _queueMBean.setCapacity(1L);
            fail("Should have failed to allow setting Capacity below FlowResumeCapacity");
        }
        catch (IllegalArgumentException ex)
        {
            //expected exception
            assertTrue(_queueMBean.getCapacity() == 2);
        }
        
        //create a channel and use it to exercise the capacity check mechanism
        AMQChannel channel = new AMQChannel(_protocolSession, 1, _messageStore);
        _queue.checkCapacity(channel);
        
        assertTrue(_queueMBean.isFlowOverfull());
        assertTrue(channel.getBlocking());
        
        //set FlowResumeCapacity to MESSAGE_SIZE and check queue is now underfull and channel unblocked
        _queueMBean.setCapacity(MESSAGE_SIZE);//must increase capacity too
        _queueMBean.setFlowResumeCapacity(MESSAGE_SIZE);
        
        assertFalse(_queueMBean.isFlowOverfull());
        assertFalse(channel.getBlocking());
    }

    private IncomingMessage message(final boolean immediate, boolean persistent) throws AMQException
    {
        MessagePublishInfo publish = new MessagePublishInfo()
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
                return immediate;
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

        ContentHeaderBody contentHeaderBody = new ContentHeaderBody();
        contentHeaderBody.bodySize = MESSAGE_SIZE;   // in bytes
        contentHeaderBody.properties = new BasicContentHeaderProperties();
        ((BasicContentHeaderProperties) contentHeaderBody.properties).setDeliveryMode((byte) (persistent ? 2 : 1));
        IncomingMessage msg = new IncomingMessage(publish);
        msg.setContentHeaderBody(contentHeaderBody);
        return msg;

    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();

        PropertiesConfiguration configuration = new PropertiesConfiguration();
        configuration.setProperty("virtualhosts.virtualhost.test.store.class", TestableMemoryMessageStore.class.getName());
        IApplicationRegistry  applicationRegistry  = new TestApplicationRegistry(new ServerConfiguration(configuration));
        ApplicationRegistry.initialise(applicationRegistry );

        configuration.setProperty("virtualhosts.virtualhost.test.store.class", TestableMemoryMessageStore.class.getName());

        _virtualHost = applicationRegistry.getVirtualHostRegistry().getVirtualHost("test");
        _messageStore = _virtualHost.getMessageStore();

        _queue = AMQQueueFactory.createAMQQueueImpl(new AMQShortString("testQueue"), false, new AMQShortString("AMQueueMBeanTest"), false, _virtualHost,
                                                    null);
        _queueMBean = new AMQQueueMBean(_queue);

        _protocolSession = new InternalTestProtocolSession(_virtualHost);
    }

    public void tearDown()
    {
        ApplicationRegistry.remove();
    }

    private void sendMessages(int messageCount, boolean persistent) throws AMQException
    {
        for (int i = 0; i < messageCount; i++)
        {
            IncomingMessage currentMessage = message(false, persistent);
            ArrayList<AMQQueue> qs = new ArrayList<AMQQueue>();
            qs.add(_queue);
            currentMessage.enqueue(qs);

            // route header
            MessageMetaData mmd = currentMessage.headersReceived();
            currentMessage.setStoredMessage(_messageStore.addMessage(mmd));

            // Add the body so we have somthing to test later
            currentMessage.addContentBodyFrame(
                    _protocolSession.getMethodRegistry()
                                                       .getProtocolVersionMethodConverter()
                                                       .convertToContentChunk(
                                                       new ContentBody(ByteBuffer.allocate((int) MESSAGE_SIZE),
                                                                       MESSAGE_SIZE)));

            AMQMessage m = new AMQMessage(currentMessage.getStoredMessage());
            for(AMQQueue q : currentMessage.getDestinationQueues())
            {
                q.enqueue(m);
            }


        }
    }
}
