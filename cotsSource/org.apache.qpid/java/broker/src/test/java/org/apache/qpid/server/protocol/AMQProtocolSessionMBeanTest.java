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
package org.apache.qpid.server.protocol;

import junit.framework.TestCase;
import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.management.common.mbeans.ManagedConnection;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.AMQQueueFactory;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.server.store.SkeletonMessageStore;

import javax.management.JMException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularData;


/** Test class to test MBean operations for AMQMinaProtocolSession. */
public class AMQProtocolSessionMBeanTest extends TestCase
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(AMQProtocolSessionMBeanTest.class);

    private MessageStore _messageStore = new SkeletonMessageStore();
    private AMQProtocolEngine _protocolSession;
    private AMQChannel _channel;
    private AMQProtocolSessionMBean _mbean;

    public void testChannels() throws Exception
    {
        // check the channel count is correct
        int channelCount = _mbean.channels().size();
        assertTrue(channelCount == 1);
        AMQQueue queue = AMQQueueFactory.createAMQQueueImpl(new AMQShortString("testQueue_" + System.currentTimeMillis()),
                                                            false,
                                                            new AMQShortString("test"),
                                                            true,
                                                            _protocolSession.getVirtualHost(), null);
        AMQChannel channel = new AMQChannel(_protocolSession, 2, _messageStore);
        channel.setDefaultQueue(queue);
        _protocolSession.addChannel(channel);
        channelCount = _mbean.channels().size();
        assertTrue(channelCount == 2);

        // general properties test
        _mbean.setMaximumNumberOfChannels(1000L);
        assertTrue(_mbean.getMaximumNumberOfChannels() == 1000L);

        // check APIs
        AMQChannel channel3 = new AMQChannel(_protocolSession, 3, _messageStore);
        channel3.setLocalTransactional();
        _protocolSession.addChannel(channel3);
        _mbean.rollbackTransactions(2);
        _mbean.rollbackTransactions(3);
        _mbean.commitTransactions(2);
        _mbean.commitTransactions(3);

        // This should throw exception, because the channel does't exist
        try
        {
            _mbean.commitTransactions(4);
            fail();
        }
        catch (JMException ex)
        {
            log.debug("expected exception is thrown :" + ex.getMessage());
        }

        // check channels() return type conveys flow control blocking status correctly
        AMQChannel channel4 = new AMQChannel(_protocolSession, 4, _messageStore);
        _protocolSession.addChannel(channel4);
        channel4.setDefaultQueue(queue);
        
        final String blocking = ManagedConnection.COMPOSITE_ITEM_NAMES[4];
        TabularData channels = _mbean.channels();
        CompositeData chan4result = channels.get(new Integer[]{4});
        assertNotNull(chan4result);
        assertEquals("Flow should not have been blocked", false, chan4result.get(blocking));
        
        channel4.block(queue);
        channels = _mbean.channels();
        chan4result = channels.get(new Integer[]{4});
        assertNotNull(chan4result);
        assertEquals("Flow should have been blocked", true, chan4result.get(blocking));    
        
        channel4.unblock(queue);
        channels = _mbean.channels();
        chan4result = channels.get(new Integer[]{4});
        assertNotNull(chan4result);
        assertEquals("Flow should have been unblocked", false, chan4result.get(blocking));  
        
        // check if closing of session works
        _protocolSession.addChannel(new AMQChannel(_protocolSession, 5, _messageStore));
        _mbean.closeConnection();
        try
        {
            channelCount = _mbean.channels().size();
            assertTrue(channelCount == 0);
            // session is now closed so adding another channel should throw an exception
            _protocolSession.addChannel(new AMQChannel(_protocolSession, 6, _messageStore));
            fail();
        }
        catch (AMQException ex)
        {
            log.debug("expected exception is thrown :" + ex.getMessage());
        }
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();

        VirtualHost vhost = ApplicationRegistry.getInstance().getVirtualHostRegistry().getVirtualHost("test");
        _protocolSession = new InternalTestProtocolSession(vhost);

        _channel = new AMQChannel(_protocolSession, 1, _messageStore);
        _protocolSession.addChannel(_channel);
        _mbean = (AMQProtocolSessionMBean) _protocolSession.getManagedObject();
    }

    @Override
    protected void tearDown()
    {
        ApplicationRegistry.remove();
    }
}
