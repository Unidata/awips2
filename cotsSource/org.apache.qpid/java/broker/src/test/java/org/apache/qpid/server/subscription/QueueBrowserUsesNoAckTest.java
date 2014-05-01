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
package org.apache.qpid.server.subscription;

import org.apache.qpid.server.util.InternalBrokerBaseCase;
import org.apache.qpid.server.protocol.InternalTestProtocolSession;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.AMQException;

import java.util.List;

public class QueueBrowserUsesNoAckTest extends InternalBrokerBaseCase
{

    public void testQueueBrowserUsesNoAck() throws AMQException
    {
        int sendMessageCount = 2;
        int prefetch = 1;

        //Check store is empty
        checkStoreContents(0);

        //Send required messsages to the queue
        publishMessages(_session, _channel, sendMessageCount);

        //Ensure they are stored
        checkStoreContents(sendMessageCount);

        //Check that there are no unacked messages
        assertEquals("Channel should have no unacked msgs ", 0,
                     _channel.getUnacknowledgedMessageMap().size());

        //Set the prefetch on the session to be less than the sent messages
        _channel.setCredit(0, prefetch);

        //browse the queue
        AMQShortString browser = browse(_channel, _queue);

        _queue.deliverAsync();

        //Wait for messages to fill the prefetch
        _session.awaitDelivery(prefetch);

        //Get those messages
        List<InternalTestProtocolSession.DeliveryPair> messages =
                _session.getDelivers(_channel.getChannelId(), browser,
                                     prefetch);

        //Ensure we recevied the prefetched messages
        assertEquals(prefetch, messages.size());

        //Check the process didn't suspend the subscription as this would
        // indicate we are using the prefetch credit. i.e. using acks not No-Ack
        assertTrue("The subscription has been suspended",
                   !_channel.getSubscription(browser).getState()
                           .equals(Subscription.State.SUSPENDED));       
    }

}
