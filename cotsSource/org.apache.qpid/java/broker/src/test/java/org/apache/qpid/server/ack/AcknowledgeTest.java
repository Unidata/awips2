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
package org.apache.qpid.server.ack;


import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.protocol.InternalTestProtocolSession;
import org.apache.qpid.server.util.InternalBrokerBaseCase;

import java.util.List;

public class AcknowledgeTest extends InternalBrokerBaseCase
{

    public void testTransactionalSingleAck() throws AMQException
    {
        _channel.setLocalTransactional();
        runMessageAck(1, 1, 1, false, 0);
    }

    public void testTransactionalMultiAck() throws AMQException
    {
        _channel.setLocalTransactional();
        runMessageAck(10, 1, 5, true, 5);
    }

    public void testTransactionalAckAll() throws AMQException
    {
        _channel.setLocalTransactional();
        runMessageAck(10, 1, 0, true, 0);
    }

    public void testNonTransactionalSingleAck() throws AMQException
    {
        runMessageAck(1, 1, 1, false, 0);
    }

    public void testNonTransactionalMultiAck() throws AMQException
    {
        runMessageAck(10, 1, 5, true, 5);
    }

    public void testNonTransactionalAckAll() throws AMQException
    {
        runMessageAck(10, 1, 0, true, 0);
    }

    protected void runMessageAck(int sendMessageCount, long firstDeliveryTag, long acknowledgeDeliveryTag, boolean acknowldegeMultiple, int remainingUnackedMessages) throws AMQException
    {
        //Check store is empty
        checkStoreContents(0);

        //Send required messsages to the queue
        publishMessages(_session, _channel, sendMessageCount);

        if (_channel.isTransactional())
        {
            _channel.commit();
        }

        //Ensure they are stored
        checkStoreContents(sendMessageCount);

        //Check that there are no unacked messages
        assertEquals("Channel should have no unacked msgs ", 0, _channel.getUnacknowledgedMessageMap().size());

        //Subscribe to the queue
        AMQShortString subscriber = subscribe(_session, _channel, _queue);

        _queue.deliverAsync();

        //Wait for the messages to be delivered
        _session.awaitDelivery(sendMessageCount);

        //Check that they are all waiting to be acknoledged
        assertEquals("Channel should have unacked msgs", sendMessageCount, _channel.getUnacknowledgedMessageMap().size());

        List<InternalTestProtocolSession.DeliveryPair> messages = _session.getDelivers(_channel.getChannelId(), subscriber, sendMessageCount);

        //Double check we received the right number of messages
        assertEquals(sendMessageCount, messages.size());

        //Check that the first message has the expected deliveryTag
        assertEquals("First message does not have expected deliveryTag", firstDeliveryTag, messages.get(0).getDeliveryTag());

        //Send required Acknowledgement
        _channel.acknowledgeMessage(acknowledgeDeliveryTag, acknowldegeMultiple);

        if (_channel.isTransactional())
        {
            _channel.commit();
        }

        // Check Remaining Acknowledgements
        assertEquals("Channel unacked msgs count incorrect", remainingUnackedMessages, _channel.getUnacknowledgedMessageMap().size());

        //Check store contents are also correct.
        checkStoreContents(remainingUnackedMessages);
    }

}
