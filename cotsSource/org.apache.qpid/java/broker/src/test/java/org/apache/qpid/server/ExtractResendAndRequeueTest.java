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
package org.apache.qpid.server;

import junit.framework.TestCase;
import org.apache.qpid.server.ack.UnacknowledgedMessageMapImpl;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.qpid.server.queue.SimpleQueueEntryList;
import org.apache.qpid.server.queue.MockAMQMessage;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.MockAMQQueue;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.server.queue.QueueEntryIterator;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.subscription.MockSubscription;
import org.apache.qpid.server.store.MemoryMessageStore;
import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.AMQException;

import java.util.Map;
import java.util.LinkedHashMap;
import java.util.LinkedList;

/**
 * QPID-1385 : Race condition between added to unacked map and resending due to a rollback.
 *
 * In AMQChannel _unackedMap.clear() was done after the visit. This meant that the clear was not in the same
 * synchronized block as as the preparation to resend.
 *
 * This clearing/prep for resend was done as a result of the rollback call. HOWEVER, the delivery thread was still
 * in the process of sending messages to the client. It is therefore possible that a message could block on the
 * _unackedMap lock waiting for the visit to compelete so that it can add the new message to the unackedMap....
 * which is then cleared by the resend/rollback thread.
 *
 * This problem was encountered by the testSend2ThenRollback test.
 *
 * To try and increase the chance of the race condition occuring this test will send multiple messages so that the
 * delivery thread will be in progress while the rollback method is called. Hopefully this will cause the
 * deliveryTag to be lost
 */
public class ExtractResendAndRequeueTest extends TestCase
{

    UnacknowledgedMessageMapImpl _unacknowledgedMessageMap;
    private static final int INITIAL_MSG_COUNT = 10;
    private AMQQueue _queue = new MockAMQQueue(getName());
    private MessageStore _messageStore = new MemoryMessageStore();
    private LinkedList<QueueEntry> _referenceList = new LinkedList<QueueEntry>();

    @Override
    public void setUp() throws AMQException
    {
        _unacknowledgedMessageMap = new UnacknowledgedMessageMapImpl(100);

        long id = 0;
        SimpleQueueEntryList list = new SimpleQueueEntryList(_queue);

        // Add initial messages to QueueEntryList
        for (int count = 0; count < INITIAL_MSG_COUNT; count++)
        {
            AMQMessage msg = new MockAMQMessage(id);

            list.add(msg);

            //Increment ID;
            id++;
        }

        // Iterate through the QueueEntryList and add entries to unacknowledgeMessageMap and referecenList
        QueueEntryIterator queueEntries = list.iterator();
        while(queueEntries.advance())
        {
            QueueEntry entry = queueEntries.getNode();
            _unacknowledgedMessageMap.add(entry.getMessage().getMessageNumber(), entry);

            // Store the entry for future inspection
            _referenceList.add(entry);
        }

        assertEquals("Map does not contain correct setup data", INITIAL_MSG_COUNT, _unacknowledgedMessageMap.size());
    }

    /**
     * Helper method to create a new subscription and aquire the given messages.
     *
     * @param messageList The messages to aquire
     *
     * @return Subscription that performed the aquire
     */
    private Subscription createSubscriptionAndAquireMessages(LinkedList<QueueEntry> messageList)
    {
        Subscription subscription = new MockSubscription();

        // Aquire messages in subscription
        for (QueueEntry entry : messageList)
        {
            entry.acquire(subscription);
        }

        return subscription;
    }

    /**
     * This is the normal consumer rollback method.
     *
     * An active consumer that has aquired messages expects those messasges to be reset when rollback is requested.
     *
     * This test validates that the msgToResend map includes all the messages and none are left behind.
     *
     * @throws AMQException the visit interface throws this
     */
    public void testResend() throws AMQException
    {
        //We don't need the subscription object here.
        createSubscriptionAndAquireMessages(_referenceList);

        final Map<Long, QueueEntry> msgToRequeue = new LinkedHashMap<Long, QueueEntry>();
        final Map<Long, QueueEntry> msgToResend = new LinkedHashMap<Long, QueueEntry>();

        // requeueIfUnabletoResend doesn't matter here.
        _unacknowledgedMessageMap.visit(new ExtractResendAndRequeue(_unacknowledgedMessageMap, msgToRequeue,
                                                                    msgToResend, true, _messageStore));

        assertEquals("Message count for resend not correct.", INITIAL_MSG_COUNT, msgToResend.size());
        assertEquals("Message count for requeue not correct.", 0, msgToRequeue.size());
        assertEquals("Map was not emptied", 0, _unacknowledgedMessageMap.size());
    }

    /**
     * This is the normal consumer close method.
     *
     * When a consumer that has aquired messages expects closes the messages that it has aquired should be removed from
     * the unacknowledgeMap and placed in msgToRequeue
     *
     * This test validates that the msgToRequeue map includes all the messages and none are left behind.
     *
     * @throws AMQException the visit interface throws this
     */
    public void testRequeueDueToSubscriptionClosure() throws AMQException
    {
        Subscription subscription = createSubscriptionAndAquireMessages(_referenceList);

        // Close subscription
        subscription.close();

        final Map<Long, QueueEntry> msgToRequeue = new LinkedHashMap<Long, QueueEntry>();
        final Map<Long, QueueEntry> msgToResend = new LinkedHashMap<Long, QueueEntry>();

        // requeueIfUnabletoResend doesn't matter here.
        _unacknowledgedMessageMap.visit(new ExtractResendAndRequeue(_unacknowledgedMessageMap, msgToRequeue,
                                                                    msgToResend, true, _messageStore));

        assertEquals("Message count for resend not correct.", 0, msgToResend.size());
        assertEquals("Message count for requeue not correct.", INITIAL_MSG_COUNT, msgToRequeue.size());
        assertEquals("Map was not emptied", 0, _unacknowledgedMessageMap.size());
    }

    /**
     * If the subscription is null, due to message being retrieved via a GET, And we request that messages are requeued
     * requeueIfUnabletoResend(set to true) then all messages should be sent to the msgToRequeue map.
     *
     * @throws AMQException the visit interface throws this
     */

    public void testRequeueDueToMessageHavingNoConsumerTag() throws AMQException
    {
        final Map<Long, QueueEntry> msgToRequeue = new LinkedHashMap<Long, QueueEntry>();
        final Map<Long, QueueEntry> msgToResend = new LinkedHashMap<Long, QueueEntry>();

        // requeueIfUnabletoResend = true so all messages should go to msgToRequeue
        _unacknowledgedMessageMap.visit(new ExtractResendAndRequeue(_unacknowledgedMessageMap, msgToRequeue,
                                                                    msgToResend, true, _messageStore));

        assertEquals("Message count for resend not correct.", 0, msgToResend.size());
        assertEquals("Message count for requeue not correct.", INITIAL_MSG_COUNT, msgToRequeue.size());
        assertEquals("Map was not emptied", 0, _unacknowledgedMessageMap.size());
    }

    /**
     * If the subscription is null, due to message being retrieved via a GET, And we request that we don't
     * requeueIfUnabletoResend(set to false) then all messages should be dropped as we do not have a dead letter queue.
     *
     * @throws AMQException the visit interface throws this
     */

    public void testDrop() throws AMQException
    {
        final Map<Long, QueueEntry> msgToRequeue = new LinkedHashMap<Long, QueueEntry>();
        final Map<Long, QueueEntry> msgToResend = new LinkedHashMap<Long, QueueEntry>();

        // requeueIfUnabletoResend = false so all messages should be dropped all maps should be empty
        _unacknowledgedMessageMap.visit(new ExtractResendAndRequeue(_unacknowledgedMessageMap, msgToRequeue,
                                                                    msgToResend, false, _messageStore));

        assertEquals("Message count for resend not correct.", 0, msgToResend.size());
        assertEquals("Message count for requeue not correct.", 0, msgToRequeue.size());
        assertEquals("Map was not emptied", 0, _unacknowledgedMessageMap.size());


        for (QueueEntry entry : _referenceList)
        {
            assertTrue("Message was not discarded", entry.isDeleted());
        }

    }

    /**
     * If the subscription is null, due to message being retrieved via a GET, AND the queue upon which the message was
     * delivered has been deleted then it is not possible to requeue. Currently we simply discar the message but in the
     * future we may wish to dead letter the message.
     *
     * Validate that at the end of the visit all Maps are empty and all messages are marked as deleted
     *
     * @throws AMQException the visit interface throws this
     */
    public void testDiscard() throws AMQException
    {
        final Map<Long, QueueEntry> msgToRequeue = new LinkedHashMap<Long, QueueEntry>();
        final Map<Long, QueueEntry> msgToResend = new LinkedHashMap<Long, QueueEntry>();

        _queue.delete();

        // requeueIfUnabletoResend : value doesn't matter here as queue has been deleted
        _unacknowledgedMessageMap.visit(new ExtractResendAndRequeue(_unacknowledgedMessageMap, msgToRequeue,
                                                                    msgToResend, false, _messageStore));

        assertEquals("Message count for resend not correct.", 0, msgToResend.size());
        assertEquals("Message count for requeue not correct.", 0, msgToRequeue.size());
        assertEquals("Map was not emptied", 0, _unacknowledgedMessageMap.size());

        for (QueueEntry entry : _referenceList)
        {
            assertTrue("Message was not discarded", entry.isDeleted());
        }
    }

}
