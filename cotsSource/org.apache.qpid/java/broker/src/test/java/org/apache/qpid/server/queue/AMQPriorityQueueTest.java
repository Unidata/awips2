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

import org.apache.qpid.AMQException;
import org.apache.qpid.server.message.AMQMessage;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.FieldTable;
import junit.framework.AssertionFailedError;

public class AMQPriorityQueueTest extends SimpleAMQQueueTest
{

    @Override
    protected void setUp() throws Exception
    {
        _arguments = new FieldTable();
        _arguments.put(AMQQueueFactory.X_QPID_PRIORITIES, 3);
        super.setUp();
    }

    public void testPriorityOrdering() throws AMQException, InterruptedException
    {

        // Enqueue messages in order
        _queue.enqueue(createMessage(1L, (byte) 10));
        _queue.enqueue(createMessage(2L, (byte) 4));
        _queue.enqueue(createMessage(3L, (byte) 0));

        // Enqueue messages in reverse order
        _queue.enqueue(createMessage(4L, (byte) 0));
        _queue.enqueue(createMessage(5L, (byte) 4));
        _queue.enqueue(createMessage(6L, (byte) 10));

        // Enqueue messages out of order
        _queue.enqueue(createMessage(7L, (byte) 4));
        _queue.enqueue(createMessage(8L, (byte) 10));
        _queue.enqueue(createMessage(9L, (byte) 0));

        // Register subscriber
        _queue.registerSubscription(_subscription, false);
        Thread.sleep(150);

        ArrayList<QueueEntry> msgs = _subscription.getMessages();
        try
        {
            assertEquals(new Long(1L), msgs.get(0).getMessage().getMessageNumber());
            assertEquals(new Long(6L), msgs.get(1).getMessage().getMessageNumber());
            assertEquals(new Long(8L), msgs.get(2).getMessage().getMessageNumber());

            assertEquals(new Long(2L), msgs.get(3).getMessage().getMessageNumber());
            assertEquals(new Long(5L), msgs.get(4).getMessage().getMessageNumber());
            assertEquals(new Long(7L), msgs.get(5).getMessage().getMessageNumber());

            assertEquals(new Long(3L), msgs.get(6).getMessage().getMessageNumber());
            assertEquals(new Long(4L), msgs.get(7).getMessage().getMessageNumber());
            assertEquals(new Long(9L), msgs.get(8).getMessage().getMessageNumber());
        }
        catch (AssertionFailedError afe)
        {
            // Show message order on failure.
            int index = 1;
            for (QueueEntry qe : msgs)
            {
                System.err.println(index + ":" + qe.getMessage().getMessageNumber());
                index++;
            }

            throw afe;
        }

    }

    protected AMQMessage createMessage(Long id, byte i) throws AMQException
    {
        AMQMessage msg = super.createMessage(id);
        BasicContentHeaderProperties props = new BasicContentHeaderProperties();
        props.setPriority(i);
        msg.getContentHeaderBody().properties = props;
        return msg;
    }

    protected AMQMessage createMessage(Long id) throws AMQException
    {
        return createMessage(id, (byte) 0);
    }

}
