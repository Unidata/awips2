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
package org.apache.qpid.test.framework;

import org.apache.log4j.Logger;

import javax.jms.Message;
import javax.jms.MessageListener;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * MessageMonitor is used to record information about messages received. This will provide methods to check various
 * properties, such as the type, number and content of messages received in order to verify the correct behaviour of
 * tests.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Count incoming messages.
 * <tr><td> Record time ellapsed since the arrival of the first message.
 * <tr><td> Reset all counts and timings.
 * </table>
 */
public class MessageMonitor implements MessageListener
{
    /** Used for debugging. */
    private final Logger log = Logger.getLogger(MessageMonitor.class);

    /** Holds the count of messages received since the last query. */
    protected AtomicInteger numMessages = new AtomicInteger();

    /** Holds the time of arrival of the first message. */
    protected Long firstMessageTime = null;

    /**
     * Handles received messages. Does Nothing.
     *
     * @param message The message. Ignored.
     */
    public void onMessage(Message message)
    {
        // log.debug("public void onMessage(Message message): called");

        numMessages.getAndIncrement();
    }

    /**
     * Gets the count of messages.
     *
     * @return The count of messages.
     */
    public int getNumMessage()
    {
        if (firstMessageTime == null)
        {
            firstMessageTime = System.nanoTime();
        }

        return numMessages.get();
    }

    /**
     * Gets the time elapsed since the first message arrived, in nanos, or zero if no messages have arrived yet.
     *
     * @return The time elapsed since the first message arrived, in nanos, or zero if no messages have arrived yet.
     */
    public long getTime()
    {
        if (firstMessageTime != null)
        {
            return System.nanoTime() - firstMessageTime;
        }
        else
        {
            return 0L;
        }
    }

    /**
     * Resets the message count and timer to zero.
     */
    public void reset()
    {
        numMessages.set(0);
        firstMessageTime = null;
    }
}
