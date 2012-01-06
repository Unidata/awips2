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

import org.apache.qpid.AMQException;
import org.apache.qpid.server.message.ServerMessage;

public enum NotificationCheck
{

    MESSAGE_COUNT_ALERT
    {
        boolean notifyIfNecessary(ServerMessage msg, AMQQueue queue, QueueNotificationListener listener)
        {
            int msgCount;
            final long maximumMessageCount = queue.getMaximumMessageCount();
            if (maximumMessageCount!= 0 && (msgCount =  queue.getMessageCount()) >= maximumMessageCount)
            {
                listener.notifyClients(this, queue, msgCount + ": Maximum count on queue threshold ("+ maximumMessageCount +") breached.");
                return true;
            }
            return false;
        }
    },
    MESSAGE_SIZE_ALERT(true)
    {
        boolean notifyIfNecessary(ServerMessage msg, AMQQueue queue, QueueNotificationListener listener)
        {
            final long maximumMessageSize = queue.getMaximumMessageSize();
            if(maximumMessageSize != 0)
            {
                // Check for threshold message size
                long messageSize;
                messageSize = (msg == null) ? 0 : msg.getSize();


                if (messageSize >= maximumMessageSize)
                {
                    listener.notifyClients(this, queue, messageSize + "b : Maximum message size threshold ("+ maximumMessageSize +") breached. [Message ID=" + msg.getMessageNumber() + "]");
                    return true;
                }
            }
            return false;
        }

    },
    QUEUE_DEPTH_ALERT
    {
        boolean notifyIfNecessary(ServerMessage msg, AMQQueue queue, QueueNotificationListener listener)
        {
            // Check for threshold queue depth in bytes
            final long maximumQueueDepth = queue.getMaximumQueueDepth();

            if(maximumQueueDepth != 0)
            {
                final long queueDepth = queue.getQueueDepth();

                if (queueDepth >= maximumQueueDepth)
                {
                    listener.notifyClients(this, queue, (queueDepth>>10) + "Kb : Maximum queue depth threshold ("+(maximumQueueDepth>>10)+"Kb) breached.");
                    return true;
                }
            }
            return false;
        }

    },
    MESSAGE_AGE_ALERT
    {
        boolean notifyIfNecessary(ServerMessage msg, AMQQueue queue, QueueNotificationListener listener)
        {

            final long maxMessageAge = queue.getMaximumMessageAge();
            if(maxMessageAge != 0)
            {
                final long currentTime = System.currentTimeMillis();
                final long thresholdTime = currentTime - maxMessageAge;
                final long firstArrivalTime = queue.getOldestMessageArrivalTime();

                if(firstArrivalTime < thresholdTime)
                {
                    long oldestAge = currentTime - firstArrivalTime;
                    listener.notifyClients(this, queue, (oldestAge/1000) + "s : Maximum age on queue threshold ("+(maxMessageAge /1000)+"s) breached.");

                    return true;
                }
            }
            return false;
                    
        }

    }
    ;

    private final boolean _messageSpecific;

    NotificationCheck()
    {
        this(false);
    }

    NotificationCheck(boolean messageSpecific)
    {
        _messageSpecific = messageSpecific;
    }

    public boolean isMessageSpecific()
    {
        return _messageSpecific;
    }

    abstract boolean notifyIfNecessary(ServerMessage msg, AMQQueue queue, QueueNotificationListener listener);

}
