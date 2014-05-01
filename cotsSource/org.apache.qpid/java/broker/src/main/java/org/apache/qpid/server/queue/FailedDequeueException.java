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

/**
 * Signals that the dequeue of a message from a queue failed.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Indicates the a message could not be dequeued from a queue.
 * <tr><td>
 * </table>
 *
 * @todo Not an AMQP exception as no status code.
 *
 * @todo Happens as a consequence of a message store failure, or reference counting error. Both of which migh become
 *       runtime exceptions, as unrecoverable conditions? In which case this one might be dropped too.
 */
public class FailedDequeueException extends AMQException
{
    public FailedDequeueException(String queue)
    {
        super("Failed to dequeue message from " + queue);
    }

    public FailedDequeueException(String queue, AMQException e)
    {
        super("Failed to dequeue message from " + queue, e);
    }
}
