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
 * MessageCleanupException represents the failure to perform reference counting on messages correctly. This should not
 * happen, but there may be programming errors giving race conditions that cause the reference counting to go wrong.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Signals that the reference count of a message has gone below zero.
 * <tr><td> Indicates that a message store has lost a message which is still referenced.
 * </table>
 *
 * @todo Not an AMQP exception as no status code.
 *
 * @todo The race conditions leading to this error should be cleaned up, and a runtime exception used instead. If the
 *       message store loses messages, then something is seriously wrong and it would be sensible to terminate the
 *       broker. This may be disguising out of memory errors.
 */
public class MessageCleanupException extends AMQException
{
    public MessageCleanupException(long messageId, AMQException e)
    {
        super("Failed to cleanup message with id " + messageId, e);
    }

    public MessageCleanupException(String message)
    {
        super(message);
    }
}
