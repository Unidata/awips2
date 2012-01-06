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

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQDataBlock;

/**
 * UnknnownMessageTypeException represents a failure when Mina passes an unexpected frame type.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represents failure to cast a frame to its expected type.
 * </table>
 *
 * @todo Not an AMQP exception as no status code.
 *
 * @todo Seems like this exception was created to handle an unsafe type cast that will never happen in practice. Would
 *       be better just to leave that as a ClassCastException. However, check the framing layer catches this error
 *       first.
 */
public class UnknnownMessageTypeException extends AMQException
{
    public UnknnownMessageTypeException(AMQDataBlock message)
    {
        super("Unknown message type: " + message.getClass().getName() + ": " + message);
    }
}
