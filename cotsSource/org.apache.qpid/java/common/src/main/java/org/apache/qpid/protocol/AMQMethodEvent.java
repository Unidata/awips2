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
package org.apache.qpid.protocol;

import org.apache.qpid.framing.AMQMethodBody;

/**
 * AMQMethodEvent encapsulates an AMQP method call, and the channel on which that method call occurred.
 *
 * <p/>Supplies the:
 * <ul>
 * <li>channel id</li>
 * <li>protocol method</li>
 * </ul>
 *
 * <p/>As the event contains the context in which it occurred, event listeners do not need to be statefull.
 * to listeners. Events are often handled by {@link AMQMethodListener}s.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Encapsulate an AMQP method call and the channel as the context for the method call.
 * </table>
 */
public class AMQMethodEvent<M extends AMQMethodBody>
{
    /** Holds the method call. */
    private final M _method;

    /** Holds the channel handle for the method call. */
    private final int _channelId;

    /**
     * Creates a method event to encasulate a method call and channel.
     *
     * @param channelId The channel on which the method call occurred.
     * @param method    The method call.
     */
    public AMQMethodEvent(int channelId, M method)
    {
        _channelId = channelId;
        _method = method;
    }

    /**
     * Gets the method call.
     *
     * @return The method call.
     */
    public M getMethod()
    {
        return _method;
    }

    /**
     * Gets the channel handle for the method call.
     *
     * @return The channel handle for the method call.
     */
    public int getChannelId()
    {
        return _channelId;
    }

    /**
     * Prints the method call as a string, mainly for debugging purposes.
     *
     * @return The method call as a string, mainly for debugging purposes.
     */
    public String toString()
    {
        StringBuilder buf = new StringBuilder("Method event: ");
        buf.append("\nChannel id: ").append(_channelId);
        buf.append("\nMethod: ").append(_method);

        return buf.toString();
    }
}
