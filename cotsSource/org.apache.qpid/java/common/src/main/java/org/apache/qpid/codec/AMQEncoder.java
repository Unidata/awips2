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
package org.apache.qpid.codec;

import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolEncoder;
import org.apache.mina.filter.codec.ProtocolEncoderOutput;

import org.apache.qpid.framing.AMQDataBlockEncoder;

/**
 * AMQEncoder delegates encoding of AMQP to a data encoder.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Delegate AMQP encoding. <td> {@link AMQDataBlockEncoder}
 * </table>
 *
 * @todo This class just delegates to another, so seems to be pointless. Unless it is going to handle some
 *       responsibilities in the future, then drop it.
 */
public class AMQEncoder implements ProtocolEncoder
{
    /** The data encoder that is delegated to. */
    private AMQDataBlockEncoder _dataBlockEncoder = new AMQDataBlockEncoder();

    /**
     * Encodes AMQP.
     *
     * @param session The Mina session.
     * @param message The data object to encode.
     * @param out     The Mina writer to output the raw byte data to.
     *
     * @throws Exception If the data cannot be encoded for any reason.
     */
    public void encode(IoSession session, Object message, ProtocolEncoderOutput out) throws Exception
    {
        _dataBlockEncoder.encode(session, message, out);
    }

    /**
     * Does nothing. Called by Mina to allow this to clean up resources when it is no longer needed.
     *
     * @param session The Mina session.
     */
    public void dispose(IoSession session)
    { }
}
