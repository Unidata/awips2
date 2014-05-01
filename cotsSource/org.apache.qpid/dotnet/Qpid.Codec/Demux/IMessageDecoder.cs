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
using System;
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Codec.Demux
{
    public interface IMessageDecoder
    {
        /// <summary>
        /// Checks the specified buffer is decodable by this decoder.
        /// </summary>
        /// <param name="buffer">The buffer to read data from.</param>
        /// <returns>
        /// OK if this decoder can decode the specified buffer.
        /// NOT_OK if this decoder cannot decode the specified buffer.
        /// if more data is required to determine if the
        /// specified buffer is decodable ({@link #OK}) or not decodable
        /// {@link #NOT_OK}.</returns>             
        MessageDecoderResult Decodable(ByteBuffer buffer);

        /// <summary>
        /// Decodes binary or protocol-specific content into higher-level message objects.
        /// MINA invokes {@link #decode(IoSession, ByteBuffer, ProtocolDecoderOutput)}
        /// method with read data, and then the decoder implementation puts decoded
        /// messages into {@link ProtocolDecoderOutput}.
        /// </summary>
        /// <returns>
        /// {@link #OK} if you finished decoding messages successfully.
        /// {@link #NEED_DATA} if you need more data to finish decoding current message.
        /// {@link #NOT_OK} if you cannot decode current message due to protocol specification violation.
        /// </returns>
        /// <exception cref="Exception">if the read data violated protocol specification </exception>     
        MessageDecoderResult Decode(ByteBuffer buffer, IProtocolDecoderOutput output);
    }
}


