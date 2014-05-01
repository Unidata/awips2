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
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Framing
{
    /// <summary>
    /// An IBody is contained within a top level frame. As such, it is not en/decodable on its own but
    /// is decoded as a step within a the overall en/decoding process.
    /// </summary>
    public interface IBody
    {
        /// <summary>
        /// Gets the type. See RFC 006 for the meaning of "type" in this context.
        /// </summary>
        /// <value>The type.</value>
        byte BodyType
        {
            get;
        }
    
        /// <summary>
        /// Get the size of the body
        /// </summary>
        /// <value>The size in bytes.</value>         
        uint Size
        {
            get;
        }

        /// <summary>
        /// Writes this instance to a buffer.
        /// </summary>
        /// <param name="buffer">The buffer.</param>
        void WritePayload(ByteBuffer buffer);
    
        /// <summary>
        /// Populates this instance from a buffer of data.
        /// </summary>
        /// <param name="buffer">The buffer.</param>
        /// <param name="size">The size.</param>
        /// <exception cref="AMQFrameDecodingException">If the buffer contains data that cannot be decoded</exception>
        void PopulateFromBuffer(ByteBuffer buffer, uint size);
    }
}
