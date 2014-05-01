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
    /// A data block represents something that has a size in bytes and the ability to write itself to a byte
    /// buffer (similar to a byte array). It represents "top level" frames in the protocol specification.
    /// </summary>
    public interface IDataBlock : IEncodableAMQDataBlock
    {
        /// <summary>
        /// Get the size of buffer needed to store the byte representation of this
        /// frame.
        /// </summary>
        /// <returns>size in bytes</returns>        
        uint Size
        {
            get;
        }

        /// <summary>
        /// Writes the datablock to the specified buffer.
        /// </summary>
        /// <param name="buffer">The buffer to write to. Must be the correct size.</param>
        void WritePayload(ByteBuffer buffer);
    }
}
