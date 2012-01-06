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

namespace Apache.Qpid.Framing
{
    public abstract class AMQMethodBody : IBody
    {
        public const byte TYPE = 1;
        
        protected abstract uint BodySize
        {
            get;
        }

        protected abstract ushort Clazz
        {
            get;
        }

        protected abstract ushort Method
        {
            get;
        }

        protected abstract void WriteMethodPayload(ByteBuffer buffer);

        public byte BodyType
        {
            get
            {
                return TYPE;
            }
        }

        public uint Size
        {
            get
            {
                return (uint) (2 + 2 + BodySize);
            }
        }

        public void WritePayload(ByteBuffer buffer)
        {
            buffer.Put(Clazz);
            buffer.Put(Method);
            WriteMethodPayload(buffer);
        }

        /// <summary>
        /// Populates the method body by decoding the specified buffer
        /// </summary>
        /// <param name="buffer">The buffer to decode.</param>
        /// <exception cref="AMQFrameDecodingException">If the buffer cannot be decoded</exception>
        protected abstract void PopulateMethodBodyFromBuffer(ByteBuffer buffer);

        /// <summary>
        /// Populates this instance from a buffer of data.
        /// </summary>
        /// <param name="buffer">The buffer.</param>
        /// <param name="size">The size.</param>
        /// <exception cref="AMQFrameDecodingException">If the buffer contains data that cannot be decoded</exception>
        public void PopulateFromBuffer(ByteBuffer buffer, uint size)
        {
            PopulateMethodBodyFromBuffer(buffer);
        }

        public override string ToString()
        {
            return String.Format("{0}{{ Class: {1} Method: {2} }}", GetType().Name, Clazz, Method);
        }
    }
}
