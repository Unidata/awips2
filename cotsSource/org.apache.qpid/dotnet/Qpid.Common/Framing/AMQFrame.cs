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
    public class AMQFrame : IDataBlock
    {
        private ushort _channel;

        private IBody _bodyFrame;

        public AMQFrame()
        {            
        }
        
        public AMQFrame(ushort channel, IBody bodyFrame)
        {
            _channel = channel;
            _bodyFrame = bodyFrame;
        }
        
        public ushort Channel
        {
            get
            {
                return _channel;
            }
            set
            {
                _channel = value;
            }
        }

        public IBody BodyFrame
        {
            get
            {
                return _bodyFrame;
            }
            set
            {
                _bodyFrame = value;
            }
        }

        #region IDataBlock Members

        public uint Size
        {
            get
            {
                return (uint) (1 + 2 + 4 + _bodyFrame.Size + 1);
            }
        }

        public void WritePayload(ByteBuffer buffer)
        {
            buffer.Put(_bodyFrame.BodyType);            
            // TODO: how does channel get populated
            buffer.Put(_channel);
            buffer.Put(_bodyFrame.Size);
            _bodyFrame.WritePayload(buffer);
            buffer.Put((byte) 0xCE);
        }

        #endregion

        /// <summary>
        /// Populates the frame instance data from the supplied buffer.
        /// </summary>
        /// <param name="buffer">The buffer.</param>
        /// <param name="channel">The channel.</param>
        /// <param name="bodySize">Size of the body in bytes</param>
        /// <param name="bodyFactory">The body factory.</param>
        /// <exception cref="AMQFrameDecodingException">Thrown if the buffer cannot be decoded</exception>
        public void PopulateFromBuffer(ByteBuffer buffer, ushort channel, uint bodySize, IBodyFactory bodyFactory)            
        {
            _channel = channel;
            _bodyFrame = bodyFactory.CreateBody(buffer);
            _bodyFrame.PopulateFromBuffer(buffer, bodySize);
        }

        public override string ToString()
        {
            return "Frame channelId: " + _channel + ", bodyFrame: " + _bodyFrame.ToString();
        }
    }
}
