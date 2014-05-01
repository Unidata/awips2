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
    public class ContentBody : IBody
    {
        public const byte TYPE = 3;

        private ByteBuffer _payload;
       
        public ByteBuffer Payload
        {
           get { return _payload; }
        }

       public ContentBody()
       {
       }
       public ContentBody(ByteBuffer payload)
       {
          PopulateFromBuffer(payload, (uint)payload.Remaining);
       }
       public ContentBody(ByteBuffer payload, uint length)
       {
          PopulateFromBuffer(payload, length);
       }

        #region IBody Members

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
                return (ushort)(Payload == null ? 0 : Payload.Remaining);
            }
        }

        public void WritePayload(ByteBuffer buffer)
        {
            if (Payload != null)
            {
                buffer.Put(Payload);
                Payload.Rewind();
            }
        }

        public void PopulateFromBuffer(ByteBuffer buffer, uint size)
        {
            if (size > 0)
            {
                _payload = buffer.Slice();
                _payload.Limit = (int)size;
                buffer.Skip((int)size);
            }
        }

        #endregion

        public static AMQFrame CreateAMQFrame(ushort channelId, ContentBody body)
        {
            AMQFrame frame = new AMQFrame();
            frame.Channel = channelId;
            frame.BodyFrame = body;
            return frame;
        }

        public override string ToString()
        {
            return string.Format("ContentBody [ Size: {0} ]", Size);
        }
    }
}
