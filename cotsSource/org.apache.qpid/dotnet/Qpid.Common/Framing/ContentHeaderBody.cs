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
    public class ContentHeaderBody : IBody
    {
        public static readonly byte TYPE = 2;

        public ushort ClassId;

        public ushort Weight;
        
        public ulong BodySize;

        /** must never be null */
        public IContentHeaderProperties Properties;

        public ContentHeaderBody()
        {
        }

        public ContentHeaderBody(IContentHeaderProperties props, ushort classId)
        {
            Properties = props;
            ClassId = classId;
        }

        public ContentHeaderBody(ushort classId, ushort weight, IContentHeaderProperties props, uint bodySize)
            : this(props, classId)
        {            
            Weight = weight;
            BodySize = bodySize;
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
                return (2 + 2 + 8 + 2 + Properties.PropertyListSize);
            }
        }

        public void WritePayload(ByteBuffer buffer)
        {
            buffer.Put(ClassId);
            buffer.Put(Weight);
            buffer.Put(BodySize);
            buffer.Put(Properties.PropertyFlags);
            Properties.WritePropertyListPayload(buffer);
        }

        public void PopulateFromBuffer(ByteBuffer buffer, uint size)
        {     
            ClassId = buffer.GetUInt16();
            Weight = buffer.GetUInt16();
            BodySize = buffer.GetUInt64();
            ushort propertyFlags = buffer.GetUInt16();
            ContentHeaderPropertiesFactory factory = ContentHeaderPropertiesFactory.GetInstance();
            Properties = factory.CreateContentHeaderProperties(ClassId, propertyFlags, buffer);    
        }

        #endregion

        public static AMQFrame CreateAMQFrame(ushort channelId, ushort classId, ushort weight, BasicContentHeaderProperties properties,
                                              uint bodySize)
        {
            AMQFrame frame = new AMQFrame();
            frame.Channel = channelId;
            frame.BodyFrame = new ContentHeaderBody(classId, weight, properties, bodySize);
            return frame;
        }

        public static AMQFrame CreateAMQFrame(ushort channelId, ContentHeaderBody body)
        {
            AMQFrame frame = new AMQFrame();
            frame.Channel = channelId;
            frame.BodyFrame = body;
            return frame;
        }

        public override string ToString()
        {
            return String.Format("ContentHeaderBody: ClassId {0}, Weight {1}, BodySize {2}, Properties {3}", ClassId, Weight,
                                 BodySize, Properties);
        }
    }
}
