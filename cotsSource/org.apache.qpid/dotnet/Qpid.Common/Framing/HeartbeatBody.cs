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
    public class HeartbeatBody : IBody
{
    public const byte TYPE = 8;
    public static AMQFrame FRAME = new HeartbeatBody().ToFrame();

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
            return 0;//heartbeats we generate have no payload
        }        
    }

    public void WritePayload(ByteBuffer buffer)
    {
    }

    public void PopulateFromBuffer(ByteBuffer buffer, uint size)
    {
        if (size > 0)
        {
            //allow other implementations to have a payload, but ignore it:
            buffer.Skip((int) size);
        }
    }

    public AMQFrame ToFrame()
    {
        return new AMQFrame(0, this);
    }
}
}
