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
package org.apache.qpid.framing;

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.protocol.AMQVersionAwareProtocolSession;
import org.apache.qpid.AMQException;

public class HeartbeatBody implements AMQBody
{
    public static final byte TYPE = 8;
    public static AMQFrame FRAME = new HeartbeatBody().toFrame();

    public HeartbeatBody()
    {

    }

    public HeartbeatBody(ByteBuffer buffer, long size)
    {
        if(size > 0)
        {
            //allow other implementations to have a payload, but ignore it:
            buffer.skip((int) size);
        }
    }

    public byte getFrameType()
    {
        return TYPE;
    }

    public int getSize()
    {
        return 0;//heartbeats we generate have no payload
    }

    public void writePayload(ByteBuffer buffer)
    {
    }

    public void handle(final int channelId, final AMQVersionAwareProtocolSession session)
            throws AMQException
    {
        session.heartbeatBodyReceived(channelId, this);
    }

    protected void populateFromBuffer(ByteBuffer buffer, long size) throws AMQFrameDecodingException
    {
        if(size > 0)
        {
            //allow other implementations to have a payload, but ignore it:
            buffer.skip((int) size);
        }
    }

    public AMQFrame toFrame()
    {
        return new AMQFrame(0, this);
    }
}
