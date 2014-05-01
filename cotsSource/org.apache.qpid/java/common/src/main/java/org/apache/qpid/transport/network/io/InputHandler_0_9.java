package org.apache.qpid.transport.network.io;
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


import java.nio.ByteBuffer;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQFrame;
import org.apache.qpid.framing.AMQFrameDecodingException;
import org.apache.qpid.framing.AMQMethodBody;
import org.apache.qpid.framing.AMQMethodBodyFactory;
import org.apache.qpid.framing.BodyFactory;
import org.apache.qpid.framing.ContentBody;
import org.apache.qpid.framing.ContentBodyFactory;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.ContentHeaderBodyFactory;
import org.apache.qpid.framing.HeartbeatBody;
import org.apache.qpid.framing.HeartbeatBodyFactory;
import org.apache.qpid.framing.MethodRegistry;
import org.apache.qpid.protocol.AMQVersionAwareProtocolSession;
import org.apache.qpid.transport.Receiver;

public class InputHandler_0_9 implements Receiver<ByteBuffer>
{

    private AMQVersionAwareProtocolSession _session;
    private MethodRegistry _registry;
    private BodyFactory bodyFactory;
    private static final BodyFactory[] _bodiesSupported = new BodyFactory[Byte.MAX_VALUE];

    static
    {
        _bodiesSupported[ContentHeaderBody.TYPE] = ContentHeaderBodyFactory.getInstance();
        _bodiesSupported[ContentBody.TYPE] = ContentBodyFactory.getInstance();
        _bodiesSupported[HeartbeatBody.TYPE] = new HeartbeatBodyFactory();
    }
    
    public InputHandler_0_9(AMQVersionAwareProtocolSession session)
    {
        _session = session;
        _registry = _session.getMethodRegistry();
    }

    public void closed()
    {
        // AS FIXME: implement
    }

    public void exception(Throwable t)
    {
        // TODO: propogate exception to things
        t.printStackTrace();
    }

    public void received(ByteBuffer buf)
    {
        org.apache.mina.common.ByteBuffer in = org.apache.mina.common.ByteBuffer.wrap(buf);
        try
        {
            final byte type = in.get();
            if (type == AMQMethodBody.TYPE)
            {
                bodyFactory = new AMQMethodBodyFactory(_session);
            }
            else
            {
                bodyFactory = _bodiesSupported[type];
            }

            if (bodyFactory == null)
            {
                throw new AMQFrameDecodingException(null, "Unsupported frame type: " + type, null);
            }

            final int channel = in.getUnsignedShort();
            final long bodySize = in.getUnsignedInt();

            // bodySize can be zero
            if ((channel < 0) || (bodySize < 0))
            {
                throw new AMQFrameDecodingException(null, "Undecodable frame: type = " + type + " channel = " + channel
                    + " bodySize = " + bodySize, null);
            }

            AMQFrame frame = new AMQFrame(in, channel, bodySize, bodyFactory);

            byte marker = in.get();
            if ((marker & 0xFF) != 0xCE)
            {
                throw new AMQFrameDecodingException(null, "End of frame marker not found. Read " + marker + " length=" + bodySize
                    + " type=" + type, null);
            }

            try
            {
                frame.getBodyFrame().handle(frame.getChannel(), _session);
            }
            catch (AMQException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        catch (AMQFrameDecodingException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}
