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
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolDecoderOutput;

import org.apache.qpid.protocol.AMQVersionAwareProtocolSession;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AMQDataBlockDecoder
{
    private static final String SESSION_METHOD_BODY_FACTORY = "QPID_SESSION_METHOD_BODY_FACTORY";

    private static final BodyFactory[] _bodiesSupported = new BodyFactory[Byte.MAX_VALUE];

    static
    {
        _bodiesSupported[ContentHeaderBody.TYPE] = ContentHeaderBodyFactory.getInstance();
        _bodiesSupported[ContentBody.TYPE] = ContentBodyFactory.getInstance();
        _bodiesSupported[HeartbeatBody.TYPE] = new HeartbeatBodyFactory();
    }

    Logger _logger = LoggerFactory.getLogger(AMQDataBlockDecoder.class);

    public AMQDataBlockDecoder()
    { }

    public boolean decodable(java.nio.ByteBuffer in) throws AMQFrameDecodingException
    {
        final int remainingAfterAttributes = in.remaining() - (1 + 2 + 4 + 1);
        // type, channel, body length and end byte
        if (remainingAfterAttributes < 0)
        {
            return false;
        }

        in.position(in.position() + 1 + 2);
        // Get an unsigned int, lifted from MINA ByteBuffer getUnsignedInt() 
        final long bodySize = in.getInt() & 0xffffffffL; 

        return (remainingAfterAttributes >= bodySize);

    }

    public AMQFrame createAndPopulateFrame(AMQMethodBodyFactory methodBodyFactory, ByteBuffer in)
        throws AMQFrameDecodingException, AMQProtocolVersionException
    {
        final byte type = in.get();

        BodyFactory bodyFactory;
        if (type == AMQMethodBody.TYPE)
        {
            bodyFactory = methodBodyFactory;
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

        return frame;
    }

    public void decode(IoSession session, ByteBuffer in, ProtocolDecoderOutput out) throws Exception
    {
        AMQMethodBodyFactory bodyFactory = (AMQMethodBodyFactory) session.getAttribute(SESSION_METHOD_BODY_FACTORY);
        if (bodyFactory == null)
        {
            AMQVersionAwareProtocolSession protocolSession = (AMQVersionAwareProtocolSession) session.getAttachment();
            bodyFactory = new AMQMethodBodyFactory(protocolSession);
            session.setAttribute(SESSION_METHOD_BODY_FACTORY, bodyFactory);
        }
        
        out.write(createAndPopulateFrame(bodyFactory, in));
    }

    public boolean decodable(ByteBuffer msg) throws AMQFrameDecodingException
    {
        return decodable(msg.buf());
    }

    public AMQDataBlock createAndPopulateFrame(AMQMethodBodyFactory factory, java.nio.ByteBuffer msg) throws AMQProtocolVersionException, AMQFrameDecodingException
    {
        return createAndPopulateFrame(factory, ByteBuffer.wrap(msg));
    }
}
