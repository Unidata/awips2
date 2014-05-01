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
package org.apache.qpid.transport.network;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import org.apache.qpid.transport.ProtocolError;
import org.apache.qpid.transport.ProtocolHeader;
import org.apache.qpid.transport.Receiver;
import org.apache.qpid.transport.SegmentType;

import static org.apache.qpid.transport.util.Functions.*;

import static org.apache.qpid.transport.network.InputHandler.State.*;


/**
 * InputHandler
 *
 * @author Rafael H. Schloming
 */

public class InputHandler implements Receiver<ByteBuffer>
{

    public enum State
    {
        PROTO_HDR,
        FRAME_HDR,
        FRAME_BODY,
        ERROR;
    }

    private final Receiver<NetworkEvent> receiver;
    private State state;
    private ByteBuffer input = null;
    private int needed;

    private byte flags;
    private SegmentType type;
    private byte track;
    private int channel;

    public InputHandler(Receiver<NetworkEvent> receiver, State state)
    {
        this.receiver = receiver;
        this.state = state;

        switch (state)
        {
        case PROTO_HDR:
            needed = 8;
            break;
        case FRAME_HDR:
            needed = Frame.HEADER_SIZE;
            break;
        }
    }

    public InputHandler(Receiver<NetworkEvent> receiver)
    {
        this(receiver, PROTO_HDR);
    }

    private void error(String fmt, Object ... args)
    {
        receiver.received(new ProtocolError(Frame.L1, fmt, args));
    }

    public void received(ByteBuffer buf)
    {
        int limit = buf.limit();
        int remaining = buf.remaining();
        while (remaining > 0)
        {
            if (remaining >= needed)
            {
                int consumed = needed;
                int pos = buf.position();
                if (input == null)
                {
                    buf.limit(pos + needed);
                    input = buf;
                    state = next(pos);
                    buf.limit(limit);
                    buf.position(pos + consumed);
                }
                else
                {
                    buf.limit(pos + needed);
                    input.put(buf);
                    buf.limit(limit);
                    input.flip();
                    state = next(0);
                }

                remaining -= consumed;
                input = null;
            }
            else
            {
                if (input == null)
                {
                    input = ByteBuffer.allocate(needed);
                }
                input.put(buf);
                needed -= remaining;
                remaining = 0;
            }
        }
    }

    private State next(int pos)
    {
        input.order(ByteOrder.BIG_ENDIAN);

        switch (state) {
        case PROTO_HDR:
            if (input.get(pos) != 'A' &&
                input.get(pos + 1) != 'M' &&
                input.get(pos + 2) != 'Q' &&
                input.get(pos + 3) != 'P')
            {
                error("bad protocol header: %s", str(input));
                return ERROR;
            }

            byte protoClass = input.get(pos + 4);
            byte instance = input.get(pos + 5);
            byte major = input.get(pos + 6);
            byte minor = input.get(pos + 7);
            receiver.received(new ProtocolHeader(protoClass, instance, major, minor));
            needed = Frame.HEADER_SIZE;
            return FRAME_HDR;
        case FRAME_HDR:
            flags = input.get(pos);
            type = SegmentType.get(input.get(pos + 1));
            int size = (0xFFFF & input.getShort(pos + 2));
            size -= Frame.HEADER_SIZE;
            if (size < 0 || size > (64*1024 - 12))
            {
                error("bad frame size: %d", size);
                return ERROR;
            }
            byte b = input.get(pos + 5);
            if ((b & 0xF0) != 0) {
                error("non-zero reserved bits in upper nibble of " +
                      "frame header byte 5: '%x'", b);
                return ERROR;
            } else {
                track = (byte) (b & 0xF);
            }
            channel = (0xFFFF & input.getShort(pos + 6));
            if (size == 0)
            {
                Frame frame = new Frame(flags, type, track, channel, ByteBuffer.allocate(0));
                receiver.received(frame);
                needed = Frame.HEADER_SIZE;
                return FRAME_HDR;
            }
            else
            {
                needed = size;
                return FRAME_BODY;
            }
        case FRAME_BODY:
            Frame frame = new Frame(flags, type, track, channel, input.slice());
            receiver.received(frame);
            needed = Frame.HEADER_SIZE;
            return FRAME_HDR;
        default:
            throw new IllegalStateException();
        }
    }

    public void exception(Throwable t)
    {
        receiver.exception(t);
    }

    public void closed()
    {
        receiver.closed();
    }

}
