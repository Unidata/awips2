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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.nio.ByteBuffer;

import org.apache.qpid.transport.codec.BBDecoder;
import org.apache.qpid.transport.codec.Decoder;

import org.apache.qpid.transport.Header;
import org.apache.qpid.transport.Method;
import org.apache.qpid.transport.ProtocolError;
import org.apache.qpid.transport.ProtocolEvent;
import org.apache.qpid.transport.ProtocolHeader;
import org.apache.qpid.transport.Receiver;
import org.apache.qpid.transport.SegmentType;
import org.apache.qpid.transport.Struct;


/**
 * Assembler
 *
 */

public class Assembler implements Receiver<NetworkEvent>, NetworkDelegate
{

    private final Receiver<ProtocolEvent> receiver;
    private final Map<Integer,List<Frame>> segments;
    private final Method[] incomplete;
    private final ThreadLocal<BBDecoder> decoder = new ThreadLocal<BBDecoder>()
    {
        public BBDecoder initialValue()
        {
            return new BBDecoder();
        }
    };

    public Assembler(Receiver<ProtocolEvent> receiver)
    {
        this.receiver = receiver;
        segments = new HashMap<Integer,List<Frame>>();
        incomplete = new Method[64*1024];
    }

    private int segmentKey(Frame frame)
    {
        return (frame.getTrack() + 1) * frame.getChannel();
    }

    private List<Frame> getSegment(Frame frame)
    {
        return segments.get(segmentKey(frame));
    }

    private void setSegment(Frame frame, List<Frame> segment)
    {
        int key = segmentKey(frame);
        if (segments.containsKey(key))
        {
            error(new ProtocolError(Frame.L2, "segment in progress: %s",
                                    frame));
        }
        segments.put(segmentKey(frame), segment);
    }

    private void clearSegment(Frame frame)
    {
        segments.remove(segmentKey(frame));
    }

    private void emit(int channel, ProtocolEvent event)
    {
        event.setChannel(channel);
        receiver.received(event);
    }

    public void received(NetworkEvent event)
    {
        event.delegate(this);
    }

    public void exception(Throwable t)
    {
        this.receiver.exception(t);
    }

    public void closed()
    {
        this.receiver.closed();
    }

    public void init(ProtocolHeader header)
    {
        emit(0, header);
    }

    public void error(ProtocolError error)
    {
        emit(0, error);
    }

    public void frame(Frame frame)
    {
        ByteBuffer segment;
        if (frame.isFirstFrame() && frame.isLastFrame())
        {
            segment = frame.getBody();
            assemble(frame, segment);
        }
        else
        {
            List<Frame> frames;
            if (frame.isFirstFrame())
            {
                frames = new ArrayList<Frame>();
                setSegment(frame, frames);
            }
            else
            {
                frames = getSegment(frame);
            }

            frames.add(frame);

            if (frame.isLastFrame())
            {
                clearSegment(frame);

                int size = 0;
                for (Frame f : frames)
                {
                    size += f.getSize();
                }
                segment = ByteBuffer.allocate(size);
                for (Frame f : frames)
                {
                    segment.put(f.getBody());
                }
                segment.flip();
                assemble(frame, segment);
            }
        }

    }

    private void assemble(Frame frame, ByteBuffer segment)
    {
        BBDecoder dec = decoder.get();
        dec.init(segment);

        int channel = frame.getChannel();
        Method command;

        switch (frame.getType())
        {
        case CONTROL:
            int controlType = dec.readUint16();
            Method control = Method.create(controlType);
            control.read(dec);
            emit(channel, control);
            break;
        case COMMAND:
            int commandType = dec.readUint16();
            // read in the session header, right now we don't use it
            int hdr = dec.readUint16();
            command = Method.create(commandType);
            command.setSync((0x0001 & hdr) != 0);
            command.read(dec);
            if (command.hasPayload())
            {
                incomplete[channel] = command;
            }
            else
            {
                emit(channel, command);
            }
            break;
        case HEADER:
            command = incomplete[channel];
            List<Struct> structs = new ArrayList(2);
            while (dec.hasRemaining())
            {
                structs.add(dec.readStruct32());
            }
            command.setHeader(new Header(structs));
            if (frame.isLastSegment())
            {
                incomplete[channel] = null;
                emit(channel, command);
            }
            break;
        case BODY:
            command = incomplete[channel];
            command.setBody(segment);
            incomplete[channel] = null;
            emit(channel, command);
            break;
        default:
            throw new IllegalStateException("unknown frame type: " + frame.getType());
        }
    }

}
