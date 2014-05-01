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
using System.Collections.Generic;
using System.IO;
using org.apache.qpid.transport.codec;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport.network
{
    /// <summary> 
    /// Assembler
    /// </summary>
    public delegate void Processor(INetworkDelegate ndelegate);

    public class Assembler : INetworkDelegate, IReceiver<ReceivedPayload<IProtocolEvent>>
    {
        private static readonly Logger log = Logger.Get(typeof (Assembler));
        private readonly Dictionary<int, List<byte[]>> segments;
        private readonly Method[] incomplete;
        [ThreadStatic] static MSDecoder _decoder;
        private readonly Object m_objectLock = new object();

        // the event raised when a buffer is read from the wire        
        public event EventHandler<ReceivedPayload<IProtocolEvent>> ReceivedEvent;
        public event EventHandler Closed;


        // Not in use : 
        public event EventHandler<ExceptionArgs> Exception;

        event EventHandler<ReceivedPayload<IProtocolEvent>> IReceiver<ReceivedPayload<IProtocolEvent>>.Received
        {
            add
            {
                lock (m_objectLock)
                {
                    ReceivedEvent += value;
                }
            }
            remove
            {
                lock (m_objectLock)
                {
                    ReceivedEvent -= value;
                }
            }
        }

        public Assembler()
        {
            segments = new Dictionary<int, List<byte[]>>();
            incomplete = new Method[64*1024];
        }

        // Invoked when a network event is received
        public void On_ReceivedEvent(object sender, ReceivedPayload<INetworkEvent> payload)
        {
            payload.Payload.ProcessNetworkEvent(this);
        }

        #region Interface INetworkDelegate

        public void Init(ProtocolHeader header)
        {
            Emit(0, header);
        }

        public void Error(ProtocolError error)
        {
            Emit(0, error);
        }

        public void Frame(Frame frame)
        {
            MemoryStream segment;
            if (frame.IsFirstFrame() && frame.IsLastFrame())
            {                
                byte[] tmp = new byte[frame.BodySize];
                frame.Body.Read(tmp, 0, tmp.Length);
                segment = new MemoryStream();
                BinaryWriter w = new BinaryWriter(segment);
                w.Write(tmp);
                Assemble(frame, new MemoryStream(tmp));
            }
            else
            {
                List<byte[]> frames;
                if (frame.IsFirstFrame())
                {
                    frames = new List<byte[]>();
                    SetSegment(frame, frames);
                }
                else
                {
                    frames = GetSegment(frame);
                }
                byte[] tmp = new byte[frame.BodySize];
                frame.Body.Read(tmp, 0, tmp.Length);
                frames.Add(tmp);

                if (frame.IsLastFrame())
                {
                    ClearSegment(frame);
                    segment = new MemoryStream();
                    BinaryWriter w = new BinaryWriter(segment);
                    foreach (byte[] f in frames)
                    {
                        w.Write(f);
                    }
                    Assemble(frame, segment);
                }
            }
        }

        #endregion

        #region Private Support Functions


        private MSDecoder GetDecoder()
        {
            if( _decoder == null )
            {
                _decoder = new MSDecoder();
            }
            return _decoder;
        }

        private void Assemble(Frame frame, MemoryStream segment)
        {
            MSDecoder decoder = GetDecoder();
            decoder.Init(segment);
            int channel = frame.Channel;
            Method command;
            switch (frame.Type)
            {
                case SegmentType.CONTROL:
                    int controlType = decoder.ReadUint16();                    
                    Method control = Method.Create(controlType);
                    control.Read(decoder);
                    Emit(channel, control);
                    break;
                case SegmentType.COMMAND:
                    int commandType = decoder.ReadUint16();
                     // read in the session header, right now we don't use it
                    decoder.ReadUint16();
                    command = Method.Create(commandType);
                    command.Read(decoder);
                    if (command.HasPayload())
                    {
                        incomplete[channel] = command;
                    }
                    else
                    {
                        Emit(channel, command);
                    }
                    break;
                case SegmentType.HEADER:
                    command = incomplete[channel];
                    List<Struct> structs = new List<Struct>();
                    while (decoder.HasRemaining())                    
                    {
                        structs.Add(decoder.ReadStruct32());
                    }
                    command.Header = new Header(structs);
                    if (frame.IsLastSegment())
                    {
                        incomplete[channel] = null;
                        Emit(channel, command);
                    }
                    break;
                case SegmentType.BODY:
                    command = incomplete[channel];                  
                    segment.Seek(0, SeekOrigin.Begin);
                    command.Body = segment;
                    incomplete[channel] = null;
                    Emit(channel, command);
                    break;
                default:
                    throw new Exception("unknown frame type: " + frame.Type);
            }
        }

        private int SegmentKey(Frame frame)
        {
            return (frame.Track + 1)*frame.Channel;
        }

        private List<byte[]> GetSegment(Frame frame)
        {
            return segments[SegmentKey(frame)];
        }

        private void SetSegment(Frame frame, List<byte[]> segment)
        {
            int key = SegmentKey(frame);
            if (segments.ContainsKey(key))
            {
                Error(new ProtocolError(network.Frame.L2, "segment in progress: %s",
                                        frame));
            }
            segments.Add(SegmentKey(frame), segment);            
        }

        private void ClearSegment(Frame frame)
        {
            segments.Remove(SegmentKey(frame));
        }

        // Emit a protocol event 
        private void Emit(int channel, IProtocolEvent protevent)
        {
            protevent.Channel = channel;
            log.Debug("Assembler: protocol event:", protevent);
            ReceivedPayload<IProtocolEvent> payload = new ReceivedPayload<IProtocolEvent>();
            payload.Payload = protevent;

            if (protevent is ConnectionCloseOk)
            {
                if (Closed != null)
                    Closed(this, EventArgs.Empty);
            }
            else
            {
                if (ReceivedEvent != null)
                    ReceivedEvent(this, payload);
                else
                    log.Debug("No listener for event: {0}", protevent);
            }
        }

        #endregion
    }
}