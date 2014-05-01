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
using System.IO;
using System.Text;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport.network
{
    /// <summary> 
    /// InputHandler
    /// </summary>
    public sealed class InputHandler : IReceiver<ReceivedPayload<INetworkEvent>>
    {
        public enum State
        {
            PROTO_HDR,
            FRAME_HDR,
            FRAME_BODY,
            ERROR
        }

        private static readonly Logger log = Logger.Get(typeof(InputHandler));
        private readonly Object m_objectLock = new object();

        // the event raised when a buffer is read from the wire        
        public event EventHandler<ReceivedPayload<INetworkEvent>> ReceivedEvent;
        public event EventHandler<ExceptionArgs> ExceptionProcessing;

        // Not in used... This even is never raised in the code => the application will block on Close() until the timeout is reached 
        public event EventHandler Closed;

        event EventHandler<ReceivedPayload<INetworkEvent>> IReceiver<ReceivedPayload<INetworkEvent>>.Received
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

        event EventHandler<ExceptionArgs> IReceiver<ReceivedPayload<INetworkEvent>>.Exception
        {
            add
            {
                lock (m_objectLock)
                {
                    ExceptionProcessing += value;
                }
            }
            remove
            {
                lock (m_objectLock)
                {
                    ExceptionProcessing -= value;
                }
            }
        }

        private State state;
        private MemoryStream input;
        private int needed;

        private byte flags;
        private SegmentType type;
        private byte track;
        private int channel;

        public InputHandler(State state)
        {
            this.state = state;
            switch (state)
            {
                case State.PROTO_HDR:
                    needed = 8;
                    break;
                case State.FRAME_HDR:
                    needed = Frame.HEADER_SIZE;
                    break;
            }
        }

        // The command listening for a buffer read.  
        public void On_ReceivedBuffer(object sender, ReceivedPayload<MemoryStream> payload)
        {
            MemoryStream buf = payload.Payload;
            int remaining = (int) buf.Length;
            if( input != null )
            {
                remaining += (int) input.Length;
            }
            try
            {
                while (remaining > 0)
                {
                    if (remaining >= needed)
                    {                        
                        if (input != null)
                        {
                            byte[] tmp = new byte[buf.Length];
                            buf.Read(tmp, 0, tmp.Length);
                            input.Write(tmp, 0, tmp.Length);
                            input.Seek(0, SeekOrigin.Begin);
                            buf = input;    
                        }                      
                        int startPos = (int)buf.Position;
                        int consumed = needed;
                        state = Next(buf);
                        if ((buf.Position - startPos) < consumed)
                        {
                            buf.Seek(consumed  - (buf.Position - startPos), SeekOrigin.Current);
                        }
                        remaining -= consumed;
                        input = null;                        
                    }
                    else
                    {
                        byte[] tmp;
                        if (input == null)
                        {
                            input = new MemoryStream();
                            tmp = new byte[remaining];                            
                        }
                        else
                        {
                            // this is a full buffer 
                            tmp = new byte[buf.Length];
                        }
                        buf.Read(tmp, 0, tmp.Length);
                        input.Write(tmp, 0, tmp.Length);
                        remaining = 0;
                    }
                }
            }
            catch (Exception t)
            {
                Console.Write(t);
                if (ExceptionProcessing != null)
                {
                    ExceptionProcessing(this, new ExceptionArgs(t));
                }
            }
        }

        #region Private Support Functions

        private State Next(MemoryStream buf)
        {
            BinaryReader reader = new BinaryReader(buf);

            switch (state)
            {
                case State.PROTO_HDR:
                    char a = reader.ReadChar();
                    char m = reader.ReadChar();
                    char q = reader.ReadChar();
                    char p = reader.ReadChar();
                    if (a != 'A' &&
                        m != 'M' &&
                        q != 'Q' &&
                        p != 'P')
                    {
                        Error("bad protocol header: {0}", buf.ToString());
                        return State.ERROR;
                    }
                    reader.ReadByte(); 
                    byte instance = reader.ReadByte();
                    byte major = reader.ReadByte();
                    byte minor = reader.ReadByte();
                    Fire_NetworkEvent(new ProtocolHeader(instance, major, minor));                    
                    needed = Frame.HEADER_SIZE;
                    return State.FRAME_HDR;
                case State.FRAME_HDR:
                    reader = new BinaryReader(buf, Encoding.BigEndianUnicode);
                    flags = reader.ReadByte();
                    type = SegmentTypeGetter.Get(reader.ReadByte()); // generated code 
                    int size =  reader.ReadUInt16();
                    size = ByteEncoder.GetBigEndian((UInt16)size);                    
                    size -= Frame.HEADER_SIZE;
                    if (size < 0 || size > (64 * 1024 - 12))
                    {
                        Error("bad frame size: {0:d}", size);
                        return State.ERROR;
                    }
                    reader.ReadByte();
                    byte b = reader.ReadByte();
                    if ((b & 0xF0) != 0)
                    {
                        Error("non-zero reserved bits in upper nibble of " +
                              "frame header byte 5: {0}", b);
                        return State.ERROR;
                    }
                    track = (byte)(b & 0xF);
                    channel = reader.ReadUInt16();
                    channel = ByteEncoder.GetBigEndian((UInt16)channel);  
                    if (size == 0)
                    {
                        Fire_NetworkEvent(new Frame(flags, type, track, channel, 0, new MemoryStream()));                  
                        needed = Frame.HEADER_SIZE;
                        return State.FRAME_HDR;
                    }
                    needed = size;
                    return State.FRAME_BODY;
                case State.FRAME_BODY:                                       
                    Fire_NetworkEvent(new Frame(flags, type, track, channel, needed, buf));                  
                    needed = Frame.HEADER_SIZE;
                    return State.FRAME_HDR;
                default:
                    if (ExceptionProcessing != null)
                    {
                        ExceptionProcessing(this, new ExceptionArgs(new Exception("Error creating frame")));
                    }
                    throw new Exception("Error creating frame");
            }
        }
        
        private void Error(String fmt, params Object[] args)
        {
            Fire_NetworkEvent(new ProtocolError(Frame.L1, fmt, args));            
        }

        private void Fire_NetworkEvent(INetworkEvent netevent)
        {
            log.Debug("InputHandler: network event:", netevent);
            ReceivedPayload<INetworkEvent> payload = new ReceivedPayload<INetworkEvent>();
            payload.Payload = netevent;
            if (ReceivedEvent != null)
            {
                ReceivedEvent(this, payload);
            }
            else
            {
                log.Debug("Nobody listening for event: {0}");
            }
        }

        #endregion
    }
}