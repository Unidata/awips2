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
using org.apache.qpid.transport.codec;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport.network
{
    /// <summary> 
    /// Disassembler
    /// </summary>
    public sealed class Disassembler : ISender<IProtocolEvent>, IProtocolDelegate<Object>
    {
        private readonly IIoSender<MemoryStream> _sender;
        private readonly int _maxPayload;
        private readonly MemoryStream _header;
        private readonly BinaryWriter _writer;
        private readonly Object _sendlock = new Object();
        [ThreadStatic] static MSEncoder _encoder;


        public Disassembler(IIoSender<MemoryStream> sender, int maxFrame)
        {
            if (maxFrame <= network.Frame.HEADER_SIZE || maxFrame >= 64*1024)
            {
                throw new Exception(String.Format("maxFrame must be > {0} and < 64K: ", network.Frame.HEADER_SIZE) + maxFrame);
            }
            _sender = sender;
            _maxPayload = maxFrame - network.Frame.HEADER_SIZE;
            _header = new MemoryStream(network.Frame.HEADER_SIZE);
            _writer = new BinaryWriter(_header);
        }

        #region Sender Interface 

        public void Send(IProtocolEvent pevent)
        {
            pevent.ProcessProtocolEvent(null, this);
        }

        public void Flush()
        {
            lock (_sendlock)
            {
                _sender.Flush();
            }
        }

        public void Close()
        {
            lock (_sendlock)
            {
                _sender.Close();
            }
        }

        #endregion

        #region ProtocolDelegate<Object> Interface 

        public void Init(Object v, ProtocolHeader header)
        {
            lock (_sendlock)
            {
                _sender.Send(header.ToMemoryStream());
                _sender.Flush();
            }
        }

        public void Control(Object v, Method method)
        {
            InvokeMethod(method, SegmentType.CONTROL);
        }

        public void Command(Object v, Method method)
        {
            InvokeMethod(method, SegmentType.COMMAND);
        }

        public void Error(Object v, ProtocolError error)
        {
            throw new Exception("Error: " + error);
        }

        #endregion

        #region private 

        private void Frame(byte flags, byte type, byte track, int channel, int size, MemoryStream buf)
        {
            lock (_sendlock)
            {
                 _writer.Write(flags);
                _writer.Write(type);
                _writer.Write(ByteEncoder.GetBigEndian((UInt16)(size + network.Frame.HEADER_SIZE)));
                _writer.Write((byte)0);
                _writer.Write(track);
                _writer.Write(ByteEncoder.GetBigEndian((UInt16)( channel)));               
                _writer.Write((byte)0);
                _writer.Write((byte)0);
                _writer.Write((byte)0);
               _writer.Write((byte)0);
                _sender.Send(_header);
                _header.Seek(0, SeekOrigin.Begin);               
                _sender.Send(buf, size);
            }
        }

        private void Fragment(byte flags, SegmentType type, IProtocolEvent mevent, MemoryStream buf)
        {
            byte typeb = (byte) type;
            byte track = mevent.EncodedTrack == network.Frame.L4 ? (byte) 1 : (byte) 0;
            int remaining = (int) buf.Length;
            buf.Seek(0, SeekOrigin.Begin);
            bool first = true;
            while (true)
            {
                int size = Math.Min(_maxPayload, remaining);
                remaining -= size;              

                byte newflags = flags;
                if (first)
                {
                    newflags |= network.Frame.FIRST_FRAME;
                    first = false;
                }
                if (remaining == 0)
                {
                    newflags |= network.Frame.LAST_FRAME;
                }                

                Frame(newflags, typeb, track, mevent.Channel, size, buf);

                if (remaining == 0)
                {
                    break;
                }
            }
        }

        private MSEncoder GetEncoder()
        {
            if( _encoder == null)
            {
                _encoder = new MSEncoder(4 * 1024);
            }
            return _encoder;
        }

        private void InvokeMethod(Method method, SegmentType type)
        {
            MSEncoder encoder = GetEncoder();
            encoder.Init();
            encoder.WriteUint16(method.GetEncodedType());
            if (type == SegmentType.COMMAND)
            {
                if (method.Sync)
                {
                    encoder.WriteUint16(0x0101);
                }
                else
                {
                    encoder.WriteUint16(0x0100);
                }
            }
            method.Write(_encoder);
            MemoryStream methodSeg = encoder.Segment();

            byte flags = network.Frame.FIRST_SEG;

            bool payload = method.HasPayload();
            if (!payload)
            {
                flags |= network.Frame.LAST_SEG;
            }

            MemoryStream headerSeg = null;
            if (payload)
            {
                Header hdr = method.Header;
                Struct[] structs = hdr.Structs;

                foreach (Struct st in structs)
                {
                    encoder.WriteStruct32(st);
                }
                headerSeg = encoder.Segment();
            }

            lock (_sendlock)
            {
                Fragment(flags, type, method, methodSeg);
                if (payload)
                {
                    Fragment( 0x0, SegmentType.HEADER, method, headerSeg);
                    Fragment(network.Frame.LAST_SEG, SegmentType.BODY, method, method.Body);
                }
            }
        }

        #endregion
    }
}
