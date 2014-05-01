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
using System.Diagnostics;
using System.IO;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport.codec
{
    /// <summary> 
    /// MSEncoder
    /// </summary>
    public sealed class MSEncoder : AbstractEncoder
    {
        private readonly MemoryStream _out;
        private readonly BinaryWriter _writer;

        public MSEncoder(int capacity)
        {
            _out = new MemoryStream(capacity);
            _writer = new BinaryWriter(_out);
        }

        public void Init()
        {
            _out.Seek(0, SeekOrigin.Begin);            
        }

        public MemoryStream Segment()
        {            
            int length = (int) _out.Position;
            MemoryStream result =  new MemoryStream(_out.ToArray(), 0, length);
            result.Seek(length, SeekOrigin.Begin);
            _out.Seek(0, SeekOrigin.Begin);
            return result;
        }


        protected override void DoPut(byte b)
        {
            _writer.Write(b);
        }

        protected override void DoPut(MemoryStream src)
        {
            _writer.Write(src.ToArray());
        }

        protected override void Put(byte[] bytes)
        {
            _writer.Write(bytes);
        }

        public override void WriteUint8(short b)
        {
            Debug.Assert(b < 0x100);
            _writer.Write((byte) b);
        }

        public override void WriteUint16(int s)
        {
            Debug.Assert(s < 0x10000);
            _writer.Write(ByteEncoder.GetBigEndian((UInt16) s));
        }

        public override void WriteUint32(long i)
        {
            Debug.Assert(i < 0x100000000L);
            _writer.Write(ByteEncoder.GetBigEndian((UInt32) i));
        }

        public override void WriteUint64(long l)
        {
            _writer.Write(ByteEncoder.GetBigEndian(l));
        }

        public override void WriteInt8(short b)
        {
            Debug.Assert(b < 0x100);
            _writer.Write((byte) b);
        }

        public override void WriteInt16(int s)
        {
            Debug.Assert(s < 0x10000);
            _writer.Write(ByteEncoder.GetBigEndian((Int16) s));
        }

        public override void WriteInt32(long i)
        {
            Debug.Assert(i < 0x100000000L);
            _writer.Write(ByteEncoder.GetBigEndian((Int32) i));
        }

        public override void WriteInt64(long l)
        {
            _writer.Write(ByteEncoder.GetBigEndian(l));
        }        
        
        public override void WriteFloat(float f) {
        	_writer.Write(f) ;
        }
        
        public override void WriteDouble(double d) {
        	_writer.Write(ByteEncoder.GetBigEndian(d)) ;
        }
        
        protected override int BeginSize8()
        {
            int pos = (int) _out.Position;
            _writer.Write((byte) 0);
            return pos;
        }

        protected override void EndSize8(int pos)
        {
            int cur = (int) _out.Position;
            _out.Seek(pos, SeekOrigin.Begin);
            _writer.Write((byte) (cur - pos - 1));
            _out.Seek(cur, SeekOrigin.Begin);
        }

        protected override int BeginSize16()
        {
            int pos = (int) _out.Position;
            _writer.Write((short) 0);
            return pos;
        }

        protected override void EndSize16(int pos)
        {
            int cur = (int) _out.Position;
            _out.Seek(pos, SeekOrigin.Begin);
            _writer.Write((short) (cur - pos - 2));
            _out.Seek(cur, SeekOrigin.Begin);
        }

        protected override int BeginSize32()
        {
            int pos = (int) _out.Position;
            _writer.Write(0);
            return pos;
        }

        protected override void EndSize32(int pos)
        {
            int cur = (int) _out.Position;
            _out.Seek(pos, SeekOrigin.Begin);
            _writer.Write(ByteEncoder.GetBigEndian((Int32) cur - pos - 4));
            _out.Seek(cur, SeekOrigin.Begin);
        }
    }
}
