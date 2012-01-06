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

namespace org.apache.qpid.transport.codec
{
	
	
	/// <summary> 
	/// MSDecoder
	/// 
	/// </summary>
	
	
	public sealed class MSDecoder : AbstractDecoder
	{

	    private BinaryReader _reader;

        public void Init(MemoryStream st)
		{            
            _reader = new BinaryReader(st, Encoding.BigEndianUnicode);
		}
		
		protected override byte DoGet()
		{
		    return _reader.ReadByte();
		}

        protected override void DoGet(byte[] bytes)
		{
            _reader.Read(bytes, 0, bytes.Length);
		}
	
		public override bool HasRemaining()
		{
		    return (_reader.BaseStream.Position < _reader.BaseStream.Length);
		}

        public override short ReadUint8()
		{
			return (short) (0xFF & _reader.ReadByte());
		}

        public override int ReadUint16()
		{
		    return ByteEncoder.GetBigEndian((UInt16) _reader.ReadInt16());
		}

        public override long ReadUint32()
		{
            return ByteEncoder.GetBigEndian((UInt32) _reader.ReadInt32());
		}

        public override long ReadUint64()
		{
		    return (long) ByteEncoder.GetBigEndian(_reader.ReadInt64());            
		}

        public override short ReadInt8()
		{
			return (short) (0xFF & _reader.ReadByte());
		}

        public override int ReadInt16()
		{
		    return ByteEncoder.GetBigEndian((Int16) _reader.ReadInt16());
		}

        public override long ReadInt32()
		{
            return ByteEncoder.GetBigEndian((Int32) _reader.ReadInt32());
		}

        public override long ReadInt64()
		{
		    return (long) ByteEncoder.GetBigEndian(_reader.ReadInt64());            
		}        
		
		public override double ReadDouble() {
			return (double) ByteEncoder.GetBigEndian(_reader.ReadDouble()) ;
		}
		
		public override float ReadFloat() {
			return (float) _reader.ReadSingle() ;
		}		
	}
}
