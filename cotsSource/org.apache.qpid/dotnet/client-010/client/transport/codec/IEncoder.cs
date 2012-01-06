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
using org.apache.qpid.transport.util;
using RangeSet = org.apache.qpid.transport.RangeSet;
using Struct = org.apache.qpid.transport.Struct;
namespace org.apache.qpid.transport.codec
{		
	/// <summary> 
    /// Encoder
	/// </summary>
	
	public interface IEncoder
	{
		
		void  WriteUint8(short b);
		void  WriteUint16(int s);
		void  WriteUint32(long i);
		void  WriteUint64(long l);

		void  WriteInt8(short b);
		void  WriteInt16(int s);
		void  WriteInt32(long i);
		void  WriteInt64(long l);      
		
		void WriteFloat(float f) ;
		void WriteDouble(double d) ;  
		
		void  WriteDatetime(long l);
		void  WriteUuid(UUID uuid);
		
		void  WriteSequenceNo(int s);
		void  WriteSequenceSet(RangeSet ranges); // XXX
		void  WriteByteRanges(RangeSet ranges); // XXX
		
		void  WriteStr8(string s);
		void  WriteStr16(string s);
		
		void  WriteVbin8(byte[] bytes);
		void  WriteVbin16(byte[] bytes);
		void  WriteVbin32(byte[] bytes);
		
		void WriteStruct32(Struct s);
        void WriteMap(Dictionary<String, Object> map);
        void WriteList(List<Object> list);
        void WriteArray(List<Object> array);

        void WriteStruct(int type, Struct s);
	}
}
