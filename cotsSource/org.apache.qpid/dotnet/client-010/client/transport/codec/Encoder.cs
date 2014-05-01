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
	
	public interface Encoder
	{
		
		void  writeUint8(short b);
		void  writeUint16(int s);
		void  writeUint32(long i);
		void  writeUint64(long l);

		void  writeInt8(short b);
		void  writeInt16(int s);
		void  writeInt32(long i);
		void  writeInt64(long l);      
		
		void writeFloat(float f) ;
		void writeDouble(double d) ;  
		
		void  writeDatetime(long l);
		void  writeUuid(UUID uuid);
		
		void  writeSequenceNo(int s);
		void  writeSequenceSet(RangeSet ranges); // XXX
		void  writeByteRanges(RangeSet ranges); // XXX
		
		void  writeStr8(string s);
		void  writeStr16(string s);
		
		void  writeVbin8(byte[] bytes);
		void  writeVbin16(byte[] bytes);
		void  writeVbin32(byte[] bytes);
		
		void writeStruct32(Struct s);
        void writeMap(Dictionary<String, Object> map);
        void writeList(List<Object> list);
        void writeArray(List<Object> array);

        void writeStruct(int type, Struct s);
	}
}
