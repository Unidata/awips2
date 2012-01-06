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

namespace org.apache.qpid.transport.codec
{		
	/// <summary> 
	/// Decoder
	/// </summary>
	
	public interface IDecoder
	{
		
		bool HasRemaining();
		
		short ReadUint8();
		int ReadUint16();
		long ReadUint32();
		long ReadUint64();

        short ReadInt8();
        int ReadInt16();
        long ReadInt32();
        long ReadInt64();   
        
        double ReadDouble() ;	
		float ReadFloat() ;		
		long ReadDatetime();
	    
        UUID ReadUuid();
		
		int ReadSequenceNo();
		RangeSet ReadSequenceSet(); // XXX
		RangeSet ReadByteRanges(); // XXX
		
		String ReadStr8();
		String ReadStr16();
		
		byte[] ReadVbin8();
		byte[] ReadVbin16();
		byte[] ReadVbin32();
		
		Struct ReadStruct32();
        Dictionary<String, Object> ReadMap();
        List<Object> ReadList();
        List<Object> ReadArray();

        Struct ReadStruct(int type);
	}
	
}
