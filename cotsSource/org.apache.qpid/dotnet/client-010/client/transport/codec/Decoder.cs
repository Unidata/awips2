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
	
	public interface Decoder
	{
		
		bool hasRemaining();
		
		short readUint8();
		int readUint16();
		long readUint32();
		long readUint64();

        short readInt8();
        int readInt16();
        long readInt32();
        long readInt64();   
        
        double readDouble() ;	
		float readFloat() ;		
		long readDatetime();
	    
        UUID readUuid();
		
		int readSequenceNo();
		RangeSet readSequenceSet(); // XXX
		RangeSet readByteRanges(); // XXX
		
		String readStr8();
		String readStr16();
		
		byte[] readVbin8();
		byte[] readVbin16();
		byte[] readVbin32();
		
		Struct readStruct32();
        Dictionary<String, Object> readMap();
        List<Object> readList();
        List<Object> readArray();

        Struct readStruct(int type);
	}
	
}
