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

namespace org.apache.qpid.transport
{
	
	
	/// <summary> 
    /// Binary
	/// </summary>
	
	public sealed class Binary
	{
		
		private readonly byte[] bytes;
		private readonly int offset_Renamed_Field;
		private readonly int size_Renamed_Field;
		private int hash = 0;
		
		public Binary(byte[] bytes, int offset, int size)
		{
			if (offset + size > bytes.Length)
			{
				throw new System.IndexOutOfRangeException();
			}
			
			this.bytes = bytes;
			offset_Renamed_Field = offset;
			size_Renamed_Field = size;
		}
		
		public Binary(byte[] bytes):this(bytes, 0, bytes.Length)
		{
		}
		
		public byte[] Array()
		{
			return bytes;
		}
		
		public int Offset()
		{
			return offset_Renamed_Field;
		}
		
		public int Size()
		{
			return size_Renamed_Field;
		}
		
		public Binary Slice(int low, int high)
		{
			int sz;
			
			if (high < 0)
			{
				sz = size_Renamed_Field + high;
			}
			else
			{
				sz = high - low;
			}
			
			if (sz < 0)
			{
				sz = 0;
			}
			
			return new Binary(bytes, offset_Renamed_Field + low, sz);
		}
		
		public override int GetHashCode()
		{
			if (hash == 0)
			{
				int hc = 0;
				for (int i = 0; i < size_Renamed_Field; i++)
				{
					hc = 31 * hc + (0xFF & bytes[offset_Renamed_Field + i]);
				}
				hash = hc;
			}
			
			return hash;
		}
		
		public  override bool Equals(System.Object o)
		{
			if (!(o is Binary))
			{
				return false;
			}
			
			Binary buf = (Binary) o;
			if (size_Renamed_Field != buf.size_Renamed_Field)
			{
				return false;
			}
			
			for (int i = 0; i < size_Renamed_Field; i++)
			{
				if (bytes[offset_Renamed_Field + i] != buf.bytes[buf.offset_Renamed_Field + i])
				{
					return false;
				}
			}
			
			return true;
		}
	}
}
