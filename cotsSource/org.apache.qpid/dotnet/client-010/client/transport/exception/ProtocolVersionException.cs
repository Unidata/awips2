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
namespace org.apache.qpid.transport
{
	
	
	/// <summary> ProtocolVersionException
	/// 
	/// </summary>
	
	[Serializable]
	public sealed class ProtocolVersionException:TransportException
	{
		public sbyte Major
		{
			get
			{
				return _major;
			}
			
		}
		public sbyte Minor
		{
			get
			{
				return _minor;
			}
			
		}
		
		private sbyte _major;
		private sbyte _minor;
		
		public ProtocolVersionException(sbyte major, sbyte minor):base(String.Format("version missmatch: %{0}-{1}", major, minor))
		{
			this._major = major;
			this._minor = minor;
		}
	}
}
