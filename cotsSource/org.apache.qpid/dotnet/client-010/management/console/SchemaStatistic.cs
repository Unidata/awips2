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
using System.Collections.Generic ;
using org.apache.qpid.transport.codec ;

namespace org.apache.qpid.console
{
	
	/**
	 * Metadata that describes the mapping of an object Statistic
	 */			
	public class SchemaStatistic
	{
	
		public string Name {get;set;}
		public short Type {get;set;}
		public string Description {get;set;}		
		public string Unit {get;set;}		
		
		public SchemaStatistic(IDecoder dec)
		{
			Dictionary<string, Object> map = dec.ReadMap() ;							
			Name = (string)  map["name"] ;
			Type = (short) short.Parse(""+map["type"]) ;
				
			if (map.ContainsKey("unit")) {
				Unit = (string) map["unit"] ;			
			}
			if (map.ContainsKey("description")) {			
				Description = (string) map["description"] ;			
			}			
		}
	}
}
