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
using org.apache.qpid.transport;
using org.apache.qpid.transport.codec ;

namespace org.apache.qpid.console
{
	
	/**
	 * Metadata that describes the mapping of a method to a QMF Object
	 */		
	public class SchemaMethod
	{
		public string Name {get;set;}
		public int ArgCount {get;set;}
		public int InputArgCount {get;set;}
		public int OutputArgCount {get;set;}		
		public int BidirectionalArgCount {get;set;}				
		public string Description {get;set;}
		public List<SchemaArgument> Arguments = new List<SchemaArgument>();
		
		public SchemaMethod(IDecoder dec)
		{
			Dictionary<string, Object> map = dec.ReadMap() ;
			Name = (string)  map["name"] ;
			ArgCount = (int) map["argCount"] ;
			if (map.ContainsKey("desc")) {
				Description = (string) map["desc"] ;
			}
			for (int x = 0 ; x < ArgCount ; x++) {
				SchemaArgument arg = new SchemaArgument(dec, true) ; 
				Arguments.Add(arg) ;
				if (arg.IsInput()) {
					InputArgCount += 1 ;
				}
				if (arg.IsOutput()) {
					OutputArgCount += 1 ;
				}			
				if (arg.IsBidirectional()) {
					BidirectionalArgCount += 1 ;
				}	
			}
		}
	}
}
