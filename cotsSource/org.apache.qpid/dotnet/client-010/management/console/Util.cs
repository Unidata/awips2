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
using org.apache.qpid.client ;
using org.apache.qpid.transport.util;
using org.apache.qpid.transport.codec ;

namespace org.apache.qpid.console
{
	public class Util
	{
		static Dictionary<Type, short> ENCODINGS = new Dictionary<Type, short>() ;
		
		
		static Util() {
			ENCODINGS.Add(typeof(string), 7) ;
			ENCODINGS.Add(typeof(short), 1) ;
			//ENCODINGS.Add(typeof(int), 2) ;	
			//ENCODINGS.Add(typeof(long), 3) ;
			ENCODINGS.Add(typeof(float), 13) ;
			ENCODINGS.Add(typeof(QMFObject), 20) ;
			ENCODINGS.Add(typeof(int), 17) ;				
			ENCODINGS.Add(typeof(long), 18) ;							
			ENCODINGS.Add(typeof(System.Collections.Generic.List<>), 21) ;			
		}
		
		/**
		 * Converts type numbers to schema type names
		 */
		public static string TypeName(short type) {
			switch(type) {
				//case 0: return "UNKNOWN" ;
				case 1: return "uint8" ;
				case 2: return "uint16" ;
				case 3: return "uint32" ;
				case 4: return "uint64" ;	
				case 5: return "bool" ;
				case 6: return "short-string" ;
				case 7: return "long-string" ;
				case 8: return "abs-time" ;		
				case 9: return "delta-time" ;
				case 10: return "reference" ;
				case 11: return "boolean" ;
				case 12: return "float" ;		
				case 13: return "double" ;
				case 14: return "uuid" ;
				case 15: return "field-table" ;
				case 16: return "int8" ;		
				case 17: return "int16" ;		
				case 18: return "int32" ;		
				case 19: return "int64" ;		
				case 20: return "object" ;		
				case 21: return "list" ;		
				case 22: return "array" ;		
			}
			
			throw new Exception(String.Format("Invalid Type Code: {0}", type)) ;
		}		
      		
		/**
		 * Converts schema numbers to schema access names
		 */      		
		public static string AccessName(int type) {
			switch(type) {
				//case 0: return "UNKNOWN" ;			
				case 1: return "ReadCreate" ;
				case 2: return "ReadWrite" ;
				case 3: return "ReadOnly" ;	
			}
			
			throw new Exception(String.Format("Invalid Access Code: {0}", type)) ;			
		}	
		
		/**
		 * Default values per schema type
		 */      				
		public static object DefaultValue(short type) {
			switch(type) {
				//case 0: return "UNKNOWN" ;
				case 1: return 0 ;
				case 2: return 0 ;
				case 3: return 0l ;
				case 4: return 0l ;	
				case 5: return false ;
				case 6: return "" ;
				case 7: return "" ;
				case 8: return 0l ;		
				case 9: return 0l ;
				case 10: return new ObjectID() ;
				case 11: return false ;
				case 12: return 0f ;		
				case 13: return 0d ;
				case 14: return new UUID(0,0) ;
				case 15: return new Dictionary<string, object>();
				case 16: return 0 ;		
				case 17: return 0 ;		
				case 18: return 0l ;		
				case 19: return 0l ;		
				case 20: return null ;		
				case 21: return new List<object>() ;		
				case 22: return new List<object>() ;		
			}
			
			throw new Exception(String.Format("Invalid Type Code: {0}", type)) ;
		}		
				
		/**
		 * Returns a QMF type based on C# object type
		 */      						
		public static short QMFType(object obj) {
			if (ENCODINGS.ContainsKey(obj.GetType())) {
				return ENCODINGS[obj.GetType()] ;
			}	else {
				throw new Exception (String.Format("Unkown Type of {0}", obj.GetType())) ;
			}
		}
		
		/**
		 * Grabs a friendly string version of bytes.
		 */      				
		public static string ByteString(byte[] bytes) {
			return System.Text.Encoding.UTF8.GetString(bytes) ;
		}
		
		protected Util()
		{

		}
	}
}
