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

namespace org.apache.qpid.console 
{
	/**
	 * The result on invoking a method on a managed object
	 */
	public class MethodResult
	{
		
		public long ReturnCode {get;set;}
		public string Text {get;set;}			
		protected Dictionary<string, object> ReturnValues ;
				
		public MethodResult(long aCode, string aMsg, Dictionary<string, object> args)
		{
			ReturnCode = aCode ;
			Text = aMsg ;
			ReturnValues = args ;
		}

		public object GetReturnValue(string name) {
			object returnValue = null ;
			if (ReturnValues.ContainsKey(name)) {
				returnValue = ReturnValues[name] ;
			}
			return returnValue ;
		}
		
		public Dictionary<string, object> GetReturnValues() {
			return ReturnValues ;
		}
		
		public override string ToString()
		{
			string returnString = "" ;
			foreach (KeyValuePair<string, object> pair in ReturnValues) {
				returnString = returnString + String.Format("(Key: '{0}' Value: '{1}')", pair.Key, pair.Value) ;
			}
			
			return string.Format("MethodResult: ReturnCode={0}, Text={1} Values=[{2}]", ReturnCode, Text, returnString);
		}

	}
}
