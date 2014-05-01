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
using org.apache.qpid.transport.codec ;


namespace org.apache.qpid.console
{
	public enum EventSeverity : short {
		 EMER  = 0 ,
		 ALERT = 1 ,
		 CRIT  = 2 ,
		 ERROR = 3 ,
		 WARN  = 4 ,
		 NOTIC = 5 ,
		 INFO  = 6 ,
		 DEBUG = 7
	}

	/**
	 * An event raised by an agent on the bus.
	 */
	public class QMFEvent
	{
		public Session Session { get;set; }
		public ClassKey ClassKey {get;set;}
		//FIXME time?
		public long Timestamp {get;set;}
		public EventSeverity Severity {get;set;}
		public Dictionary<string, object> Arguments {get;set;} 
		
		public QMFEvent(Session session, IDecoder dec)
		{
			Session = session ;
			ClassKey = new ClassKey(dec) ;
			Timestamp = dec.ReadInt64() ;
			Severity = (EventSeverity) dec.ReadUint8() ;
			SchemaClass sClass = Session.GetSchema(ClassKey) ;
			Arguments = new Dictionary<string, object>() ;
			
			if (sClass != null) {
				foreach (SchemaArgument arg in sClass.Arguments) {
					Arguments[arg.Name] = Session.DecodeValue(dec, arg.Type) ;	
				}
			}	
		}
		
		public object GetArgument(string argName) {
			object returnValue = null ;
			Arguments.TryGetValue(argName, out returnValue) ;
			return returnValue ;
		}		
	}
}
