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

namespace org.apache.qpid.console
{
	
	/**
	 * URL which defines the connection to the broker to hook up to the QMF
	 * Bus.
	 */
	public class BrokerURL 
	{
		public string Hostname {get;set;}
		public int Port {get;set;}
		public string AuthName {get;set;}
		public string AuthPassword {get;set;}
		public string AuthMechanism {get;set;}
		protected bool ssl = false ;
		
		public BrokerURL(string str) 
		{
			Uri uri = new Uri(str) ;
			this.Hostname = uri.Host ;
			if (uri.Scheme.Equals("amqp")) {
				Port=5672 ;
			} else {
				ssl = true ;
				Port=5673 ;
			}
			
			//FIXME Make this more robust
			this.AuthName = "guest" ;
			this.AuthPassword = "guest" ;
			this.AuthMechanism = "PLAIN" ;
		}
		
		public string GetURI() {
			return Hostname ;
		}
		
		public override string ToString ()
		{	
			if (ssl) {
				return String.Format("amqps://{0}:{1}", Hostname, Port) ;
			} else {
				return String.Format("amqp://{0}:{1}", Hostname, Port) ;
			}
		}
 
	}
}
