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
using org.apache.qpid.transport.codec ;

namespace org.apache.qpid.console
{
	
	/**
	 * Uniquely identifies an object on the bus.
	 */	
	public class ObjectID
	{
		
		protected long first ;
		protected long second ;
		
		public ObjectID() {
		}
		
		public ObjectID(IDecoder dec)
		{
			first = (long)dec.ReadUint64() ;
			second = (long)dec.ReadUint64() ;			
		}
		
		public ObjectID(long first, long second) {
			this.first = first ;
			this.second = second ;
		}

		public long Flags() {		
    		return (long) ((ulong)this.first & 0xF000000000000000) >> 60 ;
    	}
    	
		public long Sequence() {		
    		return (long)((ulong)this.first & 0x0FFF000000000000) >> 48 ;
    	}    	
    	
		public long BrokerBank() {		
    		return (long)((ulong)this.first & 0x0000FFFFF0000000) >> 28 ;
    	}    
    	
		public long AgentBank() {		
    		return (this.first & 0x000000000FFFFFFF) ;
    	}    
    	
    	public long Object() {
    		return second ;
    	}
    	
    	public bool IsDurable() {
    		return Sequence() == 0 ;
    	}

		public void encode(IEncoder enc) {
			enc.WriteUint64((long)first) ;
			enc.WriteUint64((long)second) ;
		}

		override public string ToString() {
			return "" + Flags() + "-" + Sequence() + "-" + BrokerBank() + "-" + AgentBank() + "-" + Object() ;
		}
		
		public string RoutingCode() {
			return Agent.RoutingCode((long)AgentBank(), (long)BrokerBank()) ;	
		}
	}
}
