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
using System.Threading ;

namespace org.apache.qpid.console
{
	
	/**
	 * Holds state during asynchronous calls to the broker.
	 */      			
	public class SequenceManager
	{
		long sequence = 0 ;
		Dictionary<long, Object> pending = new Dictionary<long, Object>() ;
		Object lockObject = new Object() ;
		
		public SequenceManager()
		{
		}
		
		public long Reserve(Object data) {
			long returnValue = 0 ;
			lock(lockObject) {
				returnValue = sequence ;
				sequence += 1 ;
				pending.Add(returnValue, data) ;
			}
			return returnValue;
		}
		
		public Object Release(long seq) {
			Object returnValue = null ;
			lock(lockObject) {
				returnValue = pending[seq] ;
			    pending.Remove(seq) ;
			}
			
			return returnValue ;
		}
	}
}
