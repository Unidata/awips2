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
	 * Callbacks which are exposed by the Session. Clients should create anm implementaiton of this
	 * for more fine grained interaction with the bus.
	 */
	public interface Console
	{
		void NewAgent(Agent agent) ;
		void AgentRemoved(Agent agent) ;
		void BrokerConnected(Broker broker) ;
		void BrokerDisconnected(Broker broker) ;	
		void BrokerInformation(Broker broker) ;
		void NewPackage(String packageName) ;
		void NewClass(short kind, ClassKey key) ;
		void ObjectProperties(Broker broker, QMFObject obj) ;
		void ObjectStatistics(Broker broker, QMFObject obj) ;
		void MethodResponse(Broker broker, long seq, MethodResult response) ;
		void EventRecieved(Broker broker, QMFEvent anEvent) ;
		void HearbeatRecieved(Agent agent, long timestamp) ;
		Type TypeMapping(ClassKey key) ;
	}		
}
