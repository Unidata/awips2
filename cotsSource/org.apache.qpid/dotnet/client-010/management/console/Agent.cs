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
using log4net ;

namespace org.apache.qpid.console
{	
	
	/**
	 * Local representation of a remote agent which has been found on the bus.
	 */	
	public class Agent
	{
		public static ILog log = LogManager.GetLogger(typeof(Agent)) ;	
		
		public Broker Broker {get;set;}
		public long BrokerBank {get;set;}
		public long AgentBank {get;set;}
		public string label {get;set;}
		
		public Agent(Broker broker, long agentBank, string label)
		{
			this.Broker = broker ;
			this.BrokerBank = broker.BrokerBank() ;
			this.AgentBank = agentBank ;
			this.label = label ;
		}
		
		public string AgentKey() {
			return Agent.AgentKey(AgentBank, BrokerBank) ;
		}
		
		public string RoutingCode() {
			return Agent.RoutingCode(AgentBank, BrokerBank)  ;		
		}		
		
		public static string AgentKey(long AgentBank, long BrokerBank) {
			return String.Format("{0}:{1}", AgentBank, BrokerBank)  ;		
		}
		
		public static string RoutingCode(long AgentBank, long BrokerBank) {
			return String.Format("agent.{0}.{1}", BrokerBank, AgentBank)  ;		
		}		
		
		public static long GetBrokerBank(string routingKey) {
			string delim = "." ;
			return long.Parse(routingKey.Split(delim.ToCharArray())[2]) ;
		}
		
		public static long GetAgentBank(string routingKey) {
			string delim = "." ;
			return long.Parse(routingKey.Split(delim.ToCharArray())[3]) ;		
		}		
		
	}
}
