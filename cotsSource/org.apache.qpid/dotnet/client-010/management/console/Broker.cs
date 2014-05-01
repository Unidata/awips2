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
using System.Threading ;
using org.apache.qpid.client ;
using org.apache.qpid.transport ;
using org.apache.qpid.transport.codec ;
using log4net ;

namespace org.apache.qpid.console
{

	/**
	 * Controls all communication with a broker. Works with the session to provide 
	 * synhchronous method calls across the asynchronous QMF bus.
	 */
	public class Broker : IMessageListener
	{
		public static ILog log = LogManager.GetLogger(typeof(Broker)) ;	
		public static int SYNC_TIME = 60000 ;
		
		public BrokerURL url ;
		public Dictionary<string, Agent> Agents = new Dictionary<string, Agent>() ;	
		
		private IClient client ;
		private IClientSession clientSession ;
		//FIXME This second session should not be needed. There is a bug in the underlieing code.
		private IClientSession outSession ;		
		private int timeout = 50000 ;
		private string replyName ;
		private string topicName ;
		private bool connected = false ;
		private bool syncInFlight = false ;		
		private bool topicBound = false ;
		private int reqsOutstanding = 0 ;
		private org.apache.qpid.console.Session consoleSession ;
		private object lockObject = new Object() ;	
		

		public Broker(org.apache.qpid.console.Session session, BrokerURL url)
		{
			log.Debug("Creating a new Broker for url " + url) ;
			this.url = url;
			consoleSession = session ;
			this.TryToConnect() ;
		}
		
		~Broker() {
			if (connected) {
				this.Shutdown() ;
			}
		}	
		
		public int BrokerBank() {
			return 1 ;
		}
		
		public bool IsConnected() {
			return connected ;
		}
		
		protected void TryToConnect() {
			reqsOutstanding = 1 ;		
			Agent newAgent = new Agent(this,0,"BrokerAgent") ;
			Agents.Add(newAgent.AgentKey(), newAgent) ;
			client = new Client() ;
			client.Connect(url.Hostname, url.Port, null, url.AuthName, url.AuthPassword) ;
			clientSession = client.CreateSession(timeout) ;		
			//clientSession.SetAutoSync(false) ;
			string name = System.Text.Encoding.UTF8.GetString(clientSession.GetName()) ;
			replyName = "reply-" + name ;
			topicName = "topic-" + name ;
			clientSession.SetAutoSync(true) ;
			Option[] options = new Option[] {Option.EXCLUSIVE, Option.AUTO_DELETE} ;
		
			// This queue is used for responses to messages which are sent.	
			clientSession.QueueDeclare(replyName,options) ;
			clientSession.ExchangeBind(replyName,"amq.direct",replyName) ;
			clientSession.AttachMessageListener(this, "rdest") ;			
			clientSession.MessageSubscribe(replyName,"rdest",MessageAcceptMode.NONE,MessageAcquireMode.PRE_ACQUIRED,null,0,null) ;			  			  						
            clientSession.MessageSetFlowMode("rdest", MessageFlowMode.WINDOW);
            clientSession.MessageFlow("rdest", MessageCreditUnit.BYTE, ClientSession.MESSAGE_FLOW_MAX_BYTES);
            clientSession.MessageFlow("rdest", MessageCreditUnit.MESSAGE, ClientSession.MESSAGE_FLOW_MAX_BYTES);  			
		
			// This queue is used for unsolicited messages sent to this class.
			clientSession.QueueDeclare(topicName, options) ;
			clientSession.AttachMessageListener(this, "tdest") ;			
			clientSession.MessageSubscribe(topicName,"tdest",MessageAcceptMode.NONE,MessageAcquireMode.PRE_ACQUIRED,null,0,null) ;							  									
            clientSession.MessageSetFlowMode("tdest", MessageFlowMode.WINDOW);
            clientSession.MessageFlow("tdest", MessageCreditUnit.BYTE, ClientSession.MESSAGE_FLOW_MAX_BYTES);
            clientSession.MessageFlow("tdest", MessageCreditUnit.MESSAGE, ClientSession.MESSAGE_FLOW_MAX_BYTES);  				
			
			outSession = client.CreateSession(timeout) ;	
			outSession.ExchangeBind(replyName,"amq.direct",replyName) ;
			
			connected = true ;
			consoleSession.HandleBrokerConnect(this) ;		
				
			
			IEncoder encoder = CreateEncoder() ;
			this.SetHeader(encoder, 'B', 0) ;
			this.Send(encoder) ;
		}
		
		public void Shutdown() {
			if (connected) {
				this.WaitForStable() ;
				clientSession.MessageStop("rdest") ;
				clientSession.MessageStop("tdest") ;
				clientSession.Close() ;
				client.Close() ;
				this.connected = false ;
			}
		}
		
		public void UpdateAgent(QMFObject obj) {
			long agentBank = (long)obj.GetProperty("agentBank") ;
			long brokerBank = (long)obj.GetProperty("brokerBank") ;
			String key = Agent.AgentKey(agentBank, brokerBank) ;
			if (obj.IsDeleted()) {
				if (Agents.ContainsKey(key)) {
					Agent agent = Agents[key] ;
					Agents.Remove(key) ;
					consoleSession.HandleAgentRemoved(agent) ;
				}
			}
			else {
				if (! Agents.ContainsKey(key)) {
					Agent newAgent = new Agent(this, agentBank, (string)obj.GetProperty("label")) ;
					Agents.Add(key, newAgent) ;
					consoleSession.HandleNewAgent(newAgent) ;
				}
			}
		}
		
		public IEncoder CreateEncoder() {
			return new MSEncoder(1000) ;	
		}
		
		
		public IEncoder CreateEncoder(char opcode, long sequence) {
			return SetHeader(this.CreateEncoder(), opcode, sequence) ;
		}
		
		public IEncoder SetHeader(IEncoder enc, char opcode, long sequence) {
			enc.WriteUint8((short)'A') ;
			enc.WriteUint8((short)'M') ;
			enc.WriteUint8((short)'2') ;
	        enc.WriteUint8((short)opcode) ;
			enc.WriteUint32(sequence) ;
			return enc ;
		}		
		
		public Message CreateMessage(IEncoder enc) {
			return this.CreateMessage(enc, "broker", -1)  ;
		}		
		
		public Message CreateMessage(IEncoder enc, string routingKey) {
			return this.CreateMessage(enc, routingKey, -1) ;
		}
		
		public Message CreateMessage(IEncoder enc, string routingKey, long ttl) {
			Message msg = new Message() ;
			msg.Body = ((MSEncoder)enc).Segment() ;
			msg.DeliveryProperties.SetRoutingKey(routingKey) ;
			if (-1 != ttl) {
				msg.DeliveryProperties.SetTtl(ttl) ;
			}
			msg.MessageProperties.SetContentType("x-application/qmf") ;
			msg.MessageProperties.SetReplyTo(new ReplyTo("amq.direct", replyName)) ;
			return msg ;
		}
		
		public void Send(IEncoder enc) {
			this.Send(this.CreateMessage(enc)) ;
		}
		
		public void Send(Message msg) {
	
			lock (lockObject) {
				log.Debug(String.Format("Sending message to routing key '{0}'", msg.DeliveryProperties.GetRoutingKey())) ;
				//log.Debug(System.Text.Encoding.UTF8.GetString(msg.Body.ToArray())) ;			
				outSession.MessageTransfer("qpid.management", msg) ;
				//clientSession.sync() ;
			}
		}
		
		protected bool CheckHeader(IDecoder decoder, out char opcode, out long sequence) {
			bool returnValue = false ;		
			opcode = 'x' ;
			sequence = -1 ;
			if(decoder.HasRemaining()) {
				char character = (char) decoder.ReadUint8() ;
				if (character != 'A') {
					return returnValue ;
				}
				character = (char) decoder.ReadUint8() ;			
				if (character != 'M') {
					return returnValue ;
				}
				character = (char) decoder.ReadUint8() ;			
				if (character != '2') {
					return returnValue ;
				}	
				returnValue = true ;
				opcode = (char) decoder.ReadUint8() ;
				sequence = decoder.ReadUint32() ;
			}
			return returnValue ;
		}
		
		public void MessageTransfer(IMessage msg) {
			MSDecoder decoder = new MSDecoder() ;
			decoder.Init(msg.Body) ;
			RangeSet rangeSet = new RangeSet() ;
			rangeSet.Add(msg.Id) ;
			char opcode = 'x' ;
			long seq = -1 ;
			while (this.CheckHeader(decoder, out opcode, out seq)) {
				//log.Debug("Message recieved with opcode " + opcode + " and sequence " + seq) ;
				//log.Debug(System.Text.Encoding.UTF8.GetString(msg.Body.ToArray())) ;
				switch (opcode) {
					case 'b':
						consoleSession.HandleBrokerResponse(this, decoder, seq) ;
						break ;
					case 'p':
						consoleSession.HandlePackageIndicator(this, decoder, seq) ;
						break ;		
					case 'z':
						consoleSession.HandleCommandComplete(this, decoder, seq) ;
						break ;		
					case 'q':
						consoleSession.HandleClassIndicator(this, decoder, seq) ;
						break ;			
					case 'm':
						consoleSession.HandleMethodResponse(this, decoder, seq) ;
						break ;							
					case 'h':
						consoleSession.HandleHeartbeatIndicator(this, decoder, seq, msg) ;
						break ;						
					case 'e':
						consoleSession.HandleEventIndicator(this, decoder, seq) ;
						break ;							
					case 's':
						consoleSession.HandleSchemaResponse(this, decoder, seq) ;
						break ;			
					case 'c':
						consoleSession.HandleContentIndicator(this, decoder, seq, true, false) ;
						break ;	
					case 'i':
						consoleSession.HandleContentIndicator(this, decoder, seq, false, true) ;
						break ;		
					case 'g':
						consoleSession.HandleContentIndicator(this, decoder, seq, true, true) ;
						break ;			
					default:
						log.Error("Invalid message type recieved with opcode " + opcode) ;
						break ;
				}		
			} 
			lock (lockObject) {
				outSession.MessageAccept(rangeSet) ;
			}
		}
		
		public void IncrementOutstanding() {
			lock (lockObject) {
				this.reqsOutstanding += 1 ;	
			}
		}
		
		public void DecrementOutstanding() {
			lock (lockObject) {
				this.reqsOutstanding -= 1 ;	
				if ((reqsOutstanding == 0) & !topicBound) {
					foreach (string key in consoleSession.BindingKeys()) {
						//this.clientSession.ExchangeBind(topicName, "qpid.mannagement", key) ;
						log.Debug("Setting Topic Binding " + key) ;						
						this.outSession.ExchangeBind(topicName, "qpid.management", key) ;
					}
					topicBound = true ;
				}
				if ((reqsOutstanding == 0) & syncInFlight) {
					syncInFlight = false ;
					Monitor.PulseAll(lockObject) ;	
				}
			}
		}	
		
		public void WaitForStable() {
			lock (lockObject) {
				if (connected) {
					DateTime start = DateTime.Now ;
					syncInFlight = true ;					
					while (reqsOutstanding != 0) {					
						log.Debug("Waiting to recieve messages") ;
						Monitor.Wait(lockObject,SYNC_TIME) ;
						TimeSpan duration = DateTime.Now - start;						
						if (duration.TotalMilliseconds > SYNC_TIME) {
							throw new Exception("Timeout waiting for Broker to Sync") ;
						}
					}
				}
			}			
		}	
		
		public void SetSyncInFlight(bool inFlight) {
			lock(lockObject) {
				syncInFlight = inFlight ;
				Monitor.PulseAll(lockObject) ;
			}
		}
		
		public bool GetSyncInFlight() {
			return syncInFlight ;
		}		
		
		public void WaitForSync(int timeout) {
			lock(lockObject) {
				DateTime start = DateTime.Now ;			
				while (syncInFlight) {
					Monitor.Wait(lockObject,timeout) ;
				}
				TimeSpan duration = DateTime.Now - start;						
				if (duration.TotalMilliseconds > timeout) {
					throw new Exception("Timeout waiting for Broker to Sync") ;
				}
			} 
		}
	}
}
