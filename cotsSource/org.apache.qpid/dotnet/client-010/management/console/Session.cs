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
using System.IO ;
using System.Reflection ;
using System.Threading ;
using log4net ;
using org.apache.qpid.client ;
using org.apache.qpid.transport.util;
using org.apache.qpid.transport.codec ;

namespace org.apache.qpid.console
{
	
	/**
	 * Main Interaction point for interaction with the bus. Can be used to locate objects 
	 * on the bus, and invoke messages on them.
	 */      			
    public class Session
    {
		public static ILog log = LogManager.GetLogger(typeof(Session)) ;
			
		public static int CONTEXT_SYNC     = 1 ;
		public static int CONTEXT_STARTUP  = 2 ;
		public static int CONTEXT_MULTIGET = 3 ;
		public static int DEFAULT_GET_WAIT_TIME = 60000 ;			
	
        public bool RecieveObjects = true ;
        public bool RecieveEvents = true ;
        public bool RecieveHeartbeat = true ;
        public bool UserBindings = false ;
       	public Console Console ;
        
        protected Dictionary<string, Dictionary<string, SchemaClass>> Packages = new Dictionary<string, Dictionary<string, SchemaClass>>() ;
        protected List<Broker> Brokers = new List<Broker>() ;
	    protected SequenceManager SequenceManager = new SequenceManager() ;
	    protected object LockObject = new Object();
	    protected List<long> SyncSequenceList = new List<long>() ;
	    protected List<QMFObject> GetResult ;
	    protected Object SyncResult ;

        public Session()
        {
        }
	
		public Session(Console console) {
		    Console = console ;
		}

        public void AddBroker(string url) {
				BrokerURL brokerurl = GetBrokerURL(url) ;
                Broker broker = new Broker(this, brokerurl)  ;
                Brokers.Add(broker) ;
                
                Dictionary<string, object> args = new Dictionary<string, object>() ;
                args.Add("_class", "agent") ;
                args.Add("_broker", broker) ;
                this.GetObjects(args) ;
        }
        
        public void RemoveBroker(Broker broker) {
        	if (Brokers.Contains(broker)) {
        		Brokers.Remove(broker) ;
        	}
        	
        	broker.Shutdown() ;	
        }
        
        public void Close() {
        	foreach (Broker broker in Brokers.ToArray()) {
        		this.RemoveBroker(broker) ;
        	}
        }

		protected BrokerURL GetBrokerURL(string url) {
			return new BrokerURL(url) ;
		}
	
	
		public List<QMFObject> GetObjects(Dictionary<string, object> args) {
			List<Broker> brokerList = null ;
			List<Agent> agentList = new List<Agent>() ;
		
			if (args.ContainsKey("_broker")) {
				brokerList = new List<Broker>() ;
				brokerList.Add((Broker)args["_broker"]) ;					
			} else {
				brokerList = this.Brokers ;
			}
			
			foreach (Broker broker in brokerList) {
				broker.WaitForStable() ;
			}
		
			if (args.ContainsKey("_agent")) {
				Agent agent = (Agent)args["_agent"] ;
				if (brokerList.Contains(agent.Broker)) {
					agentList.Add(agent) ;
				} else {
					throw new Exception("Agent is not managed by this console or the supplied broker") ;
				}
			} else {
				if (args.ContainsKey("_objectId")) {
					ObjectID oid = (ObjectID) args["_objectId"] ;
					foreach (Broker broker in Brokers) {
						foreach (Agent agent in broker.Agents.Values) {
							if ((agent.AgentBank == oid.AgentBank()) && (agent.BrokerBank == oid.BrokerBank())) {
								agentList.Add(agent) ;
							}
						}
					}				
				}
				else {
					foreach (Broker broker in brokerList) {
						foreach (Agent agent in broker.Agents.Values) {
							if (agent.Broker.IsConnected()) {
								agentList.Add(agent) ;
							}
						}
					}
				}
			}
			
			GetResult = new List<QMFObject>() ;				

			if (agentList.Count > 0) {
				//FIXME Add a bunch of other suff too
				foreach (Agent agent in agentList) {
					Dictionary<string, object> getParameters = new Dictionary<string, object>() ;				
					Broker broker = agent.Broker ;
					long seq = -1 ;
					lock(LockObject) {
						seq = SequenceManager.Reserve(Session.CONTEXT_MULTIGET) ;
						SyncSequenceList.Add(seq) ;
					}
					object packageName = null ;
					object className = null ;
					object key = null ;
					object sClass = null ;
					object oid = null ;
					object hash = null ;
					
					args.TryGetValue("_schema", out sClass) ;
					args.TryGetValue("_key", out key) ;
					args.TryGetValue("_class", out className) ;
					args.TryGetValue("_package", out packageName) ;
					args.TryGetValue("_objectID", out oid) ;
					args.TryGetValue("_hash", out hash) ;
					
					if ((className == null) && (oid == null) && (oid == null)) {
						throw new Exception("No class supplied, use '_schema', '_key', '_class', or '_objectId' argument") ;
					}
					
					if (oid != null) {
						getParameters.Add("_objectID", oid) ;
					}
					else {
						if (sClass != null) {
							key = key ?? ((SchemaClass)sClass).Key ;					
						}
						if (key != null) {
							ClassKey cKey = (ClassKey)key ;
							className = className ?? cKey.ClassName ;
							packageName = packageName ?? cKey.PackageName ;					
							hash = hash ?? cKey.Hash ;
						}
						
						if (packageName != null) {
							getParameters.Add("_package", packageName) ;
						}
						if (className != null) {
							getParameters.Add("_class", className) ;
						}					
						if (hash != null) {
							getParameters.Add("_hash", hash) ;
						}
						foreach (KeyValuePair<string, object> pair in args) {
							if (!pair.Key.StartsWith("_")) {
								getParameters.Add(pair.Key, pair.Value) ;
							}
						}
					}
					
					IEncoder enc = broker.CreateEncoder('G', seq) ;
					enc.WriteMap(getParameters) ;
					string routingKey = String.Format("agent.{0}.{1}", agent.BrokerBank, agent.AgentBank) ; 
					Message msg = broker.CreateMessage(enc, routingKey) ;
					log.Debug("Get Object Keys: ") ;
					foreach (string pKey in getParameters.Keys) {
						log.Debug(String.Format("\tKey: '{0}' Value: '{1}'", pKey, getParameters[pKey])) ;
					}
					broker.Send(msg) ;				
				}
	  
	  			int waittime = DEFAULT_GET_WAIT_TIME ;
	  			bool timeout = false ;
	  			if (args.ContainsKey("_timeout")) {
	  				waittime = (int) args["_timeout"] ;
	  			}			
	  			DateTime start = DateTime.Now ;
	  			lock (LockObject) {
	  			    // FIXME ERROR
	  				while (SyncSequenceList.Count > 0){
	  					Monitor.Wait(LockObject,waittime) ;
						TimeSpan duration = DateTime.Now - start;						
						if (duration.TotalMilliseconds > waittime) {
							foreach (long pendingSeq in SyncSequenceList) {
								SequenceManager.Release(pendingSeq) ;
							}
							SyncSequenceList.Clear() ;
							timeout = true ;
						}
	  				}
	  			}		
	  			
	  			//FIXME Add the error logic
				if ((GetResult.Count == 0) && timeout) {
					throw new Exception ("Get Request timed out") ;
				}
			}
			return GetResult ;
		}
		
		public List<string> GetPackages() {
			this.WaitForStable() ;
			List<string> returnValue = new List<string>() ;
			foreach (String name in Packages.Keys) {
				returnValue.Add(name) ;
			}
			
			return returnValue ;
		}
			
		public List<ClassKey> GetClasses(string packageName) {
			List<ClassKey> returnValue = new List<ClassKey>() ;
			this.WaitForStable() ;
			
			if (Packages.ContainsKey(packageName)) {
				foreach (SchemaClass sClass in Packages[packageName].Values) {
					returnValue.Add(sClass.Key) ;
				}
			} 
			
			return returnValue ;
		}
		
		public SchemaClass GetSchema(ClassKey key) {
			return GetSchema(key, true) ;
		}
		
		protected SchemaClass GetSchema(ClassKey key, bool waitForStable) {
			if (waitForStable) {
				this.WaitForStable() ;
			}
			
			SchemaClass returnValue = null ;
			
			try {
				returnValue = Packages[key.PackageName][key.GetKeyString()] ;
			}
			catch (KeyNotFoundException) {
				// eat it
			}
			
			return returnValue ;
		}
		
		
		protected void WaitForStable() {
			foreach (Broker broker in Brokers) {
				broker.WaitForStable() ;
			}			
		}
		
		
		public Broker GetBroker(long BrokerBank) {
			Broker returnValue = null ;
			
			foreach (Broker broker in Brokers) {
				if (broker.BrokerBank() == BrokerBank) {
					returnValue = broker ;
					break ;
				}
			}
			
			return returnValue ;
		}		
		
		public MethodResult InvokeMethod(QMFObject obj, string name, List<object> args, bool synchronous, int timeToLive) {
			Broker aBroker = this.GetBroker(obj.BrokerBank()) ;
			
			long seq = this.SendMethodRequest(obj, aBroker, name, args, synchronous, timeToLive) ;
			if (seq != 0) {
				if (!synchronous) {
					return null ;
				}
				try {
					aBroker.WaitForSync(timeToLive) ;
				} catch (Exception e) {
					SequenceManager.Release(seq) ;
					throw e ;
				}
				
				// FIXME missing error logic in the broker
				return (MethodResult) SyncResult ;
			}
			
			return null ;
		}
    			
		protected long SendMethodRequest(QMFObject obj, Broker aBroker, string name, List<object> args, bool synchronous, int timeToLive) {
			
			SchemaMethod method = obj.Schema.GetMethod(name) ;
			if (args == null) {
				args = new List<object>() ;
			}
			
			long seq = 0 ;
			if (method != null) {
				KeyValuePair<SchemaMethod, bool> pair = new KeyValuePair<SchemaMethod, bool>(method, synchronous) ;
				seq = SequenceManager.Reserve(pair) ;
				IEncoder enc = aBroker.CreateEncoder('M', seq) ;
				obj.ObjectID.encode(enc) ;
				obj.Schema.Key.encode(enc) ;
				enc.WriteStr8(name) ;
				
				if (args.Count < method.InputArgCount) {
					throw new Exception(String.Format("Incorrect number of arguments: expected {0}, got{1}", method.InputArgCount, args.Count)) ;
				}
				
				int argIndex = 0 ;
				foreach (SchemaArgument arg in method.Arguments) {
					if (arg.IsInput()) {;						
						this.EncodeValue(enc, arg.Type, args[argIndex]) ;
						argIndex += 1 ;
					} 
				}
				
				Message msg = aBroker.CreateMessage(enc,obj.RoutingKey(),timeToLive) ;
				
				if (synchronous) {
					aBroker.SetSyncInFlight(true) ;
				}
				aBroker.Send(msg) ;
			}
			return seq ;
		}
		
		public QMFObject MakeObject(ClassKey key) {
			SchemaClass sClass = this.GetSchema(key) ;
			if (sClass == null) {
				throw new Exception("No schema found for class " + key.ToString()) ;
			} 
			
			return this.CreateQMFObject(sClass, true, true, false) ;
		} 
		
		public QMFObject MakeObject(String keyString) {
			return this.MakeObject(new ClassKey(keyString)) ;
		} 		
			
		// Callback Methods
		public void HandleNewAgent(Agent agent) {
			if (Console != null) {
				Console.NewAgent(agent) ;
			}		
		}	
					
		public void HandleAgentRemoved(Agent agent) {
			if (Console != null) {
				Console.AgentRemoved(agent) ;
			}		
		}	
					
		public void HandleBrokerConnect(Broker broker) {
			if (Console != null) {
				Console.BrokerConnected(broker) ;
			}		
		}
		
		public void HandleBrokerDisconnect(Broker broker) {
			if (Console != null) {
				Console.BrokerDisconnected(broker) ;
			}		
		}			
	
		public void HandleBrokerResponse(Broker broker, IDecoder decoder, long sequence) {
			if (Console != null) {
				Console.BrokerInformation(broker) ;
			}				
		
		    long seq = SequenceManager.Reserve(CONTEXT_STARTUP) ;
			IEncoder endocder = broker.CreateEncoder('P', seq) ;
			broker.Send(endocder) ;
		}
	
		public void HandlePackageIndicator(Broker broker, IDecoder decoder, long sequence) {
			string packageName = decoder.ReadStr8() ;
			bool notify = false ;
			if (!Packages.ContainsKey(packageName)) {
				lock (LockObject) {
					Packages[packageName] = new Dictionary<string, SchemaClass>() ;
					notify = true ;
				}
			}
			
			if (notify && Console != null) {
				Console.NewPackage(packageName) ;
			}
			
			broker.IncrementOutstanding() ;
			long seq = SequenceManager.Reserve(Session.CONTEXT_STARTUP) ; 
			IEncoder enc = broker.CreateEncoder('Q', seq) ;			
			enc.WriteStr8(packageName) ;	
			broker.Send(enc) ;
		}		
		
		public void HandleCommandComplete(Broker broker, IDecoder decoder, long sequence) {
		
			long code = decoder.ReadUint32() ;
			string text = decoder.ReadStr8() ;
			Object context = this.SequenceManager.Release(sequence) ;
			
			if (context.Equals(CONTEXT_STARTUP)) {
				broker.DecrementOutstanding() ;
			} else {
			 	if ((context.Equals(CONTEXT_SYNC)) & broker.GetSyncInFlight()) {
					broker.SetSyncInFlight(false) ;
				} else {
					if (context.Equals(CONTEXT_MULTIGET) && SyncSequenceList.Contains(sequence)) {
						lock(LockObject) {
							SyncSequenceList.Remove(sequence) ;
							if (SyncSequenceList.Count == 0) {
								Monitor.PulseAll(LockObject) ;
							}
						}
					}
				}
			}
		}	
			
		public void HandleClassIndicator(Broker broker, IDecoder decoder, long sequence) {
			short kind = decoder.ReadUint8() ;	
			ClassKey classKey = new ClassKey(decoder) ;
			bool unknown = false ;
			
			
			lock (LockObject) {
				if (Packages.ContainsKey(classKey.PackageName)) {
					if (!Packages[classKey.PackageName].ContainsKey(classKey.GetKeyString())) {
						unknown = true ;
					}
				}
			}
			
			if (unknown) {
				broker.IncrementOutstanding() ;
				long seq = SequenceManager.Reserve(Session.CONTEXT_STARTUP) ; 
				IEncoder enc = broker.CreateEncoder('S', seq) ;			
				classKey.encode(enc) ;	
				broker.Send(enc) ;				
			}
		}		
		
		public void HandleMethodResponse(Broker broker, IDecoder decoder, long sequence) {	
			long code = decoder.ReadUint32() ;
			string text = decoder.ReadStr16() ;
			
			Dictionary<string, object> outArgs = new Dictionary<string, object>() ;
			object obj = SequenceManager.Release(sequence) ;
			
			if (obj == null) { 
				return ;
			}
			
			KeyValuePair<SchemaMethod, bool> pair = (KeyValuePair<SchemaMethod, bool>) obj  ;			
			if (code == 0) {
				foreach (SchemaArgument arg in pair.Key.Arguments) {
					if (arg.IsOutput()) {
						outArgs.Add(arg.Name, this.DecodeValue(decoder, arg.Type)) ;	
					}
				}
			}
			
			MethodResult result = new MethodResult(code, text, outArgs) ;
			
			if (pair.Value) {
				this.SyncResult = result;
				broker.SetSyncInFlight(false) ;
			}
			
			if (Console != null) {
				Console.MethodResponse(broker, sequence, result) ;
			}	
		}	
			
		public void HandleHeartbeatIndicator(Broker broker, IDecoder decoder, long sequence, IMessage msg) {
			if (Console != null) {
				long brokerBank = 1 ;
				long agentBank = 0 ;
				try {
					string routingKey = msg.DeliveryProperties.GetRoutingKey() ;
					if (routingKey != null) {
						agentBank = Agent.GetBrokerBank(routingKey) ;
						brokerBank = Agent.GetBrokerBank(routingKey) ;				
					}
				}
				catch (Exception e) {
					log.Warn("Internal QPID error", e) ;
				}
				
				string agentKey = Agent.AgentKey(agentBank, brokerBank) ;
				long timestamp = decoder.ReadUint64() ;
				if (broker.Agents.ContainsKey(agentKey)) {
					Agent agent = broker.Agents[agentKey] ;
					Console.HearbeatRecieved(agent, timestamp) ;
				}
			}
			
		}		
		
		public void HandleEventIndicator(Broker broker, IDecoder decoder, long sequence) {
			if (Console != null) {
				QMFEvent newEvent = new QMFEvent(this, decoder) ;
				Console.EventRecieved(broker, newEvent) ;
			}			
		}		
		
		public void HandleSchemaResponse(Broker broker, IDecoder decoder, long sequence) {
			short kind = decoder.ReadUint8() ;	
			ClassKey classKey = new ClassKey(decoder) ;
			SchemaClass sClass = new SchemaClass(kind, classKey, decoder, this) ;				
			lock(LockObject) {
				Dictionary<string, SchemaClass> classMappings = Packages[sClass.PackageName] ;
				classMappings.Remove(sClass.ClassKeyString) ;
				classMappings.Add(sClass.ClassKeyString, sClass) ;
			}
			
			SequenceManager.Release(sequence) ;
			broker.DecrementOutstanding() ;
			if (Console != null) {
				this.Console.NewClass(kind, classKey) ;
			}	
		}	
			
		public void HandleContentIndicator(Broker broker, IDecoder decoder, long sequence, bool hasProperties, bool hasStatistics) {
		
			ClassKey key = new ClassKey(decoder) ;
			SchemaClass sClass = null ;;
			lock (LockObject) {
				sClass = GetSchema(key, false) ;
			}
			if (sClass != null) {
			    QMFObject obj = this.CreateQMFObject(sClass, decoder, hasProperties, hasStatistics, true) ;
				
				if (key.PackageName.Equals("org.apache.qpid.broker") && key.ClassName.Equals("agent") && hasProperties) {
					broker.UpdateAgent(obj) ;
				}
				
				lock (LockObject) {
					if (SyncSequenceList.Contains(sequence)) {
						if (!obj.IsDeleted() && this.SelectMatch(obj)) {
							GetResult.Add(obj) ;
						}	
					}
				}
				
				if (Console != null) {
					if (hasProperties) {
						Console.ObjectProperties(broker, obj) ;
					}
					if (hasStatistics) {
						Console.ObjectStatistics(broker, obj) ;	
					}
				}
			}
		}
		
		public bool SelectMatch(QMFObject obj) {
			return true ;
		}
		
		public object DecodeValue(IDecoder dec, short type) {
		
		 	switch (type) {
		 		case 1: return dec.ReadUint8() ;        // U8
		 		case 2: return dec.ReadUint16() ;       // U16     
		 		case 3: return dec.ReadUint32() ;       // U32
		 		case 4: return dec.ReadUint64() ;       // U64 
		 		case 6: return dec.ReadStr8() ;         // SSTR
		 		case 7: return dec.ReadStr16() ;        // LSTR
		 		case 8: return dec.ReadDatetime() ;	    // ABSTIME
		 		case 9: return dec.ReadUint32() ;       // DELTATIME
		 		case 10: return new ObjectID(dec) ;		// ref
		 		case 11: return dec.ReadUint8() != 0 ;  // bool
		 		case 12: return dec.ReadFloat() ;       // float		
		 		case 13: return dec.ReadDouble() ;      // double		 
		 		case 14: return dec.ReadUuid() ;	    // UUID			
		 		case 15: return dec.ReadMap() ;             // Ftable
		 		case 16: return dec.ReadInt8() ;        // int8
		 		case 17: return dec.ReadInt16() ;       // int16    
		 		case 18: return dec.ReadInt32() ;       // int32
		 		case 19: return dec.ReadInt64() ;       // int64 		 			
		 		case 20:                                // Object
		 			// Peek into the inner type code, make sure 
		 			// it is actually an object
		 			object returnValue = null ;
		 			short innerTypeCode = dec.ReadUint8() ;
		 			if (innerTypeCode != 20) {
		 				returnValue = this.DecodeValue(dec, innerTypeCode) ;
		 			}
		 			else {
		 				ClassKey classKey = new ClassKey(dec) ;
		 				lock(LockObject) {
		 					SchemaClass sClass = GetSchema(classKey) ;
		 					if (sClass != null) {
		 						returnValue = this.CreateQMFObject(sClass, dec, true, true, false) ;
		 					}
		 				}
		 			}
		 			return returnValue;
                case 21:                                 // List
		 	        {
		 	            MSDecoder lDec = new MSDecoder();
		 	            lDec.Init(new MemoryStream(dec.ReadVbin32()));
		 	            long count = lDec.ReadUint32();
		 	            List<object> newList = new List<object>();
		 	            while (count > 0)
		 	            {
		 	                short innerType = lDec.ReadUint8();
		 	                newList.Add(this.DecodeValue(lDec, innerType));
		 	                count -= 1;
		 	            }
		 	            return newList;
		 	        }
                case 22:							    // Array
		 	        {
		 	            MSDecoder aDec = new MSDecoder();
		 	            aDec.Init(new MemoryStream(dec.ReadVbin32()));
		 	            long cnt = aDec.ReadUint32();
		 	            short innerType = aDec.ReadUint8();
		 	            List<object> aList = new List<object>();
		 	            while (cnt > 0)
		 	            {
		 	                aList.Add(this.DecodeValue(aDec, innerType));
		 	                cnt -= 1;
		 	            }
		 	            return aList;
		 	        }
		 	    default: 
		 			throw new Exception(String.Format("Invalid Type Code: {0}", type)) ;		
		 	}	
		 }		
		 
		 
		public void EncodeValue(IEncoder enc, short type, object val) {
			try {
			 	switch ((int)type) {
			 		case 1: enc.WriteUint8((short) val) ; break;       // U8
			 		case 2: enc.WriteUint16((int) val) ;  break;       // U16     
			 		case 3: enc.WriteUint32((long) val) ; break;       // U32
			 		case 4: enc.WriteUint64((long) val) ; break;       // U64 
			 		case 6: enc.WriteStr8((string) val) ; break;       // SSTR
			 		case 7: enc.WriteStr16((string) val) ; break;      // LSTR
			 		case 8: enc.WriteDatetime((long) val); break;	   // ABSTIME
			 		case 9: enc.WriteUint32((long) val);   break;      // DELTATIME
			 		case 10: ((ObjectID)val).encode(enc) ; break;	   // ref
			 		case 11: 
			 			if ((bool) val) {
			 				enc.WriteUint8(1) ;
			 			} else {
			 				enc.WriteUint8(0) ;
			 			}
			 			break ;
			 		case 12: enc.WriteFloat((float) val); break;	   // FLOAT
			 		case 13: enc.WriteDouble((double) val);   break;   // DOUBLE			 			
			 		case 14: enc.WriteUuid((UUID) val) ; break ;	   // UUID		
			 		case 15: enc.WriteMap((Dictionary<string, object>) val) ; break ;  // Ftable
			 		case 16: enc.WriteInt8((short) val) ; break;       // int8
			 		case 17: enc.WriteInt16((int) val) ;  break;       // int16     
			 		case 18: enc.WriteInt32(long.Parse(""+ val)) ; break;       // int32
			 		case 19: enc.WriteInt64(long.Parse("" + val)) ; break;       // int64 
			 		case 20: 									       // Object
			 			// Check that the object has a session, if not
			 			// take ownership of it
			 			QMFObject qObj = (QMFObject) val ;
			 			if (qObj.Session == null) {
			 				qObj.Session = this ;
			 			}
			 			qObj.Encode(enc) ; 
			 			break;      
			 		case 21:                                             // List	
			 			List<object> items = (List<object>) val ;
			 			MSEncoder lEnc = new MSEncoder(1) ;
			 			lEnc.Init() ;			 			
			 			lEnc.WriteUint32(items.Count) ;
			 			foreach (object obj in items) {
			 				short innerType = Util.QMFType(obj) ;
			 				lEnc.WriteUint8(innerType) ;
			 				this.EncodeValue(lEnc,innerType,obj) ;		 				
			 			}
			 			enc.WriteVbin32(lEnc.Segment().ToArray()) ;
			 			break ;
			 		case 22:							                 // Array
			 			List<object> aItems = (List<object>) val ;
			 			MSEncoder aEnc = new MSEncoder(1) ;
			 			aEnc.Init() ;			 						 			
			 			long aCount = aItems.Count ;	 			
			 			aEnc.WriteUint32(aCount) ;
			 			if (aCount > 0) {
			 				Object anObj = aItems[0] ;
			 				short innerType = Util.QMFType(anObj) ;
				 			aEnc.WriteUint8(innerType) ;
				 			foreach (object obj in aItems) {
				 				this.EncodeValue(aEnc,innerType,obj) ;		 				
				 			}
			 			}
			 			enc.WriteVbin32(aEnc.Segment().ToArray()) ;			 			
			 			break ;
			 		default: 
			 			throw new Exception(String.Format("Invalid Type Code: {0}", type)) ;			 			
			 	}	
			 }
			 catch (System.InvalidCastException e) {
			 	string msg = String.Format("Class cast exception for typecode {0}, type {1} ", type, val.GetType()) ;
			 	log.Error(msg) ;
			 	throw new Exception(msg + type, e) ;
			 }
		 }				 
		 
	    public List<string> BindingKeys() {
	    	List<string> bindings = new List<string>() ;
	    	bindings.Add("schema.#") ;
	    	if (RecieveObjects & RecieveEvents & RecieveHeartbeat & !UserBindings) {
	    		bindings.Add("console.#") ;
	    	}
	    	else {
	    		if (RecieveObjects & !UserBindings) {
	    			bindings.Add("console.obj.#") ;
	    		}
	    		else {
					bindings.Add("console.obj.*.*.org.apache.qpid.broker.agent") ;
	    		}
	    		if (RecieveEvents) {
	    			bindings.Add("console.event.#") ;
	    		}
	    		if (RecieveHeartbeat) {
	    			bindings.Add("console.heartbeat.#") ;
	    		}
	    	}
	    	return bindings ;
	    }
	    
	    protected QMFObject CreateQMFObject(SchemaClass schema, bool hasProperties, bool hasStats , bool isManaged) {
	    	Type realClass = typeof(QMFObject) ;	    	
	    	if (Console != null) {
	    		realClass = Console.TypeMapping(schema.Key) ;
	    	}
	    	Type[] types = new Type[] {typeof(Session), typeof(SchemaClass), typeof(bool), typeof(bool),typeof(bool)} ;
	    	object[] args = new object[] {this, schema, hasProperties, hasStats, isManaged} ;
	    	ConstructorInfo ci = realClass.GetConstructor(types);	
	    	return (QMFObject) ci.Invoke(args) ;
	    }	
	    
	    protected QMFObject CreateQMFObject(SchemaClass schema, IDecoder dec, bool hasProperties, bool hasStats , bool isManaged) {
	    	Type realClass = typeof(QMFObject) ;	    	
	    	if (Console != null) {
	    		realClass = Console.TypeMapping(schema.Key) ;    		
	    	}
	    	Type[] types = new Type[] {typeof(Session), typeof(SchemaClass), typeof(IDecoder), typeof(bool), typeof(bool),typeof(bool)} ;
	    	object[] args = new object[] {this, schema, dec, hasProperties, hasStats, isManaged} ;
	    	ConstructorInfo ci = realClass.GetConstructor(types);	    
	    	return (QMFObject) ci.Invoke(args) ;
	    }		    		 																		 																
	}
}
