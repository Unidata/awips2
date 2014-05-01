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
using log4net ;
using org.apache.qpid.transport.codec ;

namespace org.apache.qpid.console
{
	
	/**
	 * An object which is returned from an agent by the Session. It can have
	 * methods, properties, and statistics.
	 */
	public class QMFObject
	{
	
		public static ILog log = LogManager.GetLogger(typeof(QMFObject)) ;	

 		public Session Session  {get;set;}
 		protected SchemaClass _Schema ;
 		virtual public SchemaClass Schema {get {return _Schema;} set {_Schema=value;}}
 		bool Managed ;
 		public DateTime CurrentTime {get;set;}
		public DateTime CreateTime {get;set;} 		
		public DateTime DeleteTime {get;set;}		
		public ObjectID ObjectID {get;set;}
		public Dictionary<string, object> Properties = new Dictionary<string, object>() ;
		public Dictionary<string, object> Statistics = new Dictionary<string, object>() ;		
			
 		
 		// This constructor is the "naked" constructor which creates
 		// an object without a session or a schema. It is used by
 		// subclasses which are auto generated
 		public QMFObject() {
 		}
 		
 		public QMFObject(QMFObject source) {
 			this.Session = source.Session ;
 			this.Schema = source.Schema ;
 			this.Managed = source.Managed ;
 			this.CurrentTime = source.CurrentTime ;
 			this.CreateTime = source.CreateTime ;
 			this.DeleteTime = source.DeleteTime ;
 			this.ObjectID = source.ObjectID ;
 			this.Properties = source.Properties ;
 			this.Statistics = source.Statistics ;
 		}
 		
 		// This constructor is used by a session make object call to 
 		// create a blank object from a schema.
 		public QMFObject(Session session, SchemaClass schema, bool hasProperties, bool hasStats , bool isManaged) {
			Session = session ;
			Schema = schema ;
			Managed = isManaged ; 
			
			if (hasProperties) {
				foreach (SchemaProperty prop in Schema.GetAllProperties()) {
					object propValue = null ; 
					if (!prop.Optional) {
						propValue = Util.DefaultValue(prop.Type) ;
					}
					this.SetProperty(prop.Name, propValue) ;			
				}
			}	
			
			if (hasStats) {
				foreach (SchemaStatistic stat in Schema.Statistics)  {			
					SetStatistic(stat.Name, Util.DefaultValue(stat.Type)) ;
				}
			}															
 		}
 		
 		// This constructor is used by the session to create an object based on a data 
 		// stream by the agent.
		public QMFObject(Session session, SchemaClass schema, IDecoder dec, bool hasProperties, bool hasStats , bool isManaged)
		{
			Session = session ;
			Schema = schema ;
			Managed = isManaged ;
			
			if (Managed) {
			    // FIXME DateTime or Uint64??
				CurrentTime = new DateTime(dec.ReadDatetime()) ;
				CreateTime = new DateTime(dec.ReadDatetime()) ;				
				DeleteTime = new DateTime(dec.ReadDatetime()) ;				
				ObjectID = new ObjectID(dec) ;
			}
			
			if (hasProperties) {
				List<string> excluded = ProcessPresenceMasks(dec, Schema) ;
				
				foreach (SchemaProperty prop in Schema.GetAllProperties()) {
					if (excluded.Contains(prop.Name)) {
					    log.Debug(String.Format("Setting Property Default {0}", prop.Name)) ;					    					
						safeAddProperty(prop.Name, null) ;	
					} else {
						//log.Debug(String.Format("Setting Property {0}", prop.Name)) ;
						safeAddProperty(prop.Name, session.DecodeValue(dec, prop.Type)) ;
					}
				}
			}
			
			if (hasStats) {
				foreach (SchemaStatistic stat in Schema.GetAllStatistics())  {
					//log.Debug(String.Format("Setting Statistic {0}", stat.Name)) ;				
					Statistics.Add(stat.Name, session.DecodeValue(dec, stat.Type)) ;
				}
			}
			
		}

		protected List<string> ProcessPresenceMasks(IDecoder dec, SchemaClass schema) {
			List<string> excludes = new List<string> () ;
			short bit = 0 ;
			short mask = 0 ;
			foreach (SchemaProperty prop in Schema.GetAllProperties()) {
				if (prop.Optional) {
					//log.Debug(String.Format("Property named {0} is optional", prop.Name)) ;				
					if (bit == 0) {
						mask=dec.ReadUint8() ;
						bit = 1 ;
					}

					if ((mask & bit) == 0) {
						//log.Debug(String.Format("Property named {0} is not present", prop.Name)) ;
						excludes.Add(prop.Name) ;
					}
					bit *= 2 ;
					if (bit == 256) {
						bit = 0 ;
					}
				}
			}
			return excludes ;			
		}
		
		protected List<SchemaMethod> getMethods() {
			return Schema.GetAllMethods() ;
		}
		
		public object GetProperty(string attributeName) {
			//FIXME any object refs?
			object returnValue = null ;
			Properties.TryGetValue(attributeName, out returnValue) ;
			return returnValue ;
		}
		
		protected void SetStatistic(string attributeName, object newValue) {
			Statistics.Remove(attributeName) ;
			Statistics.Add(attributeName, newValue) ;
		}
		
		public void SetProperty(string attributeName, object newValue) {
			Properties.Remove(attributeName) ;
			Properties.Add(attributeName, newValue) ;
		}		
		
		public bool IsDeleted() {
			return !DeleteTime.Equals(new DateTime(0)) ;
		}
		
		public string RoutingKey() {
			return ObjectID.RoutingCode() ;
		}
		
		public long AgentBank() {
			return ObjectID.AgentBank() ;
		}
		
		public long BrokerBank() {
			return ObjectID.BrokerBank() ;
		}
				
		override public string ToString() {
			string propertyString = "" ;
			foreach (KeyValuePair<string, object> pair in Properties) {
				propertyString = propertyString + String.Format("(Name: '{0}' Value: '{1}')", pair.Key, pair.Value) ;
			}
			
			string statsString = "" ;
			foreach (KeyValuePair<string, object> sPair in Statistics) {
				statsString = statsString + String.Format("(Name: '{0}' Value: '{1}')", sPair.Key, sPair.Value) ;
			}			
			if (Managed) {
				return String.Format("Managed QMFObject {0}:{1}({2}) Properties: [{3}] Statistics: [{4}])", Schema.PackageName, Schema.ClassName, ObjectID, propertyString, statsString) ;
			} else {
				return String.Format("QMFObject {0}:{1} Properties: [{2}] Statistics: [{3}]", Schema.PackageName, Schema.ClassName, propertyString, statsString) ;
			}
		}
		
		public MethodResult InvokeMethod(string name, params object[] args) {
			return this.InternalInvokeMethod(name, new List<object>(args), true, Broker.SYNC_TIME ) ;
		}
		
		public MethodResult InvokeMethod(string name, int timeToLive, params object[] args) {
			return this.InternalInvokeMethod(name, new List<object>(args), true, timeToLive) ;
		}
		
		public MethodResult InvokeMethod(string name, bool synchronous, int timeToLive, params object[] args) {
			return this.InternalInvokeMethod(name, new List<object>(args), synchronous, timeToLive ) ;
		}
				
		public MethodResult InvokeMethod(string name, bool synchronous, params object[] args) {
			return this.InternalInvokeMethod(name, new List<object>(args), synchronous, Broker.SYNC_TIME) ;
		}
		
		protected MethodResult InternalInvokeMethod(string name, List<object> args, bool synchronous, int timeToLive) {
			if (!Managed) {
				throw new Exception ("Object is not Managed") ;
			}
			if (Schema.GetMethod(name) == null) {
				throw new Exception (String.Format("Method named '{0}' does not exist", name))  ;
			}
			return Session.InvokeMethod(this, name, args, synchronous, timeToLive) ;
		}
		
		public void Encode (IEncoder enc) {
			int mask = 0 ;
			int bit = 0 ;
			List<SchemaProperty> propsToEncode = new List<SchemaProperty>() ;
			log.Debug(String.Format("Encoding class {0}:{1}", Schema.PackageName, Schema.ClassName)) ;
					
			enc.WriteUint8(20) ;
			Schema.Key.encode(enc) ;
			
			foreach (SchemaProperty prop in Schema.GetAllProperties()) {
				if (prop.Optional) {
					if (bit == 0) {
						bit =1 ;
					}
					if ((Properties.ContainsKey(prop.Name)) && (Properties[prop.Name] != null)) {
						mask |= bit ;
						propsToEncode.Add(prop) ;
					} else {
					}
					bit = bit << 1 ;
					if (bit == 256) {
						bit = 0 ;
						enc.WriteUint8((short)mask) ;
						mask = 0 ;
					}
				} else {
					propsToEncode.Add(prop) ;
				}
			}
			if (bit != 0) {
				enc.WriteUint8((short)mask) ;
			}		
			
			foreach (SchemaProperty prop in propsToEncode) {
				object obj = Properties[prop.Name] ;
				//log.Debug(String.Format("Encoding property {0}", prop.Name)) ;
				Session.EncodeValue(enc, prop.Type, obj) ;
			}
			foreach (SchemaStatistic stat in Schema.Statistics) {
				object obj = Statistics[stat.Name] ;
				Session.EncodeValue(enc, stat.Type, obj) ;
			}			
			log.Debug("Done") ;
		 
		 }
		 
		
		protected void safeAddProperty(string propName, object value) {
			if (Properties.ContainsKey(propName)) {
				Properties[propName] = value ;
			} else {
				Properties.Add(propName, value) ;
			}
		}
				 
		
	}
}
