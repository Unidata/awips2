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
using org.apache.qpid.transport;
using org.apache.qpid.transport.codec ;

using log4net ;

namespace org.apache.qpid.console
{
	
	/**
	 * Metadata that describes the mapping of a class to a QMF Object
	 */	
	public class SchemaClass
	{
		public static int CLASS_KIND_TABLE = 1 ;
		public static int CLASS_KIND_EVENT = 2 ;
		
		public static ILog log = LogManager.GetLogger(typeof(SchemaClass)) ;		
		
		
		public ClassKey Key {get;set;}
		public ClassKey SuperType {get;set;}
		public int Kind {get;set;}
		public List<SchemaMethod> Methods = new List<SchemaMethod>() ;
		public List<SchemaArgument> Arguments = new List<SchemaArgument>() ;
		public List<SchemaProperty> Properties = new List<SchemaProperty>() ;
		public List<SchemaStatistic> Statistics = new List<SchemaStatistic>() ;
		
		public string ClassName { get { return Key.ClassName;}}
		public string PackageName { get { return Key.PackageName;}}		
		public string ClassKeyString { get { return Key.GetKeyString();}}		
		
		protected Session Session {get;set;}				
		
		public SchemaClass(int kind, ClassKey key, IDecoder dec, Session session)
		{
			log.Debug(String.Format("New schema class {0}", key)) ;
		    Kind = kind ;
		    Session = session ;
		    this.Key = key ;   
		    bool hasSupertype = false ;
		 
		 	if (kind == CLASS_KIND_TABLE) {
				int propCount = dec.ReadUint16() ;
				int statCount = dec.ReadUint16() ;
				int methodCount = dec.ReadUint16() ;
				
				if (hasSupertype) {		
			 		SuperType = new ClassKey(dec) ;
			 	}
				
				for(int x = 0 ; x < propCount ; x++) {
					Properties.Add(new SchemaProperty(dec)) ;
				}
				for(int x = 0 ; x < statCount ; x++) {
					Statistics.Add(new SchemaStatistic(dec)) ;
				}	
				for(int x = 0 ; x < methodCount ; x++) {
					Methods.Add(new SchemaMethod(dec)) ;
				}	
			}
			
			if (kind == CLASS_KIND_EVENT) {			
				int argCount = dec.ReadUint16() ;		
				if (hasSupertype) {
			 		SuperType = new ClassKey(dec) ;
			 	}				
				for(int x = 0 ; x < argCount ; x++) {
					Arguments.Add(new SchemaArgument(dec, false)) ;
				}	
			}
		}
		
		public SchemaMethod GetMethod(string name) {
			SchemaMethod returnValue = null ;
			foreach(SchemaMethod method in Methods) {
				if (method.Name.Equals(name)) {
					returnValue = method ;
					break ;
				}
			}
			return returnValue ;
		}
		
		public List<SchemaProperty> GetAllProperties() {
			if (SuperType == null) {
				return Properties ;
			} else {
				List<SchemaProperty> allProperties = new List<SchemaProperty>(Properties) ;
				allProperties.AddRange(Session.GetSchema(SuperType).GetAllProperties()) ;
				return allProperties ;
			}
		}
		
		public List<SchemaStatistic> GetAllStatistics() {
			if (SuperType == null) {
				return Statistics ;
			} else {
				List<SchemaStatistic> allStats = new List<SchemaStatistic>(Statistics) ;
				allStats.AddRange(Session.GetSchema(SuperType).GetAllStatistics()) ;
				return allStats ;
			}
		}
		
		public List<SchemaMethod> GetAllMethods() {
			if (SuperType == null) {
				return Methods ;
			} else {
				List<SchemaMethod> allMethods = new List<SchemaMethod>(Methods) ;
				allMethods.AddRange(Session.GetSchema(SuperType).GetAllMethods()) ;
				return allMethods ;
			}
		}			
		
		public bool HasSuperType() {
			return SuperType != null ;
		}
	}
}
