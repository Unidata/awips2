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
using org.apache.qpid.client ;

namespace org.apache.qpid.console
{
	
	
	public class XMLUtil
	{
		
		public static string CommonAttributes(SchemaVariable var) {
	    	string returnString = "" ;
		    if (var.Description != null){
		       returnString = returnString + String.Format(" desc='{0}'", var.Description) ;  
		    }       
		    
		    if (var.RefPackage != null){
		       returnString = returnString + String.Format(" refPackage='{0}'", var.RefPackage) ;  
		    }  	    
		    	    
		    if (var.RefClass != null){
		       returnString = returnString + String.Format(" refClass='{0}'", var.RefClass) ;  
		    }  	
		    
		    if (var.Unit != null){
		       returnString = returnString + String.Format(" unit='{0}'", var.Unit) ;  
		    }  		    
		    
		    if (var.Min != null){
		       returnString = returnString + String.Format(" min='{0}'", var.Min) ;  
		    }	       
		    if (var.Max != null){
		       returnString = returnString + String.Format(" max='{0}'", var.Max) ;  
		    }	       
		    if (var.MaxLength != null){
		       returnString = returnString + String.Format(" maxLength='{0}'", var.MaxLength) ;  
		    }
		    
		    return returnString ;
	      }
	
		public static string SchemaXML(Session sess, string packageName) {
			string returnValue = String.Format("<schema package='{0}'>\n", packageName) ;
			foreach (ClassKey key in sess.GetClasses(packageName)) {
				SchemaClass schema = sess.GetSchema(key) ;
				if (schema.Kind == 1) {
					if (schema.SuperType == null)
						returnValue += String.Format("\t<class name='{0}' hash='{1}'>\n", key.ClassName, key.GetHashString()) ;
                    else 
                    	returnValue += String.Format("\t<class name='{0}' hash='{1}' extends='{2}'>\n", key.ClassName, key.GetHashString(), schema.SuperType.GetKeyString()) ;						
					foreach (SchemaProperty prop in schema.Properties) {
						object[] attributes = new object[5] ;
						attributes[0] = prop.Name ;
						attributes[1] = Util.TypeName(prop.Type) ;
						attributes[2] = Util.AccessName(prop.Access) ;
						attributes[3] = prop.Optional ;
						attributes[4] = XMLUtil.CommonAttributes(prop);
						returnValue += String.Format("\t\t<property name='{0}' type='{1}' access='{2}' optional='{3}'{4}/>\n", attributes) ;
					}
					foreach (SchemaMethod meth in schema.Methods) {
						returnValue += String.Format("\t\t<method name='{0}'/>\n", meth.Name) ;	
						foreach (SchemaArgument arg in meth.Arguments) {
							object[] attributes = new object[4] ;					
							attributes[0] = arg.Name ;
							attributes[1] = arg.Direction ;
							attributes[2] = Util.TypeName(arg.Type) ;
							attributes[3] = XMLUtil.CommonAttributes(arg);					
							returnValue += String.Format("\t\t\t<arg name='{0}' dir='{1}' type='{2}'{3}/>\n", attributes) ;
						}
						returnValue += String.Format("\t\t</method>\n") ;
					}
					returnValue += String.Format("\t</class>\n") ;				
				} else {
					returnValue += String.Format("\t<event name='{0}' hash='{1}'>\n", key.ClassName, key.GetHashString()) ;	
					foreach (SchemaArgument arg in schema.Arguments) {
						object[] attributes = new object[4] ;					
						attributes[0] = arg.Name ;
						attributes[1] = Util.TypeName(arg.Type) ;
						attributes[2] = XMLUtil.CommonAttributes(arg);					
						returnValue += String.Format("\t\t\t<arg name='{0}' type='{1}'{2}/>\n", attributes) ;
					}
					returnValue += String.Format("\t</event>\n") ;
				}
			}
			returnValue += String.Format("</schema>\n") ;		
			
			return returnValue ;
		}		
		
		public static string SchemaXML(Session sess, string[] packageNames) {
			string returnValue = "<schemas>\n" ;
			foreach (string package in packageNames) {
				returnValue += XMLUtil.SchemaXML(sess, package) ;
				returnValue += "\n" ;
			}
			returnValue += "</schemas>\n" ;
			return returnValue ;
		}
		
		protected XMLUtil()
		{
		}
	}
}
