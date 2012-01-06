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
package org.apache.qpid.management.wsdm.capabilities;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanOperationInfo;
import javax.management.ObjectName;

import org.apache.muse.core.Environment;

/**
 * Defines behaviour needed by WS-DM artifact builders.
 * Each concrete implementor must provide its own parser 
 * implementation of the given JMX data.
 * 
 * @author Andrea Gazzarini
 */
public interface IArtifactBuilder 
{	
	/**
	 * The build process begin and the given parameter is the
	 * object name of the corresponding JMX entity.
	 * 
	 * @throws BuilderException when the initialization fails.
	 */
	void begin(ObjectName objectName) throws BuilderException;
	
	/**
	 * Processes an attribute (its metadata) of the current MBean.
	 *  
	 * @param attributeMetadata the attribute metadata.
	 * @throws BuilderException when the builder cannot parse the given metadata.
	 */
	void onAttribute(MBeanAttributeInfo attributeMetadata) throws BuilderException;
	
	/**
	 * Processes an operation (its metadata) of the current MBean.
	 *  
	 * @param operationMetadata the operation metadata.
	 * @throws BuilderException when the builder cannot parse the given metadata.
	 */
	void onOperation(MBeanOperationInfo operationMetadata) throws BuilderException;
	
	/**
	 * Ends of the attributes section.
	 * 
	 * @throws BuilderException when the builder encounter a problem during this phase..
	 */
	void endAttributes() throws BuilderException;

	/**
	 * Ends of the operations section.
	 * 
	 * @throws BuilderException when the builder encounter a problem during this phase..
	 */
	void endOperations() throws BuilderException;
	
	/**
	 * Injects the adapter enviroment on this builder.
	 * 
	 * @param environment the adapter environment.
	 */
	void setEnvironment(Environment environment);
}