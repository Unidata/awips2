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
import javax.management.MBeanInfo;
import javax.management.MBeanOperationInfo;
import javax.management.ObjectName;

import org.apache.muse.core.Environment;
import org.apache.muse.core.Resource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Director used for coordinate the building process of WS-DM artifacts.
 * 
 * @author Andrea Gazzarini
 */
final class WSDMArtifactsDirector 
{
	private final ObjectName _eventSourceObjectName;
	private final MBeanInfo _metadata;
	
	private final MBeanCapabilityBuilder _capabilityBuilder;
	private final WsdlBuilder _wsdlBuilder;
	private final RmdBuilder _rmdBuilder;
	
	/**
	 * Builds a new director with the given objectname and (jmx) metadata.
	 * 
	 * @param eventSourceObjectName the object name of the event source mbean.
	 * @param metadata the jmx metadata of the corresponding mbean.
	 */
	WSDMArtifactsDirector(ObjectName eventSourceObjectName, MBeanInfo metadata) 
	{
		this._eventSourceObjectName = eventSourceObjectName;
		this._metadata = metadata;
		
		_wsdlBuilder = new WsdlBuilder();
		_capabilityBuilder = new MBeanCapabilityBuilder();
		_rmdBuilder = new RmdBuilder();
	}

	/**
	 * Starts the build process of this director.
	 * This method acts as a facade of the whole build process.
	 * 
	 * @throws BuilderException when one step of the build process fails.
	 */
	void direct() throws BuilderException 
	{
		processObjectName();
		processAttributes(); 
		endAttributes();
		processOperations();
		endOperations();
	}

	/**
	 * Notifiies builder that all the operations metadata have been transmitted.
	 * 
	 * @throws BuilderException when one builder raises an exception during this operation.
	 */
	private void endOperations() throws BuilderException 
	{
		_capabilityBuilder.endOperations();
		_wsdlBuilder.endOperations();
		_rmdBuilder.endOperations();
	}

	/**
	 * Notifiies builder that all the attributes metadata have been transmitted.
	 * 
	 * @throws BuilderException when one builder raises an exception during this operation.
	 */
	private void endAttributes() throws BuilderException
	{
		_capabilityBuilder.endAttributes();
		_wsdlBuilder.endAttributes();
		_rmdBuilder.endAttributes();
	}

	/**
	 * Injects event source object name on all builders.
	 * 
	 * @throws BuilderException when one builder raises an exception during this operation.
	 */
	void processObjectName() throws BuilderException 
	{
		_capabilityBuilder.begin(_eventSourceObjectName);
		_wsdlBuilder.begin(_eventSourceObjectName);
		_rmdBuilder.begin(_eventSourceObjectName);
	}
	
	/**
	 * Injects attributes metadata on all builders.
	 * 
	 * @throws BuilderException when one builder raises an exception during this operation.
	 */
	void processAttributes() throws BuilderException 
	{
		for (MBeanAttributeInfo attribute : _metadata.getAttributes()) 
		{
			_capabilityBuilder.onAttribute(attribute);
			_wsdlBuilder.onAttribute(attribute);		
			_rmdBuilder.onAttribute(attribute);
		}
	}
	
	/**
	 * Injects operations metadata on all builders.
	 * 
	 * @throws BuilderException when one builder raises an exception during this operation.
	 */
	void processOperations() throws BuilderException
	{
		for (MBeanOperationInfo operation : _metadata.getOperations()) 
		{
			_capabilityBuilder.onOperation(operation);
			_wsdlBuilder.onOperation(operation);			
		}
	}	
	
	/**
	 * Returns the capabilty class.
	 * 
	 * @return the capability class.
	 */
	Class<MBeanCapability> getCapabilityClass() 
	{
		return _capabilityBuilder.getCapabilityClass();
	}

	/**
	 * Returns the wsdl.
	 * 
	 * @return the wsdl.
	 */
	Document getWsdl() 
	{
		return _wsdlBuilder.getWsdl();
	}

	/**
	 * Returns the resource metadata descriptor containing metadata (rules, constraints, etc)
	 * for the current resource.
	 * The returned object is an array of Element and each of them maps a resource property.
	 * 
	 * @return the resource metadata descriptor (as an array of Element).
	 */
	Element [] getResourceMetadataDescriptor() 
	{
		return _rmdBuilder.getResourceMetadataDescriptor();
	}
	
	/**
	 * Injects the environment on this director.
	 * 
	 * @param environment the QMan environment.
	 */
	void setEnvironment(Environment environment) 
	{
		_wsdlBuilder.setEnvironment(environment);
		_capabilityBuilder.setEnvironment(environment);	
		_rmdBuilder.setEnvironment(environment);
	}

	/**
	 * Injectcs the ws resource on this director.
	 * 
	 * @param resource the ws resource.
	 */
	public void setResource(Resource resource) 
	{
		_wsdlBuilder.setWsdlPath(resource.getWsdlPath());
	}
}