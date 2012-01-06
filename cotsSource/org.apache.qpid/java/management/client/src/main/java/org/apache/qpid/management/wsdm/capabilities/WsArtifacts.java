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

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Web Service Artifacts.
 * Basically it acts as a container for all artifacts built when a new WS-Resource is created.
 * With WS artifacts we mean :
 * 
 * <ul>
 * 	<li>Capability class (which encapsulate the WS-DM capability concern)</li>
 * 	<li>WS Resource Metadata Descriptor (RMD)</li>
 * 	<li>Web Service Description (WSDL)</li>
 * </ul>
 * 
 * @author Andrea Gazzarini
 */
class WsArtifacts {

	private final Class<MBeanCapability>_capabilityClass;
	private final Element[] _resourceMetadataDescriptor;
	private final Document _wsdl; 

	/**
	 * Builds a new artifacts container with the given artifacts.
	 * 
	 * @param capabilityClass the capability class.
	 * @param resourceMetadataDescriptor the resource metadata descriptor.
	 * @param wsdl the wsdl.
	 */
	public WsArtifacts(
			Class<MBeanCapability> capabilityClass,
			Element[] resourceMetadataDescriptor, 
			Document wsdl) 
	{
		this._capabilityClass = capabilityClass;
		this._resourceMetadataDescriptor = resourceMetadataDescriptor;
		this._wsdl = wsdl;
	}

	/**
	 * Returns the capability class.
	 * 
	 * @return the capability class.
	 */
	Class<MBeanCapability> getCapabilityClass() 
	{
		return _capabilityClass;
	}

	/**
	 * Returns the resource metadata descriptor.
	 * It is not a whole document but each property metadata is described in a 
	 * separated element so the returned object is an array of elements.
	 * 
	 * @return the resource metadata descriptor.
	 */
	Element[] getResourceMetadataDescriptor() 
	{
		return _resourceMetadataDescriptor;
	}

	/**
	 * Returns the web service description.
	 * 
	 * @return the web service description (WSDL).
	 */
	Document getWsdl() 
	{
		return _wsdl;
	}
}