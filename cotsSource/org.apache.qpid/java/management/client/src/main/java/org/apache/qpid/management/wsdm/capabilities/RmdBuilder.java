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

import java.util.ArrayList;
import java.util.List;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanOperationInfo;
import javax.management.ObjectName;

import org.apache.muse.core.Environment;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.resource.metadata.WsrmdConstants;
import org.apache.qpid.management.Names;
import org.apache.qpid.qman.debug.WsdlDebugger;
import org.w3c.dom.Element;

/**
 * Resource Metadata Descriptor Builder.
 * It is used for build the metadata descriptor for properties of the 
 * incoming jmx object. 
 * 
 * @author Andrea Gazzarini
 */
class RmdBuilder implements IArtifactBuilder
{
	private List<Element> _metadataDescriptor = new ArrayList<Element>();
	
	ObjectName _objectName;
	
	/**
	 * Nothing to be done here on this builder. 
	 * Simply logs a message indicating the target object name.
	 * 
	 * @param objectName the jmx name of the object that is going to be processed.
	 */
	public void begin(ObjectName objectName)
	{
		this._objectName = objectName;
	}
	
	/**
	 * Nothing to be done here on this builder. 
	 * 
	 * @throws BuilderException never.
	 */
	public void endAttributes()
	{
		// N.A. for this builder.
	}
	
	/**
	 * Nothing to be done here on this builder. 
	 * 
	 * @throws BuilderException never.
	 */
	public void endOperations()
	{
		// N.A. for this builder.
	}

	/**
	 * Process a single attribute metadata.
	 * An attribute (that is, a property) represented by the corresponding incoming 
	 * attribute metadata will generate an wsrmd:Property xml element with the constraints
	 * (initial values, static values, allowed values) contained on the metadata.
	 * 
	 * @param attributeMetadata the attribute (jmx) metadata.
	 */
	public void onAttribute(MBeanAttributeInfo attributeMetadata)
	{
		Element property = XmlUtils.createElement(WsrmdConstants.PROPERTY_QNAME);
		property.setAttribute(Names.NAME_ATTRIBUTE, Names.PREFIX+":"+attributeMetadata.getName());
		property.setAttribute(Names.MODIFIABILITY, 
				attributeMetadata.isWritable() 
					? Names.READ_WRITE 
					: Names.READ_ONLY);
		property.setAttribute(Names.MUTABILITY,Names.MUTABLE);
		
		WsdlDebugger.debug(_objectName, property);
		
		_metadataDescriptor.add(property);
	}

	/**
	 * Nothing to be done here on this builder. 
	 * 
	 * @throws BuilderException never.
	 */	
	public void onOperation(MBeanOperationInfo operation)
	{
		// N.A. for this builder
	}

	/**
	 * Nothing to be done here on this builder. 
	 * 
	 * @throws BuilderException never.
	 */
	public void setEnvironment(Environment environment) 
	{
		// N.A. for this builder
	}

	/**
	 * Nothing to be done here on this builder. 
	 * 
	 * @throws BuilderException never.
	 */
	public Element[] getResourceMetadataDescriptor() 
	{
		Element [] properties = _metadataDescriptor.toArray(
				new Element[_metadataDescriptor.size()]);
		return properties;
	}
}