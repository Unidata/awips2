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

import java.lang.management.ManagementFactory;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.handler.impl.QpidDomainObject;
import org.w3c.dom.Element;

import junit.framework.TestCase;

/**
 * Test case for Resource Metadata Descriptor Builder.
 * 
 * @author Andrea Gazzarini
 */
public class RmdBuilderTest extends TestCase
{
	private MBeanInfo _metadata;
	private RmdBuilder _builder;
	private ObjectName _objectName;
	
	@Override
	protected void setUp() throws Exception
	{
		MBeanServer server = ManagementFactory.getPlatformMBeanServer();
		_objectName = new ObjectName("Test:Name=QpidDomainObject");
		
		server.registerMBean(new QpidDomainObject(), _objectName);
		_metadata = server.getMBeanInfo(_objectName);
		
		_builder = new RmdBuilder();
		_builder.begin(_objectName);
		
		assertEquals(_objectName,_builder._objectName);
	}
	
	/**
	 * Tests the execution of the onOperation() method.
	 */
	public void testOnOperation() throws Exception
	{
		MBeanAttributeInfo [] attributes = _metadata.getAttributes();
		for (MBeanAttributeInfo attribute : attributes)
		{
			_builder.onAttribute(attribute);
		}
		
		Element [] rmd = _builder.getResourceMetadataDescriptor();
		
		assertEquals(attributes.length,rmd.length);
		
		for (MBeanAttributeInfo attribute: _metadata.getAttributes())
		{
			Element propertyMetadataDescriptor = getPropertyMetadatDescriptor(attribute.getName(), rmd);
			
			String modifiability = propertyMetadataDescriptor.getAttribute(Names.MODIFIABILITY);
			String expectedValue = 
				attribute.isWritable() 
					? Names.READ_WRITE 
					: Names.READ_ONLY;
			assertEquals(expectedValue,modifiability);
		}
	}

	/**
	 * Returns the property metadata descriptor associated with the given attribute name.
	 * 
	 * @param name the attribute name.
	 * @param rmd the resource metadata descriptor.
	 * @return the property metadata descriptor associated with the given attribute name.
	 * @throws RuntimeException if metadata for the given attribute is not found.
	 */
	private Element getPropertyMetadatDescriptor(String name, Element [] rmd)
	{
		for (Element propertyMetadataDescriptor : rmd)
		{
			if ((Names.PREFIX+":"+name).equals(
					propertyMetadataDescriptor.getAttribute(Names.NAME_ATTRIBUTE)))
			{
				return propertyMetadataDescriptor;
			}
		}
		throw new RuntimeException("Property MetadataDescriptor not found for attribute "+name);
	}
}