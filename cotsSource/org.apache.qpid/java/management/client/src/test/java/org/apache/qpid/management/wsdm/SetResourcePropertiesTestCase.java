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
package org.apache.qpid.management.wsdm;

import java.lang.reflect.Array;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.management.MBeanAttributeInfo;
import javax.xml.namespace.QName;

import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.muse.ws.resource.WsrfConstants;
import org.apache.qpid.management.Names;

/**
 * Test case for Set Resource Properties interfaces.
 * 
 * @author Andrea Gazzarini
 */
public class SetResourcePropertiesTestCase extends BaseWsDmAdapterTestCase
{
	/**
	 * Test the WS-RP SetResourceProperty interface of the WS-DM adapter.
	 * 
	 * <br>precondition : a WS-Resource exists and is registered. 
	 * <br>postcondition : property values are correctly updated on the target WS-Resource..
	 */
	public void testSetResourcePropertiesOK() throws Exception
	{
		Map<String, Object> sampleMap = new HashMap<String, Object>();
		sampleMap.put("Key1", "BLABALABLABALBAL");
		sampleMap.put("Key2", 182838484l);
		sampleMap.put("Key3", -928376362);
		sampleMap.put("Key4", 23762736276.33D);
		sampleMap.put("Key4", 2327363.2F);
		
		Map<String, Object> sampleValues = new HashMap<String, Object>();
		sampleValues.put(String.class.getName(),"SAMPLE_STRING");
		sampleValues.put(UUID.class.getName(),UUID.randomUUID());
		sampleValues.put(Boolean.class.getName(),Boolean.FALSE);
		sampleValues.put(Map.class.getName(),sampleMap);
		sampleValues.put(Long.class.getName(),283781273L);
		sampleValues.put(Integer.class.getName(),12727);
		sampleValues.put(Short.class.getName(),new Short((short)22));
		sampleValues.put(Date.class.getName(),new Date());
		
		MBeanAttributeInfo [] attributesMetadata = _mbeanInfo.getAttributes();
		boolean atLeastThereIsOneWritableProperty = false;
		
		for (MBeanAttributeInfo attributeMetadata : attributesMetadata)
		{
			String name = attributeMetadata.getName();
			
			if (attributeMetadata.isWritable())
			{	
				atLeastThereIsOneWritableProperty = true;
				Object sampleValue = sampleValues.get(attributeMetadata.getType());
				Object []values = new Object[]{sampleValue};
				
				Object result = _managementServer.getAttribute(_resourceObjectName, name);
				if (result == null)
				{
					_resourceClient.insertResourceProperty(	
							new QName(
									Names.NAMESPACE_URI,
									name,
									Names.PREFIX),
							values);
				} else 
				{
					_resourceClient.updateResourceProperty(	
							new QName(
									Names.NAMESPACE_URI,
									name,
									Names.PREFIX),
							values);					
				}
				
				Object propertyValues = _resourceClient.getPropertyAsObject(
						new QName(
								Names.NAMESPACE_URI,
								name,
								Names.PREFIX),
						Class.forName(attributeMetadata.getType()));
				int length = Array.getLength(propertyValues);
				if (length != 0)
				{
					Object propertyValue = Array.get(propertyValues, 0);
					
					assertEquals(
							"Comparison failed for property "+name,
							sampleValue,
							propertyValue);
				} else {
					assertNull(
							String.format(
									"\"%s\" property value shouldn't be null. Its value is %s",
									name,
									_managementServer.getAttribute(_resourceObjectName,name)),
									sampleValue);
				}				
			}
		}
		assertTrue(
				"It's not possibile to run successfully this test case if " +
					"the target WS-Resource has no at least one writable property",
				atLeastThereIsOneWritableProperty);
	}

	/**
	 * Test the WS-RP SetResourceProperty interface of the WS-DM adapter when the 
	 * target property is null.
	 * According to WS-RP specs this operation is not allowed because in this case a SetResourceProperty with an "Insert"
	 * message should be sent in order to initialize the property.
	 * 
	 * <br>precondition : a ws resource exists and is registered. The value of the target property is null. 
	 * <br>postcondition : a Soap fault is received indicating the failuire.
	 */
	public void testSetResourcePropertiesKO() throws Exception
	{
		Object typePropertyValue = _managementServer.getAttribute(_resourceObjectName, "Type");
		assertNull(typePropertyValue);
		
		try 
		{		
			_resourceClient.updateResourceProperty(
					new QName(
							Names.NAMESPACE_URI,
							"Type",
							Names.PREFIX),
					new Object[]{"sampleValue"});					
			fail(
					"If the property is null on the target ws resource, according " +
					"to WS-RP specs, an update of its value is not possible.");
		} catch(SoapFault expected)
		{
			
		}
	}
	
	/**
	 * Tests the SetResourceProperties (update) interface when the request contains 
	 * an unknwon target resource.
	 * 
	 * <br>precondition : the SetResourceProperties contains an unknwon resource.
	 * <br>postcondition : a SoapFault is thrown and the corresponding detail contains an 
	 * 								UnknownResourceFault element.
	 */
	public void testUpdateResourcePropertiesKO_WithUnknownResourceFault() throws Exception
	{
		try 
		{
			_resourceClient.getEndpointReference().removeParameter(Names.RESOURCE_ID_QNAME);
			_resourceClient.getEndpointReference().addParameter(Names.RESOURCE_ID_QNAME,"lablabalbal");
			
			_resourceClient.updateResourceProperty(
					new QName(
							Names.NAMESPACE_URI,
							"Type",
							Names.PREFIX),
					new Object[]{"sampleValue"});		
		} catch(SoapFault expected)
		{
			assertEquals(
					WsrfConstants.RESOURCE_UNKNOWN_QNAME.getLocalPart(),
					expected.getDetail().getLocalName());
		}
	}
	
	/**
	 * Tests the SetResourceProperties (insert) interface when the request contains 
	 * an unknwon target resource.
	 * 
	 * <br>precondition : the SetResourceProperties contains an unknwon resource.
	 * <br>postcondition : a SoapFault is thrown and the corresponding detail contains an 
	 * 								UnknownResourceFault element.
	 */
	public void testInsertResourcePropertiesKO_WithUnknownResourceFault() throws Exception
	{
		try 
		{
			_resourceClient.getEndpointReference().removeParameter(Names.RESOURCE_ID_QNAME);
			_resourceClient.getEndpointReference().addParameter(Names.RESOURCE_ID_QNAME,"lablabalbal");
			
			_resourceClient.insertResourceProperty(
					new QName(
							Names.NAMESPACE_URI,
							"Type",
							Names.PREFIX),
					new Object[]{"sampleValue"});		
		} catch(SoapFault expected)
		{
			assertEquals(
					WsrfConstants.RESOURCE_UNKNOWN_QNAME.getLocalPart(),
					expected.getDetail().getLocalName());
		}		
	}	
}
