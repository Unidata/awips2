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

import javax.management.MBeanAttributeInfo;
import javax.xml.namespace.QName;

import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.muse.ws.resource.WsrfConstants;
import org.apache.qpid.management.Names;

/**
 * Test case for Web Service Resource Properties interfaces.
 * Those interfaces are defined on http://docs.oasis-open.org/wsrf/wsrf-ws_resource_properties-1.2-spec-os.pdf
 * (Web Services Resource Properties 1.2 - (WS-ResourceProperties).
 * For a better explanation see chapter 5 of the specification above.
 * 
 * @author Andrea Gazzarini
 */
public class GetResourcePropertiesTestCase extends BaseWsDmAdapterTestCase
{
	
	/**
	 * Test the WS-RP GetResourceProperty interface of the WS-DM adapter.
	 * 
	 * <br>precondition : a ws resource exists and is registered. 
	 * <br>postcondition : property values coming from WS-DM resource are the same of the JMX interface.
	 */
	public void testGetResourcePropertiesOK() throws Exception
	{
		MBeanAttributeInfo [] attributesMetadata = _mbeanInfo.getAttributes();
		for (MBeanAttributeInfo attributeMetadata : attributesMetadata)
		{
			String name = attributeMetadata.getName();
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
						_managementServer.getAttribute(_resourceObjectName,name),
						propertyValue);
			} else {
				assertNull(
						String.format(
								"\"%s\" property value shouldn't be null. Its value is %s",
								name,
								_managementServer.getAttribute(_resourceObjectName,name)),
								_managementServer.getAttribute(_resourceObjectName,name));
			}
		}
	}
	
	/**
	 * Tests the GetMultipleResourceProperties interface when the request contains 
	 * an unknwon target resource.
	 * 
	 * <br>precondition : the GetMultipleResourceProperties request contains an unknwon resource.
	 * <br>postcondition : a SoapFault is thrown and the corresponding detail contains an 
	 * 								UnknownResourceFault element.
	 */
	public void testGetResourcePropertiesKO_WithUnknownResourceFault() throws Exception
	{
		try 
		{
			_resourceClient.getEndpointReference().removeParameter(Names.RESOURCE_ID_QNAME);
			_resourceClient.getEndpointReference().addParameter(Names.RESOURCE_ID_QNAME,"lablabalbal");
			
			_resourceClient.getResourceProperty(new QName("a","b","c"));
		} catch(SoapFault expected)
		{
			assertEquals(
					WsrfConstants.RESOURCE_UNKNOWN_QNAME.getLocalPart(),
					expected.getDetail().getLocalName());
		}
	}	
}
