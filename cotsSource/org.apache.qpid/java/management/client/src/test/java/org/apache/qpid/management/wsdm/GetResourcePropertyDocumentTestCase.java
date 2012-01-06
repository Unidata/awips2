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

import javax.xml.namespace.QName;

import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.muse.ws.resource.WsrfConstants;
import org.apache.qpid.management.Names;
import org.w3c.dom.Element;

/**
 * Test case for Web Service Resource Properties interfaces.
 * Those interfaces are defined on http://docs.oasis-open.org/wsrf/wsrf-ws_resource_properties-1.2-spec-os.pdf
 * (Web Services Resource Properties 1.2 - (WS-ResourceProperties).
 * For a better explanation see chapter 5 of the specification above.
 * 
 * @author Andrea Gazzarini
 */
public class GetResourcePropertyDocumentTestCase extends BaseWsDmAdapterTestCase
{
	/**
	 * Tests the GetResourcePropertyDocument interface when the request contains 
	 * an unknwon target resource.
	 * 
	 * <br>precondition : the GetResourcePropertyDocument contains an unknwon resource.
	 * <br>postcondition : a SoapFault is thrown and the corresponding detail contains an 
	 * 								UnknownResourceFault element.
	 */
	public void testGetResourcePropertyDocumentKO_WithUnknownResourceFault() throws Exception
	{
		try 
		{
			_resourceClient.getEndpointReference().removeParameter(Names.RESOURCE_ID_QNAME);
			_resourceClient.getEndpointReference().addParameter(Names.RESOURCE_ID_QNAME,"lablabalbal");
			_resourceClient.setTrace(true);
			
			_resourceClient.getResourcePropertyDocument();
		} catch(SoapFault expected)
		{
			assertEquals(
					WsrfConstants.RESOURCE_UNKNOWN_QNAME.getLocalPart(),
					expected.getDetail().getLocalName());
		}
	}
	
	/**
	 * Tests the WS-RP PutResourcePropertyDocument interface of the WS-DM adapter.
	 * 
	 * <br>precondition : a ws resource exists and is registered. 
	 * <br>postcondition : A read / write property is correctly set according to WSRP interface.
	 */
	public void testGetAndPutResourcePropertyDocumentOK() throws Exception
	{	
		String expectedMgmtPubIntervalValue = "4321";
		String propertyName = "MgmtPubInterval";
		
		Element propertiesDocument = _resourceClient.getResourcePropertyDocument();
		Element [] properties = XmlUtils.getAllElements(propertiesDocument);
		
		for (Element element : properties)
		{
			if (propertyName.equals(element.getLocalName())) {
				element.setTextContent(expectedMgmtPubIntervalValue);
			} else {
				propertiesDocument.removeChild(element);
			}
		}
		
		_resourceClient.putResourcePropertyDocument(propertiesDocument);
		
		Element newProperties = _resourceClient.getResourcePropertyDocument();
		
		Element mgmtPubInterval = XmlUtils.getElement(
				newProperties, new QName(
						Names.NAMESPACE_URI,
						propertyName,
						Names.PREFIX));

		assertEquals(expectedMgmtPubIntervalValue,mgmtPubInterval.getTextContent());		
	}
	
	/**
	 * Tests the WS-RP PutResourcePropertyDocument interface of the WS-DM adapter.
	 * Specifically it tries to update the value of a read-only property.
	 * 
	 * <br>precondition : a ws resource exists, it is registered and has at least one read-only property. 
	 * <br>postcondition : An exception is thrown indicating the failure.
	 */
	public void testGetAndPutResourcePropertyDocumentKO_WithReadOnlyProperty() throws Exception
	{	
		String propertyName = "Name";
		
		Element propertiesDocument = _resourceClient.getResourcePropertyDocument();
		Element [] properties = XmlUtils.getAllElements(propertiesDocument);
		
		for (Element element : properties)
		{
			if (propertyName.equals(element.getLocalName())) {
				element.setTextContent("ThisIsTheNewValueOfNameProperty");
			} else {
				propertiesDocument.removeChild(element);
			}
		}

		try 
		{
			_resourceClient.putResourcePropertyDocument(propertiesDocument);
			fail("It's not possible to update the value of a read-only property.");
		} catch (SoapFault expected) 
		{
			
		}
	}
}
