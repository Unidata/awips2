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

import org.apache.muse.core.proxy.ProxyHandler;
import org.apache.muse.core.proxy.ReflectionProxyHandler;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.muse.ws.metadata.WsxConstants;
import org.apache.muse.ws.resource.WsrfConstants;
import org.apache.muse.ws.resource.metadata.WsrmdConstants;
import org.apache.qpid.management.Names;
import org.w3c.dom.Element;

/**
 * Test case for QMan metadata exchange interface.
 * 
 * @author Andrea Gazzarini
 */
public class MetadataExchangeInterfaceTestCase extends BaseWsDmAdapterTestCase
{
	/**
	 * Test the MetadataExchange interface when the corresponding 
	 * request doesn't contain a dialect. According to WS-MetadataExchange specs this should be 
	 * intended as a "give-me-all-metadata" for that resource.
	 * 
	 * <br>precondition : the GetMetadata request doesn't contain a dialect. 
	 * <br>postcondition : the whole metadata document is returned with all metadata . 
	 * It will contain both WSDL and RMD.
	 */
	@SuppressWarnings("unchecked")
	public void testGetMetadataOK_WithoutDialect() throws Exception
	{
		Element[] result = (Element[]) _resourceClient.invoke(
				getProxyHandler(), 
				new Object[]{""});

		assertEquals(2,result.length);
		
		Element  rmdMetadataSection = result[0];
		Element wsdlMetadataSection = result[1];
		
		Element rmd = XmlUtils.getFirstElement(rmdMetadataSection);
		Element wsdl = XmlUtils.getFirstElement(wsdlMetadataSection);

		assertEquals("MetadataDescriptor",rmd.getLocalName());
		assertEquals("definitions",wsdl.getLocalName());
	}

	/**
	 * Test the MetadataExchange interface when the WSDL dialect is specified on the request.
	 * 
	 * <br>precondition : the GetMetadata request contains WSDL dialect. 
	 * <br>postcondition : the resource WSDL metadata document is returned.
	 */
	@SuppressWarnings("unchecked")
	public void testGetMetadataOK_WithWSDLDialect() throws Exception
	{
		Element[] result = (Element[]) _resourceClient.invoke(
				getProxyHandler(), 
				new Object[]{WsxConstants.WSDL_DIALECT});

		assertEquals(1,result.length);
		
		Element wsdlMetadataSection = result[0];
		
		Element wsdl = XmlUtils.getFirstElement(wsdlMetadataSection);

		assertEquals("definitions",wsdl.getLocalName());
	}

	/**
	 * Test the MetadataExchange interface when the RMD dialect is specified on the request.
	 * 
	 * <br>precondition : the GetMetadata request contains RMD dialect. 
	 * <br>postcondition : the RMD metadata document is returned.
	 */
	@SuppressWarnings("unchecked")
	public void testGetMetadataOK_WithRMDDialect() throws Exception
	{
		Element[] result = (Element[]) _resourceClient.invoke(
				getProxyHandler(), 
				new Object[]{WsrmdConstants.NAMESPACE_URI});

		assertEquals(1,result.length);
		
		Element rmdMetadataSection = result[0];
		
		Element wsdl = XmlUtils.getFirstElement(rmdMetadataSection);

		assertEquals("MetadataDescriptor",wsdl.getLocalName());
	}
	
	/**
	 * Test the MetadataExchange interface with an unknown metadata dialect.
	 * 
	 * <br>precondition : the GetMetadata request contains an unknown dialect. 
	 * <br>postcondition : the returned metadata section is empty.
	 */
	@SuppressWarnings("unchecked")
	public void testGetMetadataKO_WithoutUnknownDialect() throws Exception
	{
		Element [] metadata = (Element[]) _resourceClient.invoke(
				getProxyHandler(), 
				new Object[]{"HopeThisIsAnUnknownDialect"});
		
		assertEquals(0,metadata.length);
	}	

	/**
	 * Test the MetadataExchange interface with an unknown metadata dialect.
	 * 
	 * <br>precondition : the GetMetadata request contains an unknown dialect. 
	 * <br>postcondition : the returned metadata section is empty.
	 */
	@SuppressWarnings("unchecked")
	public void testGetMetadataKO_WithoutUnknownResourceFault() throws Exception
	{
		try 
		{
			_resourceClient.getEndpointReference().removeParameter(Names.RESOURCE_ID_QNAME);
			_resourceClient.getEndpointReference().addParameter(Names.RESOURCE_ID_QNAME,"lablabalbal");
	
			_resourceClient.invoke(getProxyHandler(), new Object[]{""});
		} catch(SoapFault expected)
		{
			assertEquals(
					WsrfConstants.RESOURCE_UNKNOWN_QNAME.getLocalPart(),
					expected.getDetail().getLocalName());
		}
	}		
	
	/**
	 * Returns a proxy handler used for working with metadata exchange
	 * interface.
	 * 
	 * @return a metadata proxy handler. 
	 */
	private ProxyHandler getProxyHandler()
	{
        ProxyHandler getMetadataHandler = new ReflectionProxyHandler();
        getMetadataHandler.setAction("http://schemas.xmlsoap.org/ws/2004/09/mex/GetMetadata");
        getMetadataHandler.setRequestName(new QName("http://schemas.xmlsoap.org/ws/2004/09/mex", "GetMetadata", "wsx"));
        getMetadataHandler.setRequestParameterNames(new QName[]{new QName("http://schemas.xmlsoap.org/ws/2004/09/mex", "Dialect", "wsx")});
        getMetadataHandler.setResponseName(new QName("http://schemas.xmlsoap.org/ws/2004/09/mex", "Metadata", "wsx"));
        getMetadataHandler.setReturnType(Element[].class);
        return getMetadataHandler;
	}
}