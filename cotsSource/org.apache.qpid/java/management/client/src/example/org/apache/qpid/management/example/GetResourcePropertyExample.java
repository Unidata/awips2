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
package org.apache.qpid.management.example;

import javax.xml.namespace.QName;

import org.apache.muse.core.proxy.ProxyHandler;
import org.apache.muse.core.proxy.ReflectionProxyHandler;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.resource.remote.WsResourceClient;
import org.apache.muse.ws.resource.sg.remote.ServiceGroupClient;
import org.w3c.dom.Element;

/**
 * This example shows how to get the  property value from a WS-Resource.
 * First of all a request is send to WS-DM in order to get all registered WS-Resources.
 * If the returned list is not empty then a GetMetadataRequest is sent to the
 * first child.
 * The result metadata descriptor contains all properties of the target WS-Resource.
 * For each of them a GetResourcePropertyRequest is sent in order to get its value.
 * 
 * @author Andrea Gazzarini
 */
public class GetResourcePropertyExample extends AbstractQManExample
{

	/**
	 * First, sends a request to WS-DM Adapter in order to get the list of managed resources.
	 * If the list is not empty, then takes the first member and sends it a GetMetadataRequest 
	 * in order to get its WSDL.
	 * After that, for each property contained in ResourceMetadataDescriptorm (RDM) a 
	 * GetResourcePropertyRequest is sent in order to get its value.
	 * 
	 * @param host the host where QMan is running.
	 * @param port the port where QMan is running.
	 * @throws Exception when the example fails (not at application level).
	 */
	void executeExample(String host, int port) throws Exception
	{		
	
		// 1) Creates an endpoint reference of the adapter service...
		EndpointReference adapterEndpointReference = getAdapterEndpointReference(host, port);
		
		// 2) Creates the Adapter service client...
		ServiceGroupClient adapterClient = new ServiceGroupClient(adapterEndpointReference);
		adapterClient.setTrace(true);
		
		// 3) Retrieves the all registered members (QMan WS-Resources)
		WsResourceClient [] resources = adapterClient.getMembers();

		// Sanity check : we cannot proceed if there are no WS-Resources.
		if (resources.length == 0)
		{
			System.out.println("----------------------------WARNING---------------------------");
			System.out.println("Cannot proceed with the example... it seems");
			System.out.println("that there are no managed WS-Resources on QMan.");
			System.out.println("Please check QMan in order to see that it is really");
			System.out.println("connected with a broker.");
			System.out.println("-------------------------------------------------------------------");
			System.exit(0);
		} 
		
		// 4) Creates a proxy handler for service invocation.
		ProxyHandler metadataProxyHandler = createProxyHandler();
		
		// 5) ..and invokes the GetMetadata on the first member.
		WsResourceClient wsResourceClient = resources[0];
		wsResourceClient.setTrace(true);
		
		// Dialect is RDM for this example
		String dialect = "http://docs.oasis-open.org/wsrf/rmd-1";
		Object [] inputParameters = {dialect};
		
		// RDM is the first element of the returned array. 
		// The first element is a wsx:Metadata containing all resource properties. 
		Element [] metadata = (Element[]) wsResourceClient.invoke(metadataProxyHandler, inputParameters);
		Element resourceMetadataDescriptor	 = metadata[0];
		
		// 6) using XPath navigates xml in order to get the list of all properties.
		Element [] properties = XmlUtils.findInSubTree(
				resourceMetadataDescriptor, 
				new QName("http://docs.oasis-open.org/wsrf/rmd-1","Property","wsrmd"));

		for (Element property : properties)
		{
			
			String attributeName = property.getAttribute("name"); // = qman:<Attribute Name>
			
			// For this example we are only interested on qman namespace related properties...
			if (attributeName.startsWith("qman"))
			{
				String attributeNameWithoutPrefix = attributeName.replaceFirst("qman:", ""); //  = <Attribute Name>
				
				// 7) Send a GetResourcePropertyRequest for the given attribute.
				// We do nothing with the returned value(s) because it / they 
				// has / have already printed out (wsResourceClient.setTrace(true))
				@SuppressWarnings("unused")
				Element [] values = wsResourceClient.getResourceProperty(
						new QName(
								"http://amqp.apache.org/qpid/management/qman",
								attributeNameWithoutPrefix,
								"qman"));
			}
		}
	}
	
	/**
	 * Prints out a description of this example.
	 */
	void printOutExampleDescription()
	{
		System.out.println("                 "+getClass().getSimpleName()+" ");
		System.out.println("-------------------------------------------------------------------");
		System.out.println();
		System.out.println("This example shows how to get the  property value");
		System.out.println("from a WS-Resource.");
		System.out.println("First of all a request is send to WS-DM in order to get");
		System.out.println("all registered WS-Resources.");
		System.out.println("If the returned list is not empty then a GetMetadataRequest");
		System.out.println("to the first child.");
		System.out.println("The result metadata descriptor contains all properties of"); 
		System.out.println("the target WS-Resource.");
		System.out.println("For each of them a GetResourcePropertyRequest is sent");
		System.out.println(" in order to get its value.");
		System.out.println();
		System.out.println("-------------------------------------------------------------------");
		System.out.println();
	}

	/**
	 * A proxy handler is a module needed in order to make a capability 
	 * service invocation.
	 * It contains logic to serialize and deserialize request, response, input and 
	 * output parameters during a web service invocation.
	 * 
	 * @return a proxy handler.
	 */
	private ProxyHandler createProxyHandler()
	{
        ProxyHandler handler = new ReflectionProxyHandler();
        handler.setAction("http://schemas.xmlsoap.org/ws/2004/09/mex/GetMetadata");
        handler.setRequestName(new QName("http://schemas.xmlsoap.org/ws/2004/09/mex", "GetMetadata", PREFIX));
        handler.setRequestParameterNames(new QName[]{new QName("http://schemas.xmlsoap.org/ws/2004/09/mex", "Dialect", PREFIX)});
        handler.setResponseName(new QName("http://schemas.xmlsoap.org/ws/2004/09/mex", "Metadata", PREFIX));
        handler.setReturnType(Element[].class);
        return handler;
	}
	
	public static void main(String[] arguments)
	{
		new GetResourcePropertyExample().execute(arguments);
	}
}