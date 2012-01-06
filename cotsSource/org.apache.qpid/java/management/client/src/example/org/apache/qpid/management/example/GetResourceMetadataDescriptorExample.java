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
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.resource.remote.WsResourceClient;
import org.apache.muse.ws.resource.sg.remote.ServiceGroupClient;
import org.w3c.dom.Element;

/**
 * This example shows how to get metadata from a WS-Resource.
 * The service supports different kinds of metadata. 
 * User who wants to receive metadata of a WS-Resource must 
 * send a GetMetadataRequesta specifying the requested dialect. 
 *  
 * Supported metadata that could be requested are 
 * 
 * <ul>
 * 	<li>
 * 		WSDL : requested using "http://schemas.xmlsoap.org/wsdl/" as dialect..
 * 	<li>
 * 	<li>
 * 		RDM (Resource Metadata Descriptor) : requested using "http://docs.oasis-open.org/wsrf/rmd-1 "as dialect.
 * 	</li>
 * </ul>
 * 
 * Note that this example focuses on RDM Metadata only; another example is dedicated to WSDL.
 * 
 * @author Andrea Gazzarini
 */
public class GetResourceMetadataDescriptorExample extends AbstractQManExample
{

	/**
	 * First, sends a request to WS-DM Adapter in order to get the list of managed resources.
	 * If the list is not empty, then takes the first member and sends it a GetMetadataRequest 
	 * in order to get its RDM.
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
		WsResourceClient firstMember = resources[0];
		firstMember.setTrace(true);
		
		// Dialect is RDM for this example
		String dialect = "http://docs.oasis-open.org/wsrf/rmd-1";
		Object [] inputParameters = {dialect};
		
		// WSDL is the first element of the returned array. We don't need to print out it here
		// because at this point it should have been already printed out (line 96 : firstMember.setTrace(true))
		@SuppressWarnings("unused")
		Element [] metadata = (Element[]) firstMember.invoke(metadataProxyHandler, inputParameters);
	}
	
	/**
	 * Prints out a description of this example.
	 */
	void printOutExampleDescription()
	{
		System.out.println("                 "+getClass().getSimpleName()+" ");
		System.out.println("-------------------------------------------------------------------");
		System.out.println();
		 System.out.println("The example shows how to get metadata from a");
		 System.out.println("WS-Resource.");
		 System.out.println("A QMan WS-Resource has different kinds of metadata.");
		 System.out.println("(see below)");
		 System.out.println("User who wants to receive metadata of a WS-Resource");
		 System.out.println("must send a GetMetadataRequesta specifying the");
		 System.out.println("associated dialect."); 
		 System.out.println("Supported metadata that could be requested are : ");
		 System.out.println();
		 System.out.println("- WSDL : in this case dialect is \"http://schemas.xmlsoap.org/wsdl/\";");
		 System.out.println("- RDM (Resource Metadata Descriptor) : in this case dialect is \"http://docs.oasis-open.org/wsrf/rmd-1 \".");
		System.out.println();
		System.out.println("Note that this examples focuses on RDM Metadata only;");
		System.out.println("another one is dedicated to WSDL.");
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
		new GetResourceMetadataDescriptorExample().execute(arguments);
	}
}