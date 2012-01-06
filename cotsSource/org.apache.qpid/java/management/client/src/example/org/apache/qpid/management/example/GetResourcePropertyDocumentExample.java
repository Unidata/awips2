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

import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.resource.remote.WsResourceClient;
import org.apache.muse.ws.resource.sg.remote.ServiceGroupClient;
import org.w3c.dom.Element;

/**
 * This example shows how to get the whole property document from a WS-Resource.
 * Resource property document represents a particular composed structural view of 
 * the resource properties of the WS-Resource.
 * Let's say that It is a way to get all-in-once the state of the WS-Resource.
 * 
 * First of all a request is send to WS-DM in order to get all registered WS-Resources.
 * If the returned list is not empty then a GetResourcePropertyDocumentRequest is 
 * sent to the first child.
 * 
 * @author Andrea Gazzarini
 */
public class GetResourcePropertyDocumentExample extends AbstractQManExample
{

	/**
	 * First of all a request is send to WS-DM in order to get all registered WS-Resources.
	 * If the returned list is not empty then a GetResourcePropertyDocumentRequest is 
	 * sent to the first child.
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
		
		// 4) ..and invokes the GetMetadata on the first member.
		WsResourceClient wsResourceClient = resources[0];
		wsResourceClient.setTrace(true);
		
		@SuppressWarnings("unused")
		Element resourcePropertyDocument = wsResourceClient.getResourcePropertyDocument();
	}
	/**
	 * Prints out a description of this example.
	 */
	void printOutExampleDescription()
	{			
		System.out.println("                 "+getClass().getSimpleName()+" ");
		System.out.println("-------------------------------------------------------------------");
		System.out.println();
		System.out.println("This example shows how to get the whole property");
		System.out.println("document from a WS-Resource.");
		System.out.println("Resource property document represents a particular ");
		System.out.println("composed structural view of the resource properties");
		System.out.println("of the WS-Resource.");
		System.out.println("First of all a request is send to WS-DM in order to get");
		System.out.println("all registered WS-Resources."); 
		System.out.println("the target WS-Resource.");
		System.out.println("If the returned list is not empty then a");
		System.out.println("GetResourcePropertyDocumentRequest is sent to the first child.");
		System.out.println();
		System.out.println("-------------------------------------------------------------------");
		System.out.println();
	}
	
	public static void main(String[] arguments)
	{
		new GetResourcePropertyDocumentExample().execute(arguments);
	}
}