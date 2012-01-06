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

/**
 * An example demonstrating the usage of GetResourcePropertyRequest/Response on 
 * the WS-DM Adapter.
 * 
 * @author Andrea Gazzarini
 */
public class GetQManResourceMembersExample extends AbstractQManExample
{
	/**
	 * Looks for memebers of QMan group requesting ws-rp:Entry property to 
	 * WS-DM Adapter resource service.
	 * 
	 * @param host the host where QMan is running.
	 * @param port the port where QMan is running.
	 * @throws Exception when the example fails (not at application level).
	 */
	void executeExample(String host, int port) throws Exception
	{		
		// 1) Creates an endpoint reference of the adapter service...
		EndpointReference serviceEndpointReference = getAdapterEndpointReference(host, port);
		
		// 2) Creates the Service client...
		ServiceGroupClient adapterClient = new ServiceGroupClient(serviceEndpointReference);
		adapterClient.setTrace(true);
		
		// 3) Invokes the service.
		WsResourceClient [] resources = adapterClient.getMembers();
	
		String result = (resources.length != 0) 
			? ("QMan has at the moment "+resources.length+" registered resources.")
			: "It seems that there are no managed resource on QMan side...";
		
		System.out.println("--------------------------------------------------------------------------");
		System.out.println(result);
		System.out.println("--------------------------------------------------------------------------");
	}
	
	/**
	 * Prints out a description of this example.
	 */
	void printOutExampleDescription()
	{
		System.out.println("      "+getClass().getSimpleName()+" ");
		System.out.println("-------------------------------------------------------------------");
		System.out.println();
		System.out.println("This example shows the usage of WS-DM ");
		System.out.println("GetResourcePropertyRequest / Response on a ");
		System.out.println("Group service.");
		System.out.println("The target resource is the WS-DM Adapter itself ");
		System.out.println("and the requested property is \"wsrf-sg:Entry\".");				
		System.out.println("WS-DM Adapter is a special WS-Resource (is a Group)");
		System.out.println("that  acts as the main entry point for retrieving");
		System.out.println("all other managed resources.");				
		System.out.println("So clients that want to deal with QMan WS-Resources");
		System.out.println("must first get resource identifiers sending");				
		System.out.println("a GetResourcePropertyRequest to WS-DM Adapter ");				
		System.out.println("with \"wsrf-sg:Entry\" as target target property.");		
		System.out.println();
		System.out.println("-------------------------------------------------------------------");
		System.out.println();
	}
	
	public static void main(String[] arguments)
	{
		new GetQManResourceMembersExample().execute(arguments);
	}
}