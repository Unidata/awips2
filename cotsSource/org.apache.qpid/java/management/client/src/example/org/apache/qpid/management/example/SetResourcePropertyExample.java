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

import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.muse.core.proxy.ProxyHandler;
import org.apache.muse.core.proxy.ReflectionProxyHandler;
import org.apache.muse.util.xml.XmlUtils;
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.resource.remote.WsResourceClient;
import org.apache.muse.ws.resource.sg.remote.ServiceGroupClient;
import org.w3c.dom.Element;

/**
 * This example shows how to change the state of a WS-Resource. That means 
 * a SetResourcePropertyRequest is sent to that WS-Resource.
 * First of all a request is send to WS-DM in order to get all registered WS-Resources.
 * If the returned list is not empty then two GetMetadataRequests are sent to the
 * first child (one for WSDL and one for RDM).
 * The result metadata descriptors are the used to determine : 
 * 
 * <br> What are names of WS-Resouce properties
 * <br> Their modifiability (read-only or read-write)
 * <br> Their type
 * 
 * So a SetResourcePropertyRequest can be sent in order to change the WS-Resource state.
 * The example is looking for a property that has one of the following datatype :
 * 
 * <ul>
 * 	<li>String</li>
 * 	<li>Long</li>
 * 	<li>Integer</li>
 * 	<li>Short</li>
 * 	<li>Double</li>
 * 	<li>Float</li>
 * </ul>
 * 
 * After the update / insert request has been sent, a GetResourcePropertiesRequest is made 
 * again in order to see if the state has changed correctly.
 * 
 * @author Andrea Gazzarini
 */
public class SetResourcePropertyExample extends AbstractQManExample
{
	/**
	 * First of all a request is send to WS-DM in order to get all registered WS-Resources.
	 * If the returned list is not empty then two GetMetadataRequests are sent to the
	 * first child (one for WSDL and one for RDM).
	 * The result metadata descriptors are the used to determine : 
	 * 
	 * <br> What are names of WS-Resouce properties
	 * <br> Their modifiability (read-only or read-write)
	 * <br> Their type
	 * 
	 * So a SetResourcePropertyRequest can be sent in order to change the WS-Resource state.
	 * The example is looking for a property that has one of the following datatype :
	 * 
	 * <ul>
	 * 	<li>String</li>
	 * 	<li>Long</li>
	 * 	<li>Integer</li>
	 * 	<li>Short</li>
	 * 	<li>Double</li>
	 * 	<li>Float</li>
	 * </ul>
	 * 
	 * After the update / insert request has been sent, a GetResourcePropertiesRequest is made 
	 * again in order to see if the state has changed correctly.
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
		adapterClient.setTrace(false);
		
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
		
		// Resource Metadata Descriptor
		String dialect = "http://docs.oasis-open.org/wsrf/rmd-1";
		Object [] inputParameters = {dialect};
		
		// RDM is the first element of the returned array. 
		// The first element is a wsx:Metadata containing all resource properties. 
		Element [] metadata = (Element[]) wsResourceClient.invoke(metadataProxyHandler, inputParameters);
		Element resourceMetadataDescriptor	 = metadata[0];
		
		// 6) Now we need WSDL in order to catch datatypes
		dialect = "http://schemas.xmlsoap.org/wsdl/";
		inputParameters = new Object[]{dialect};
		metadata = (Element[]) wsResourceClient.invoke(metadataProxyHandler, inputParameters);
		Element wsdl = metadata[0];
		
		//7) Defines sample values used for update property.
		Map<String, Object> sampleValues = new HashMap<String, Object>();
		sampleValues.put("xsd:string","This is a string.");
		sampleValues.put("xsd:integer",new Integer(12345));
		sampleValues.put("xsd:int",new Integer(54321));
		sampleValues.put("xsd:long",new Integer(12345));
		sampleValues.put("xsd:double",new Double(12345.6d));
		sampleValues.put("xsd:float",new Float(123.4f));
		sampleValues.put("xsd:short",new Short((short)12));
		
		// 8) using XPath navigates xml in order to get the list of all properties.
		Element [] properties = XmlUtils.findInSubTree(
				resourceMetadataDescriptor, 
				new QName("http://docs.oasis-open.org/wsrf/rmd-1","Property","wsrmd"));
		
		Element [] wsdlElements = XmlUtils.findInSubTree(
				wsdl, 
				new QName("http://www.w3.org/2001/XMLSchema","element","xsd"));
		
		// Did we find at least one writable property?
		boolean atLeastOnePropertyHasBeenFound = false;
		
		for (Element property : properties)
		{	
			// Sanity check : if the property is read-only then proceed with next
			// property.
			if (!"read-write".equals(property.getAttribute("modifiability")))
			{
				continue;
			}
			
			String attributeName = property.getAttribute("name"); // = qman:<Attribute Name>

			// For this example we are only interested on qman namespace related properties...
			if (attributeName.startsWith("qman"))
			{
				String attributeNameWithoutPrefix = attributeName.replaceFirst("qman:", ""); //  = <Attribute Name>

				for (Element wsdlElement : wsdlElements)
				{
					String name = wsdlElement.getAttribute("name");
					String type = wsdlElement.getAttribute("type");
					if ((name != null) && (attributeNameWithoutPrefix.equals(name)) && (type != null))
					{
						Object newValue = sampleValues.get(type);
						if (newValue != null)
						{
							atLeastOnePropertyHasBeenFound = true;
							
							inputParameters = new Object[] {newValue};
							
							// 9) Makes a GetResourcePropertiesRequest in order to get the current value.
							QName propertyQName = new QName(
									"http://amqp.apache.org/qpid/management/qman",
									name,
									"qman");
							
							// The returned value is really an array because property shoudl be a multi-value property.
							// So in order to get its value we need to extract the first value.
							Object currentValue = wsResourceClient.getPropertyAsObject(propertyQName,newValue.getClass());
							
							// 10a) If the property is not set (value is null) then an "Insert" request must be sent.
							if (currentValue == null || Array.getLength(currentValue) == 0)
							{
								wsResourceClient.insertResourceProperty(propertyQName,inputParameters);
							} 
							// 10b) If the property is not null then an "Update" request must be sent.
							else 
							{
								wsResourceClient.updateResourceProperty(propertyQName,inputParameters);								
							}
							
							// 11) Let's query again the resource using GetResourceProperties in order to ensure the
							// previous property has been properly updated.
							currentValue = wsResourceClient.getPropertyAsObject(propertyQName,newValue.getClass());
							
							String resultMessage = (newValue.equals(Array.get(currentValue, 0))) 
								? "Resource has been correctly updated."
								: "Something was wrong : resource seems not to be properly updated.";
							
							System.out.println("----------------------------------------------------------------------------------");
							System.out.println(resultMessage);
							System.out.println("----------------------------------------------------------------------------------");

							// Let's stop...one property is enough for this example :)
							break;
						}
					}
				}
				if (!atLeastOnePropertyHasBeenFound)
				{
					System.out.println("----------------------------------------------------------------------------------");
					System.out.println("WARNING : This example wasn't able to run because no writable ");
					System.out.println("property has been found on the target WS-Resource.");
					System.out.println("----------------------------------------------------------------------------------");
				}
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
		System.out.println("This example shows how to change the state of a WS-Resource.");
		System.out.println("That means a SetResourcePropertyRequest is sent to that");
		System.out.println("WS-Resource.");
		System.out.println("First of all a request is send to WS-DM in order to get all");
		System.out.println("registered WS-Resources.");
		System.out.println("If the returned list is not empty then two GetMetadataRequests");
		System.out.println("(one for WSDL and one for RDM) are sent to the first child.");
		System.out.println("The result metadata descriptors are used for determine :");
		System.out.println();
		System.out.println("1) WS-Resource property names;");
		System.out.println("2) Modifiability (read-only, read-write");
		System.out.println("3) Datatype;");
		System.out.println("-------------------------------------------------------------------");
		System.out.println();
		System.out.println("So a SetResourcePropertyRequest can be sent in order");
		System.out.println("to change the WS-Resource state.");
		System.out.println("The example is looking for a property that has one of the");
		System.out.println("following datatype :");
		System.out.println();
		System.out.println("1) String (xsd:string)");
		System.out.println("2) Long (xsd:long)");
		System.out.println("3) Integer (xsd:integer or xsd:int)");
		System.out.println("4) Double (xsd:double)");
		System.out.println("5) Float (xsd:float)");
		System.out.println("6) Short (xsd:short)");
		System.out.println();
		System.out.println("After the update / insert request has been sent, a ");
		System.out.println("GetResourcePropertiesRequest is made again");
		System.out.println("in order to see if the state has changed correctly.");
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
		new SetResourcePropertyExample().execute(arguments);
	}
}