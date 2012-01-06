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

import java.lang.management.ManagementFactory;
import java.net.URI;
import java.util.UUID;

import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import junit.framework.TestCase;

import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.resource.remote.WsResourceClient;
import org.apache.muse.ws.resource.sg.remote.ServiceGroupClient;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.Protocol;
import org.apache.qpid.management.TestConstants;

/**
 * Test case for WS-Resource lifecycle management.
 * 
 * @author Andrea Gazzarini
 */
public abstract class BaseWsDmAdapterTestCase extends TestCase implements TestConstants{
	
	protected MBeanServer _managementServer;
	protected ObjectName _resourceObjectName;
		
	protected WsResourceClient _resourceClient;
	protected MBeanInfo _mbeanInfo;
	
	/**
	 * Set up fixture for this test case.
	 * 
	 * @throws Exception when the test case intialization fails.
	 */
	protected void setUp() throws Exception 
	{		
		_managementServer = ManagementFactory.getPlatformMBeanServer();
		
        ServiceGroupClient serviceGroup = getServiceGroupClient();
        WsResourceClient [] members = serviceGroup.getMembers();
        
        assertEquals(
        		"No resource has been yet created so how is " +
        			"it possible that service group children list is not empty?",
        		0,
        		members.length);

        _managementServer.invoke(
        		Names.QPID_EMULATOR_OBJECT_NAME, 
        		"createQueue", 
        		new Object[]{_resourceObjectName = createResourceName()}, 
        		new String[]{ObjectName.class.getName()});
                
        members = serviceGroup.getMembers();
        assertEquals(
        		"One resource has just been created so " +
        			"I expect to find it on service group children list...",
        		1,
        		members.length);
        
        _resourceClient = members[0];
        _mbeanInfo = _managementServer.getMBeanInfo(_resourceObjectName);
	}

	/**
	 * Shutdown procedure for this test case.
	 * 
	 * @throws Exception when either the server or some resource fails to shutdown.
	 */
	@Override
	protected void tearDown() throws Exception
	{
        ServiceGroupClient serviceGroup = getServiceGroupClient();
        WsResourceClient [] members = serviceGroup.getMembers();

		_managementServer.invoke(
				Names.QPID_EMULATOR_OBJECT_NAME,
				"unregister",
				new Object[]{_resourceObjectName},
				new String[]{ObjectName.class.getName()});

      	members = serviceGroup.getMembers();

      	assertEquals(
      			"No resource has been yet created so how is it possible that service group children list is not empty?",
      			0,
      			members.length);
	}
				
	/**
	 * Creates a service group client reference.
	 * 
	 * @return a service group client reference.
	 */
	private ServiceGroupClient getServiceGroupClient()
	{
		URI address = URI.create(
				Protocol.DEFAULT_ENDPOINT_URI.replaceFirst("8080",System.getProperty(Names.ADAPTER_PORT_PROPERTY_NAME)));
		return new ServiceGroupClient(new EndpointReference(address));
	}
	
	/**
	 * In order to test the behaviour of the WS-DM adapter, at 
	 * least one resource must be created. This is the method that 
	 * returns the name (ObjectName on JMX side, Resource-ID on WSDM side)
	 * of that resource
	 * 
	 * @return the name of the MBean instance that will be created.
	 * @throws Exception when the name if malformed. Practically never.
	 */
	private ObjectName createResourceName() throws Exception
	{
		return new ObjectName(
				"Q-MAN:objectId="+UUID.randomUUID()+
				", brokerID="+UUID.randomUUID()+
				",class=queue"+
				",package=org.apache.qpid"+
				",name="+System.currentTimeMillis());
	}
}