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
package org.apache.qpid.management.wsdm.capabilities;

import java.lang.management.ManagementFactory;
import java.net.URI;

import javax.management.ObjectName;

import junit.framework.TestCase;

import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.resource.WsResource;
import org.apache.muse.ws.resource.impl.SimpleWsResource;
import org.apache.qpid.management.domain.handler.impl.QpidDomainObject;
import org.apache.qpid.management.wsdm.common.EntityInstanceNotFoundFault;
import org.apache.qpid.management.wsdm.common.NoSuchAttributeFault;
import org.apache.qpid.management.wsdm.common.QManFault;

/**
 * Test case for MBeanCapability supertype layer..
 * 
 * @author Andrea Gazzarini
 */
public class MBeanCapabilityTest extends TestCase
{	
	private final String _typeAttributeName = "Type";
	private final String _newTypeValue = "DomainObject";
	
	private ObjectName _objectName;
	private ObjectName _unknownObjectName;

	private MBeanCapability _capability;

	@Override
	protected void setUp() throws Exception
	{
		_objectName = new ObjectName("Test:Name=aName");
		_unknownObjectName = new ObjectName("Test:Type=unknown");
		
		_capability = new MBeanCapability(){
			@Override
			public WsResource getWsResource()
			{
				return new SimpleWsResource(){
					@Override
					public EndpointReference getEndpointReference()
					{
						return new EndpointReference(URI.create("http://qpid.apache.org/qman"));
					}
				};
			}
		};
		_capability.setResourceObjectName(_objectName);
		ManagementFactory.getPlatformMBeanServer().registerMBean(new QpidDomainObject(), _objectName);
	}
	
	/**
	 * Tests the execution of the getAttribute() and setAttribute() method.
	 * 
	 * <br>precondition : the mbean is registered and a _capability is associated with it.
	 * <br>postcondition : the set value of the requested attribute is correctly returned.
	 */
	public void testGetAndSetAttributeOK() throws Exception 
	{
		Object name = _capability.getAttribute(_typeAttributeName);
		assertNull("Name has an initial value of null so how is possibile that is not null?",name);
		
		_capability.setAttribute(_typeAttributeName,_newTypeValue);
		
		name = _capability.getAttribute(_typeAttributeName);
		assertEquals("Now the name attribute must be set to \""+_newTypeValue+"\"",_newTypeValue,name);
	}
	
	/**
	 * Tests the execution of the getAttribute() and setAttribte() methods when an unknown attribute is given..
	 * 
	 * <br>precondition : the mbean is registered, a _capability is associated with it and the requested attribute doesn't exist.
	 * <br>postcondition : an exception is thrown indicating the failure.
	 */
	public void testNoSuchAttributeFault() throws Exception
	{
		// I suppose that we shouldn't have an attribute with this name...
		String unknownAttribute = String.valueOf(System.currentTimeMillis());
		
		try 
		{
			_capability.getAttribute(unknownAttribute);
			fail("An exception must be thrown here in order to indicate that the attribute is unknown.");
		} catch(NoSuchAttributeFault expected)
		{
		}
		
		try 
		{
			_capability.setAttribute(unknownAttribute,null);
			fail("An exception must be thrown here in order to indicate that the attribute is unknown.");
		} catch(NoSuchAttributeFault expected)
		{
		}
	}
	
	/**
	 * Tests the execution of the setAttribute,getAttribute and invoke methods when the target mbean
	 * doesn't exists.
	 * 
	 * <br>precondition : the object name associated with the capability is not pointing to an existent MBean.
	 * <br>postcondition : an exception is thrown indicating the failure.
	 */
	public void testEntityInstanceNotFoundFault() throws Exception
	{
		_capability.setResourceObjectName(_unknownObjectName);
		
		try 
		{
			_capability.getAttribute(_typeAttributeName);
			fail("An exception must be thrown here in order to indicate that the attribute is unknown.");
		} catch(EntityInstanceNotFoundFault expected)
		{
		}
		
		try 
		{
			_capability.setAttribute(_typeAttributeName,_newTypeValue);
			fail("An exception must be thrown here in order to indicate that the attribute is unknown.");
		} catch(EntityInstanceNotFoundFault  expected)
		{
		}
		
		try 
		{
			_capability.invoke("operationName", null,null);
			fail("An exception must be thrown here in order to indicate that the attribute is unknown.");
		} catch(EntityInstanceNotFoundFault expected)
		{
		}
	}

	/**
	 * Tests the execution of the setAttribute,getAttribute and invoke methods when an unknown / unexpected 
	 * exception is thrown.
	 * 
	 * <br>precondition : the mbean is registered and a capability is associated with it. Something 
	 * 								unexpected happens during method invocation.
	 * <br>postcondition : an exception is thrown indicating the failure.
	 */
	public void testQManFault() throws Exception
	{
		// Emulate a RuntimeException (which is the best example of uncaught exception... :) )
		_capability.setResourceObjectName(null);
		
		try 
		{
			_capability.getAttribute(_typeAttributeName);
			fail("An exception must be thrown here in order to indicate that the attribute is unknown.");
		} catch(QManFault expected)
		{
		}
		
		try 
		{
			_capability.setAttribute(_typeAttributeName,_newTypeValue);
			fail("An exception must be thrown here in order to indicate that the attribute is unknown.");
		} catch(QManFault  expected)
		{
		}
		
		try 
		{
			_capability.invoke("operationName", null,null);
			fail("An exception must be thrown here in order to indicate that the attribute is unknown.");
		} catch(QManFault expected)
		{
		}
	}
	
	
	/**
	 * Shutdown procedure for this test case.
	 */
	@Override
	protected void tearDown() throws Exception
	{
		ManagementFactory.getPlatformMBeanServer().unregisterMBean(_objectName);
	}
}
