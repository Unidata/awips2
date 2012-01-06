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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import org.apache.muse.ws.notification.NotificationProducer;
import org.apache.qpid.management.Names;

import junit.framework.TestCase;

/**
 * Test case for QMan adapter capability.
 * 
 * @author Andrea Gazzarini
 */
public class QManAdapterCapabilityTest extends TestCase
{	
	/**
	 * Tests the execution of the getTopicName() method.
	 * 
	 * <br>precondition : an object type is given to the method (null is allowed).
	 * <br>postcondition : according to getTopicName() specs, the name of the 
	 * 		topic associated with the given object type must be returned.
	 */
	public void testGetTopicName() 
	{
		final InvocationHandler invocationHandler = new InvocationHandler(){

			public Object invoke(Object proxy, Method method, Object[] args) 
			{
				return null;
			}
		};
		
		QManAdapterCapability capability = new QManAdapterCapability(){
			@Override
			NotificationProducer getPublisherCapability()
			{
				return (NotificationProducer) Proxy.newProxyInstance(
						getClass().getClassLoader(), 
						new Class[]{NotificationProducer.class},
						invocationHandler);
			}
		};
		
		capability.createLifeCycleTopics();
		
		assertEquals(
				Names.EVENTS_LIFECYLE_TOPIC_NAME,
				capability.getTopicName(Names.EVENT));
		
		assertEquals(
				Names.OBJECTS_LIFECYLE_TOPIC_NAME,
				capability.getTopicName(Names.CLASS));		
		
		assertEquals(
				Names.UNKNOWN_OBJECT_TYPE_LIFECYLE_TOPIC_NAME,
				capability.getTopicName("This is an unknown object Type @#!--!!@#"));				
	}
}