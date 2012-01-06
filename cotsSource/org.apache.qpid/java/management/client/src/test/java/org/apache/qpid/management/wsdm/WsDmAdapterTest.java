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

import java.io.IOException;
import java.net.ServerSocket;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.muse.core.serializer.SerializerRegistry;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.Protocol;
import org.apache.qpid.management.wsdm.capabilities.Result;
import org.apache.qpid.management.wsdm.muse.serializer.DateSerializer;
import org.apache.qpid.management.wsdm.muse.serializer.InvocationResultSerializer;
import org.apache.qpid.management.wsdm.muse.serializer.MapSerializer;
import org.apache.qpid.management.wsdm.muse.serializer.ObjectSerializer;
import org.apache.qpid.management.wsdm.muse.serializer.UUIDSerializer;
import org.mortbay.component.LifeCycle;
import org.mortbay.component.LifeCycle.Listener;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

public class WsDmAdapterTest
{

	/**
	 * Test case wide set up.
	 * Provides Server startup & shutdown global procedure.
	 * 
	 * @author Andrea Gazzarini
	 */
	private static class WsDmAdapterTestSetup extends TestSetup
	{
		private Object _serverMonitor = new Object();
		
		Listener listener = new WebApplicationLifeCycleListener() 
		{
			public void lifeCycleStarted(LifeCycle event)
			{
				synchronized (_serverMonitor) 
				{
					_serverMonitor.notify();
				}
			}
		};
		
		private ServerThread _server;
		
		/**
		 * Builds a new test setup with for the given test.
		 * 
		 * @param test the decorated test.
		 */
		public WsDmAdapterTestSetup(Test test)
		{
			super(test);
		}
		
		/**
		 * Starts up Web server.
		 * 
		 * @throws Exception when the server startup fails.
		 */
		@Override
		protected void setUp() throws Exception
		{	
			SerializerRegistry.getInstance().registerSerializer(Object.class, new ObjectSerializer());
			SerializerRegistry.getInstance().registerSerializer(Date.class, new DateSerializer());
			SerializerRegistry.getInstance().registerSerializer(Map.class, new MapSerializer());
			SerializerRegistry.getInstance().registerSerializer(HashMap.class, new MapSerializer());
			SerializerRegistry.getInstance().registerSerializer(UUID.class, new UUIDSerializer());
			SerializerRegistry.getInstance().registerSerializer(Result.class, new InvocationResultSerializer());
			
			System.setProperty(
					Names.ADAPTER_HOST_PROPERTY_NAME, 
					Protocol.DEFAULT_QMAN_HOSTNAME);

			System.setProperty(
					Names.ADAPTER_PORT_PROPERTY_NAME, 
					String.valueOf(getFreePort()));
			
			_server = new ServerThread(listener);
			_server.start();
			
			synchronized(_serverMonitor) {
				_serverMonitor.wait();
				Thread.sleep(2000);
			}
		}
		
		@Override
		protected void tearDown() throws Exception
		{
			_server.shutdown();
		}
	};
	
	/**
	 * Gets the test suite composition.
	 * 
	 * @return the test suite composition.
	 */
	public static Test suite()
	{
		TestSuite suite = new TestSuite("Test suite for QMan WS-DM.");
		suite.addTestSuite(MetadataExchangeInterfaceTestCase.class);
		suite.addTestSuite(OperationInvocationInterfaceTestCase.class);
		suite.addTestSuite(GetResourcePropertyDocumentTestCase.class);
		suite.addTestSuite(SetResourcePropertiesTestCase.class);		
		suite.addTestSuite(GetMultipleResourcePropertiesTestCase.class);	
		suite.addTestSuite(GetResourcePropertiesTestCase.class);			
		return new WsDmAdapterTestSetup(suite);
	}
	
	/**
	 * Finds a free port that will be used to run the embedded 
	 * web server.
	 * 
	 * @return a 	free port that will be used to run the 
	 * 					embedded web server.
	 */
	private static int getFreePort() throws IOException {
		ServerSocket server = null;
		try 
		{
			server = new ServerSocket(0);
			return server.getLocalPort();
		} finally 
		{
			server.close();			
		}
	}
}