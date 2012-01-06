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
package org.apache.qpid.management.configuration;

import java.util.Map;
import java.util.UUID;

import junit.framework.TestCase;

import org.apache.qpid.management.Protocol;
import org.apache.qpid.management.domain.handler.base.IMessageHandler;
import org.apache.qpid.management.domain.handler.impl.ConfigurationMessageHandler;
import org.apache.qpid.management.domain.handler.impl.EventContentMessageHandler;
import org.apache.qpid.management.domain.handler.impl.HeartBeatIndicationMessageHandler;
import org.apache.qpid.management.domain.handler.impl.InstrumentationMessageHandler;
import org.apache.qpid.management.domain.handler.impl.MethodResponseMessageHandler;
import org.apache.qpid.management.domain.handler.impl.SchemaResponseMessageHandler;
import org.apache.qpid.management.domain.model.AccessMode;
import org.apache.qpid.management.domain.model.type.AbsTime;
import org.apache.qpid.management.domain.model.type.DeltaTime;
import org.apache.qpid.management.domain.model.type.ObjectReference;
import org.apache.qpid.management.domain.model.type.Str16;
import org.apache.qpid.management.domain.model.type.Str8;
import org.apache.qpid.management.domain.model.type.Uint16;
import org.apache.qpid.management.domain.model.type.Uint32;
import org.apache.qpid.management.domain.model.type.Uint64;
import org.apache.qpid.management.domain.model.type.Uint8;
import org.xml.sax.SAXException;

/**
 * Test case for configurator.
 * 
 * @author Andrea Gazzarini
 *
 */
public class ConfiguratorTest extends TestCase
{
    /**
     * Tests the execution of the configure() method when no configuration file is given.
     * 
     * <br>precondition : configuration file option is not set
     * <br>postcondition : no exception is thrown, the configuration is holding no broker data and the predefined mappings are 
     * stored in configuration.	
     */
	public void testConfigureOK_WithNoConfigurationFile() throws Exception
	{
		Configurator configurator = new Configurator();
		configurator.configure();
		Configuration configuration = Configuration.getInstance();
		
		assertEquals(new Uint8(), configuration.getType(1));
		assertEquals(new Uint16(), configuration.getType(2));
		assertEquals(new Uint32(), configuration.getType(3));
		assertEquals(new Uint64(), configuration.getType(4));
		assertEquals(new Str8(), configuration.getType(6));
		assertEquals(new Str16(), configuration.getType(7));
		assertEquals(new AbsTime(), configuration.getType(8));
		assertEquals(new DeltaTime(), configuration.getType(9));
		assertEquals(new ObjectReference(), configuration.getType(10));
		assertEquals(new org.apache.qpid.management.domain.model.type.Boolean(), configuration.getType(11));
		assertEquals(new org.apache.qpid.management.domain.model.type.Uuid(), configuration.getType(14));
		assertEquals(new org.apache.qpid.management.domain.model.type.Map(), configuration.getType(15));	
		
		assertEquals(AccessMode.RC,configuration.getAccessMode(1));
		assertEquals(AccessMode.RW,configuration.getAccessMode(2));
		assertEquals(AccessMode.RO,configuration.getAccessMode(3));
		
		Map<Character, IMessageHandler> managementHandlers = configuration.getManagementQueueHandlers();
		assertEquals(4,managementHandlers.size());
		assertEquals(
				InstrumentationMessageHandler.class,
				managementHandlers.get(Protocol.INSTRUMENTATION_CONTENT_RESPONSE_OPCODE).getClass());
		
		assertEquals(
				ConfigurationMessageHandler.class,
				managementHandlers.get(Protocol.CONFIGURATION_CONTENT_RESPONSE_OPCDE).getClass());

		assertEquals(
				EventContentMessageHandler.class,
				managementHandlers.get(Protocol.EVENT_CONTENT_RESPONSE_OPCDE).getClass());

		assertEquals(
				HeartBeatIndicationMessageHandler.class,
				managementHandlers.get(Protocol.HEARTBEAT_INDICATION_RESPONSE_OPCODE).getClass());   
		
		Map<Character, IMessageHandler> methodReplyHandlers = configuration.getMethodReplyQueueHandlers();
		assertEquals(2, methodReplyHandlers.size());
		
		assertEquals(
				MethodResponseMessageHandler.class,
				methodReplyHandlers.get(Protocol.OPERATION_INVOCATION_RESPONSE_OPCODE).getClass());

		assertEquals(
				SchemaResponseMessageHandler.class,
				methodReplyHandlers.get(Protocol.SCHEMA_RESPONSE_OPCODE).getClass());   
	}
    
    /**
     * Tests the changes of the configurator internal state while configuration file is parsed.
     * 
     * <br>precondition: N.A.
     * <br>postcondition: N.A.
     */
    public void testDirectorParsing() throws SAXException{
        Configurator configurator = new Configurator();
        
        assertSame(Configurator.DEFAULT_PARSER,configurator._currentParser);

        configurator.startElement(null, null, Tag.BROKERS.toString(), null);
        assertSame(configurator._brokerConfigurationParser,configurator._currentParser);
    }    
    
    /**
     * It's not possibile to add twice the same broker connection data.
     * Is so an exception must be thrown indicating that the given broker is already connected.
     * 
     * <br>precondition : the given data identifies an already connected broker.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testAddTwoIdenticalBrokers() throws ConfigurationException, BrokerConnectionException 
    {
    	Configurator configurator = new Configurator();
    	configurator.configure();
    	
    	BrokerConnectionData data = new BrokerConnectionData("sofia.gazzax.com",5672,"virtualHost","user","pwd",1,4,-1);
    	
    	Configuration.getInstance()._brokerConnectionInfos.put(UUID.randomUUID(),data);
    	
    	try {
			configurator.createAndReturnBrokerConnectionData(
					UUID.randomUUID(), 
					data.getHost(), 
					data.getPort(),
					"anotherUser",
					"anotherPassword", 
					data.getVirtualHost(), 
					33,
					12,
					1000);
			fail("If a broker is added twice an exception must be thrown.");
		} catch (BrokerAlreadyConnectedException expected) {
			assertEquals(data,expected.getBrokerConnectionData());
		} 
    }
}
