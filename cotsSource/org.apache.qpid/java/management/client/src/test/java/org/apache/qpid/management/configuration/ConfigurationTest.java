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

import org.apache.qpid.management.TestConstants;
import org.apache.qpid.management.domain.handler.base.IMessageHandler;
import org.apache.qpid.management.domain.handler.impl.ConfigurationMessageHandler;
import org.apache.qpid.management.domain.handler.impl.InstrumentationMessageHandler;
import org.apache.qpid.management.domain.handler.impl.SchemaResponseMessageHandler;

/**
 * Test case for Configuration singleton.
 */
public class ConfigurationTest extends TestCase
{
    /**
     * Tests the singleton behaviour of the configuration object.
     */
    public void testSingleton()
    {
        assertSame(Configuration.getInstance(),Configuration.getInstance());
    }
        
    /**
     * Tests the execution of getType() method when a unknown code is supplied.
     * 
     * <br>precondition : the requested type doesn't exist on the configuration.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testGetTypeKO()
    {
        try
        {
            Configuration.getInstance().getType(TestConstants.VALID_CODE*10001);
            fail("If an unknwon code is supplied an exception must be thrown.");
        } catch (UnknownTypeCodeException expected)
        {
            assertEquals(TestConstants.VALID_CODE*10001,expected.getCode());
        }        
    }
        
    /**
     * Tests the execution of getAccessMode() method when a unknown code is supplied.
     * 
     * <br>precondition : the requested access mode doesn't exist on the configuration.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testGetAccessModeKO()
    {
        try
        {
            Configuration.getInstance().getAccessMode(TestConstants.VALID_CODE*1528);
            fail("If an unknwon code is supplied an exception must be thrown.");
        } catch (UnknownAccessCodeException expected)
        {
            assertEquals(TestConstants.VALID_CODE*1528,expected.getCode());
        }        
    }    
    
    /**
     * Tests the execution of the getBrokerConnectionData when a valid broker id is supplied.
     * 
     * <br>precondition : on configuration a connection data is stored and associated with the supplied id.
     * <br>postcondition : the requested connection data is returned and no exception is thrown.
     */
    public void testGetBrokerConnectionDataOK() throws Exception 
    {
        BrokerConnectionData connectionData = new BrokerConnectionData();
        connectionData.setHost("host");
        connectionData.setPort("7001");
        connectionData.setInitialPoolCapacity("0");
        connectionData.setMaxPoolCapacity("10");
        connectionData.setMaxWaitTimeout("1");
        Configuration.getInstance().addBrokerConnectionData(TestConstants.BROKER_ID, connectionData);
        
        BrokerConnectionData result = Configuration.getInstance().getBrokerConnectionData(TestConstants.BROKER_ID);
        assertSame(connectionData, result);
    }
    
    /**
     * Tests the execution of the getBrokerConnectionData when a unknown broker id is supplied.
     * 
     * <br>precondition : on configuration there's no connection data associated with the supplied id.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testGetBrokerConnectionDataKO_withUnknownBrokerId() 
    {
        UUID brokerId = UUID.randomUUID();
        try 
        {
            Configuration.getInstance().getBrokerConnectionData(brokerId);
            fail("If an unknown broker id is supplied then an exception must be thrown.");
        } catch(UnknownBrokerException expected) 
        {
            assertEquals(brokerId.toString(),expected.getMessage());
        }
    }    
    
    /**
     * Tests the execution of the getBrokerConnectionData when a null id is supplied.
     * 
     * <br>precondition : a null broker is given.
     * <br>postcondition : an exception is thrown indicating the failure.
     */
    public void testGetBrokerConnectionDataKO_withNullBrokerId() 
    {
        try 
        {
            Configuration.getInstance().getBrokerConnectionData(null);
            fail("If a null broker id is supplied then an exception must be thrown.");
        } catch(UnknownBrokerException expected) 
        {
        }
    }       

    /**
     * Tests the behaviour of the getManagementQueueHandlers() method.
     * 
     * <br>precondition: 2 management handlers are in stored configuration
     * <br>postcondition : 2 management handlers are returned.
     */
    public void testGetManagementQueueHandlersOk() 
    {        
        IMessageHandler instrMessageHandler = new InstrumentationMessageHandler();
        IMessageHandler configMessageHandler = new ConfigurationMessageHandler();
        
        MessageHandlerMapping instrMapping = new MessageHandlerMapping('i',instrMessageHandler);
        MessageHandlerMapping configMapping = new MessageHandlerMapping('c',configMessageHandler);
                
        Configuration.getInstance().addManagementMessageHandlerMapping(instrMapping);
        Configuration.getInstance().addManagementMessageHandlerMapping(configMapping);
        
        Map<Character, IMessageHandler> handlerMappings = Configuration.getInstance().getManagementQueueHandlers();
        
        assertEquals(2,handlerMappings.size());
        assertEquals(instrMessageHandler,handlerMappings.get(instrMapping.getOpcode()));
        assertEquals(configMessageHandler,handlerMappings.get(configMapping.getOpcode()));        
    }
    
    /**
     * Tests the behaviour of the getManagementQueueHandlers() method.
     * 
     * <br>precondition: 2 management handlers are in stored configuration
     * <br>postcondition : 2 management handlers are returned.
     */
    public void testGetMethodReplyQueueHandlersOk() 
    {
        IMessageHandler schemaMessageHandler = new SchemaResponseMessageHandler();
        
        MessageHandlerMapping schemaMapping = new MessageHandlerMapping('s',schemaMessageHandler);
        
        Configuration.getInstance().addMethodReplyMessageHandlerMapping(schemaMapping);
        
        Map<Character, IMessageHandler> handlerMappings = Configuration.getInstance().getMethodReplyQueueHandlers();
        
        assertEquals(schemaMessageHandler,handlerMappings.get(schemaMapping.getOpcode()));
    }    
}
