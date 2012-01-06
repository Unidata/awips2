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
package org.apache.qpid.management.domain.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.ObjectName;
import javax.management.ReflectionException;

import junit.framework.TestCase;

import org.apache.qpid.management.TestConstants;
import org.apache.qpid.management.configuration.ConfigurationException;
import org.apache.qpid.management.configuration.Configurator;
import org.apache.qpid.management.domain.model.QpidEvent.QManManagedEvent;

/**
 * Test case for qpid class entity.
 * 
 * @author Andrea Gazzarini
 */
public class QpidEventTest extends TestCase
{
    private QpidEvent _event;
    private QpidPackage _qpidPackage;
    
    @Override
    protected void setUp () throws Exception
    {
        _qpidPackage = new QpidPackage(TestConstants.QPID_PACKAGE_NAME,TestConstants.DOMAIN_MODEL);
        _event = new QpidEvent(TestConstants.BIND_EVENT_NAME,TestConstants.HASH,_qpidPackage);
    }
        
    /**
     * Tests the execution of the createEventInstance() method.
     * Basically it tests the addition of a new event instance.
     * 
     * <br>precondition: event deifinition has no object instances.
     * <br>precondition : event definition contains the new object instance.
     */
    public void testCreateEventInstance() 
    {            
        assertTrue(
                "A just created event should be empty. I mean there shouldn't be event instances inside.", 
        		_event.hasNoInstances());

        QManManagedEvent instance = createEventInstance();
        
        assertFalse (
                "Now a new instance should be there...",
                _event.hasNoInstances());
        
        assertEquals(TestConstants.TEST_RAW_DATA,instance._rawEventData);
        assertEquals(TestConstants.NOW,instance._timestamp);
        assertEquals(TestConstants.SEVERITY, instance._severity);
    }
    
    /**
     * Tests the internal state change of an event definition.
     */
    public void testStateChange() throws UnableToBuildFeatureException
    {
    	// Let's override this class because this is not an online tests and therefore
    	// QMan is not supposed to be up.
    	_event = new QpidEvent(TestConstants.BIND_EVENT_NAME,TestConstants.HASH,_qpidPackage)
    	{
    		@Override
    		void requestSchema() throws Exception {
    			// Do Nothing.
    		}
    		
    		@Override
    		void setSchema(List<Map<String, Object>> argumentDefinitions)throws UnableToBuildFeatureException {
    			_state = _schemaInjected;
    		}
    	};
    	
    	assertSame(
    			"Initial state doesn't match.",
    			_event._schemaNotRequested,
    			_event._state);
    	
    	_event.addEventData(TestConstants.TEST_RAW_DATA, TestConstants.NOW, TestConstants.SEVERITY);
    	
    	assertSame(
    			"Request schema has been requested but not yet injected. The current state is not indicating that!",
    			_event._schemaRequestedButNotYetInjected,
    			_event._state);
    	
    	_event.setSchema(TestConstants.EMPTY_ARGUMENTS_SCHEMA);
    	
    	assertSame(
    			"Request schema has been injected. The current state is not indicating that!",
    			_event._schemaInjected,
    			_event._state);    	
    }
    
    /**
     * Tests the injection of a valid schema on a QpidEvent.
     * 
     * <br>precondition : a valid arguments is injected on the qpid event.
     * <br>postcondition : event definition is built successfully.
     */
    public void testSchemaInjectionOK() throws UnableToBuildFeatureException, ConfigurationException, InstanceNotFoundException, MBeanException, ReflectionException
    {
    	_event = new QpidEvent(TestConstants.BIND_EVENT_NAME,TestConstants.HASH,_qpidPackage)
    	{
    		@Override
    		void requestSchema() throws Exception 
    		{
    			// DO NOTHING : QMan is not running and therefore the schema will be manually injected.
    		}
    		
    		@Override
    		void updateEventInstanceWithData(QManManagedEvent instance) 
    		{
    			// DO NOTHING : otherwise we should supply a valid raw data to be converted. ;-)
    		}
    	};
    	
       Configurator configurator = new Configurator();
        configurator.configure();

        List<Map<String,Object>> arguments = new ArrayList<Map<String, Object>>();
        arguments.add(createArgument(TestConstants.AGE_ATTRIBUTE_NAME, TestConstants.AGE_ATTRIBUTE_DESCRIPTION));
        arguments.add(createArgument(TestConstants.SURNAME_ATTRIBUTE_NAME, TestConstants.SURNAME_ATTRIBUTE_DESCRIPTION));
        
        // Incoming data : that will fire the schema request and a state change from schema-not-requested to schema-requested-but-not-injected
        _event.addEventData(TestConstants.TEST_RAW_DATA, TestConstants.NOW, TestConstants.SEVERITY);
        
        // I must be sure that what is obvious for me it's obvious for QMan... :)
        assertSame(
    			"Request schema has been requested but not yet injected. The current state is not indicating that!",
    			_event._schemaRequestedButNotYetInjected,
    			_event._state);
        
        // Inject schema
        _event.setSchema(arguments);

        // Arguments must be 2 + 2 (severity)
        assertEquals(2,_event._arguments.size());
        
        QpidProperty argument = _event._arguments.get(TestConstants.AGE_ATTRIBUTE_NAME);
        assertEquals(TestConstants.AGE_ATTRIBUTE_NAME,argument.getName());
        assertEquals(AccessMode.RO,argument.getAccessMode());
        assertEquals(TestConstants.AGE_ATTRIBUTE_DESCRIPTION,argument.getDescription());
        assertEquals(Short.class,argument.getJavaType());
        assertFalse(argument.isOptional());
        
        argument = _event._arguments.get(TestConstants.SURNAME_ATTRIBUTE_NAME);
        assertEquals(TestConstants.SURNAME_ATTRIBUTE_NAME,argument.getName());
        assertEquals(AccessMode.RO,argument.getAccessMode());
        assertEquals(TestConstants.SURNAME_ATTRIBUTE_DESCRIPTION,argument.getDescription());
        assertEquals(Short.class,argument.getJavaType());
        assertFalse(argument.isOptional());
        
        assertEquals(1,_event._eventInstances.size());
        
        JmxService service = new JmxService();
        Set<ObjectName> objectNames = service.getEventMBeans();
        
        assertEquals(1,objectNames.size());
    }    
    
    /**
     * Tests the behaviour of the event class when a schema request can't be made.
     * 
     * <br>precondition : event must be in "schema-not-requested" state when incoming data arrives.
     * <br>postcondition : no exception is thrown and no state transition happens.
     */
    public void testStateChange_withRequestSchemaFailure()
    {
    	_event = new QpidEvent(TestConstants.BIND_EVENT_NAME,TestConstants.HASH,_qpidPackage)
    	{
    		@Override
    		void requestSchema() throws Exception {
    			throw new Exception();
    		}
    		
    		@Override
    		void setSchema(List<Map<String, Object>> argumentDefinitions)throws UnableToBuildFeatureException {
    			_state = _schemaInjected;
    		}
    	};
    	
    	assertSame(
    			"Initial state must be schema-not-requested.",
    			_event._schemaNotRequested,
    			_event._state);
    	
    	_event.addEventData(TestConstants.TEST_RAW_DATA, TestConstants.NOW, TestConstants.SEVERITY);
    	
    	assertSame(
    			"Current state must be still schema-not-requested.",
    			_event._schemaNotRequested,
    			_event._state);
    }
    
    /**
     * Tests the behaviour of the event class when a schema injection fails.
     * 
     * <br>precondition : event must be in "schema-not-requested" state when incoming data arrives.
     * <br>postcondition : an exception is thrown and no state transition happens.
     */
    public void testStateChange_withSchemaInjectionFailure()
    {
    	_event = new QpidEvent(TestConstants.BIND_EVENT_NAME,TestConstants.HASH,_qpidPackage)
    	{
    		@Override
    		void requestSchema() throws Exception {
    			// DO NOTHING.
    		}
    		
    		@Override
    		void setSchema(List<Map<String, Object>> argumentDefinitions)throws UnableToBuildFeatureException {
    			throw new UnableToBuildFeatureException("");
    		}
    	};
    	
    	assertSame(
    			"Initial state must be schema-not-requested.",
    			_event._schemaNotRequested,
    			_event._state);
    	
    	_event.addEventData(TestConstants.TEST_RAW_DATA, TestConstants.NOW, TestConstants.SEVERITY);
    	
      	assertSame(
    			"Request schema has been requested but not yet injected. The current state is not indicating that!",
    			_event._schemaRequestedButNotYetInjected,
    			_event._state);
      	
      	try {
			_event.setSchema(TestConstants.EMPTY_ARGUMENTS_SCHEMA);
			fail("If we are here something was wrong becuase the setSchema() of this event is throwing an exception...");
		} catch (UnableToBuildFeatureException expected) {
	      	assertSame(
	    			"Request schema has been requested but not yet injected. The current state is not indicating that!",
	    			_event._schemaRequestedButNotYetInjected,
	    			_event._state);
		}
    }    

    /**
     * Factory method for qpid managed event instances.
     * 
     * @return a new QpidManagedEvent with test data inside.
     */
    private QManManagedEvent createEventInstance()
    {
    	return  _event.createEventInstance(
        		TestConstants.TEST_RAW_DATA, 
        		TestConstants.NOW,
        		TestConstants.SEVERITY);
    }
    
    /**
     * Factory method for event argument.
     * 
     * @return a new argument metadata.
     */
    private Map<String,Object> createArgument(String name,String desc)
    {
    	 Map <String,Object> argument = new HashMap<String, Object>();
         argument.put(QpidFeatureBuilder.Attribute.name.name(),name);
         argument.put(QpidFeatureBuilder.Attribute.desc.name(), desc);
         argument.put(QpidFeatureBuilder.Attribute.type.name(), TestConstants._1);       
         return argument;
    }
}
