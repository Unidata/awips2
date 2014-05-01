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
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;

import junit.framework.TestCase;

import org.apache.qpid.management.TestConstants;
import org.apache.qpid.management.configuration.ConfigurationException;
import org.apache.qpid.management.configuration.Configurator;
import org.apache.qpid.management.domain.handler.impl.MethodOrEventDataTransferObject;
import org.apache.qpid.management.domain.model.QpidClass.QManManagedObject;

/**
 * Test case for Qpid Class.
 * 
 * @author Andrea Gazzarini
 */
public class QpidClassTest extends TestCase
{
    private QpidClass _class;
    private QpidPackage _package;
    
    @Override
    protected void setUp () throws ConfigurationException
    {
    	Configurator configurator = new Configurator();
    	configurator.configure();
    	_package = new QpidPackage(TestConstants.QPID_PACKAGE_NAME,TestConstants.DOMAIN_MODEL);
        _class = new QpidClass(TestConstants.EXCHANGE_CLASS_NAME,TestConstants.HASH,_package);
    }
        
    /**
     * Tests the execution of the getObjectInstance() method.
     * Basically it tests the addition of a new object instance.
     * 
     * <br>precondition: class has no object instances.
     * <br>precondition : class contains the added object instance.
     */
    public void testGetObjectInstance() 
    {            
        assertFalse (
                "Nobody set instance #"+TestConstants.OBJECT_ID+" into this class so why is it there?", 
        		_class._objectInstances.containsKey(TestConstants.OBJECT_ID));

        QManManagedObject instance = _class.getObjectInstance(TestConstants.OBJECT_ID, false);
        
        assertTrue (
                "Now the instance #"+TestConstants.OBJECT_ID+" should be there...",
                _class._objectInstances.containsKey(TestConstants.OBJECT_ID));
        
        assertEquals(instance,_class.getObjectInstance(TestConstants.OBJECT_ID, false));
    }
       
    /**
     * Tests the injection of instrumentation and configuration data (related to a specific object instance) before the 
     * schema is installed.
     * 
     * <br>precondition : the schema hasn't yet installed on this class.
     * <br>postcondition : incoming configuration & instrumentation data is stored into the corresponding object instance.
     */
    public void testAddInstrumentationAndConfigurationDataBeforeSchemaInstallation() 
    {
        _class._state = _class._schemaRequestedButNotYetInjected;
        QManManagedObject objectInstance = _class.getObjectInstance(TestConstants.OBJECT_ID,false);
        
        assertTrue(
                "This object instance is a new one so how is it possible that it has already instrumentation data? ",
                objectInstance._rawInstrumentationData.isEmpty());
        assertTrue(
                "This object instance is a new one so how is it possible that it has already configuration data? ",
                objectInstance._rawConfigurationData.isEmpty());
        
        byte [] dummyConfigurationData = {1,2,3,4,5,6,7,8};
        byte [] dummyInstrumentationData = {11,21,31,41,51,61,71,81};
        
        _class.addConfigurationData(TestConstants.OBJECT_ID, dummyConfigurationData);
        _class.addInstrumentationData(TestConstants.OBJECT_ID, dummyInstrumentationData);
        
        assertEquals("Now configuration data should be there...",1,objectInstance._rawConfigurationData.size());
        assertEquals("Now instrumentation data should be there...",1,objectInstance._rawInstrumentationData.size());
        
        assertTrue(
                "Object instance configuration data should be the previously set...",
                Arrays.equals(objectInstance._rawConfigurationData.get(0), 
                dummyConfigurationData));
        
        assertTrue(
                "Object instance instrumentation data should be the previously set...",
                Arrays.equals(objectInstance._rawInstrumentationData.get(0), 
                dummyInstrumentationData));        
    }
    
    /**
     * Tests the internal state change of a class definition.
     */
    public void testStateChange() throws UnableToBuildFeatureException
    {
    	_class = new QpidClass(TestConstants.EXCHANGE_CLASS_NAME,TestConstants.HASH,_package)
    	{
    		@Override
    		void requestSchema() throws Exception {
    			_state = _schemaRequestedButNotYetInjected;
    		}
    		
    		@Override
    		void setSchema(List<Map<String, Object>> propertyDefinitions,
    				List<Map<String, Object>> statisticDefinitions,
    				List<MethodOrEventDataTransferObject> methodDefinitions) throws UnableToBuildFeatureException {
    			_state = _schemaInjected;
    		}
    	};
    	
    	assertSame(
    			"Initial state doesn't match.",
    			_class._schemaNotRequested,
    			_class._state);
    	
    	_class.addConfigurationData(TestConstants.OBJECT_ID, TestConstants.TEST_RAW_DATA);
    	_class.addInstrumentationData(TestConstants.OBJECT_ID, TestConstants.TEST_RAW_DATA);

    	assertSame(
    			"Request schema has been requested but not yet injected. The current state is not indicating that!",
    			_class._schemaRequestedButNotYetInjected,
    			_class._state);    	
    	
    	_class.setSchema(
    			TestConstants.EMPTY_PROPERTIES_SCHEMA, 
    			TestConstants.EMPTY_STATISTICS_SCHEMA, 
    			new LinkedList<MethodOrEventDataTransferObject>());
    	
    	assertSame(
    			"Request schema has been injected. The current state is not indicating that!",
    			_class._schemaInjected,
    			_class._state);    	
    }
    
    /**
     * Tests the injection of a valid schema on a QpidClass.
     * 
     * <br>precondition : a valid arguments is injected on the qpid class.
     * <br>postcondition : class definition is built successfully.
     */
    public void testSchemaInjectionOk() throws UnableToBuildFeatureException
    {
    	_class = new QpidClass(TestConstants.EXCHANGE_CLASS_NAME,TestConstants.HASH,_package)
    	{
    		@Override
    		void requestSchema() throws Exception 
    		{
    			// DO NOTHING : QMan is not running and therefore the schema will be manually injected.
    		}
    		
    		 @Override
    		void updateInstanceWithConfigurationData(QManManagedObject instance, byte[] rawData) 
    		{
    			// DO NOTHING Given raw data is not valid so it cannot be converted.
			}    		
    	};
    	
        // Incoming configuration data : that will fire the schema request and a state change 
    	// from schema-not-requested to schema-requested-but-not-injected
    	_class.addConfigurationData(TestConstants.OBJECT_ID, TestConstants.TEST_RAW_DATA);
    	
        // I must be sure that what is obvious for me it's obvious for QMan... :)
        assertSame(
    			"Request schema has been requested but not yet injected. The current state is not indicating that!",
    			_class._schemaRequestedButNotYetInjected,
    			_class._state);
        
        List<Map<String,Object>> propertyDefinitions = new ArrayList<Map<String,Object>>(2);
        propertyDefinitions.add(
        		createProperty(
	        		TestConstants.AGE_ATTRIBUTE_NAME, 
	        		1, 
	        		TestConstants.YEARS, 
	        		TestConstants.SAMPLE_MIN_VALUE, 
	        		TestConstants.SAMPLE_MAX_VALUE, 
	        		null,
	        		TestConstants.AGE_ATTRIBUTE_DESCRIPTION, 
	        		TestConstants._1,
	        		false, 
	        		TestConstants._0));
      
        propertyDefinitions.add(
        		createProperty(
	        		TestConstants.SURNAME_ATTRIBUTE_NAME, 
	        		TestConstants.SAMPLE_ACCESS_CODE,
	        		null,
	        		null, 
	        		null, 
	        		TestConstants.SAMPLE_MAX_VALUE,
	        		TestConstants.SURNAME_ATTRIBUTE_DESCRIPTION, 
	        		TestConstants._1,
	        		true, 
	        		TestConstants._1));
        
        _class.setSchema(propertyDefinitions, TestConstants.EMPTY_STATISTICS_SCHEMA, TestConstants.EMPTY_METHODS_SCHEMA);
        
        assertEquals(2,_class._properties.size());
        
        QpidProperty property = _class._properties.get(TestConstants.AGE_ATTRIBUTE_NAME);
        assertEquals(TestConstants.AGE_ATTRIBUTE_NAME,property.getName());
        assertEquals(AccessMode.RC,property.getAccessMode());
        assertEquals(TestConstants.YEARS,property.getUnit());
        assertEquals(TestConstants.SAMPLE_MIN_VALUE,property.getMinValue());
        assertEquals(TestConstants.SAMPLE_MAX_VALUE,property.getMaxValue());
        assertEquals(Integer.MIN_VALUE,property.getMaxLength());
        assertEquals(TestConstants.AGE_ATTRIBUTE_DESCRIPTION,property.getDescription());
        assertEquals(Short.class,property.getJavaType());
        assertFalse(property.isOptional());
        
        property = _class._properties.get(TestConstants.SURNAME_ATTRIBUTE_NAME);
        assertEquals(TestConstants.SURNAME_ATTRIBUTE_NAME,property.getName());
        assertEquals(AccessMode.RC,property.getAccessMode());
        assertNull(property.getUnit());
        assertEquals(Integer.MIN_VALUE,property.getMinValue());
        assertEquals(Integer.MIN_VALUE,property.getMaxValue());
        assertEquals(TestConstants.SAMPLE_MAX_VALUE,property.getMaxLength());
        assertEquals(TestConstants.SURNAME_ATTRIBUTE_DESCRIPTION,property.getDescription());
        assertEquals(Short.class,property.getJavaType());
        assertTrue(property.isOptional());
        
        MBeanInfo mbeanInfo = _class._metadata;
        assertEquals(TestConstants.EXCHANGE_CLASS_NAME,mbeanInfo.getClassName());
        
        MBeanAttributeInfo [] attributes = mbeanInfo.getAttributes();
        assertEquals(2,attributes.length);
        
        MBeanAttributeInfo attribute = attributes[0];
        assertEquals(TestConstants.AGE_ATTRIBUTE_NAME,attribute.getName());
        assertEquals(TestConstants.AGE_ATTRIBUTE_DESCRIPTION,attribute.getDescription());
        assertFalse(attribute.isWritable());
        assertTrue(attribute.isReadable());
        assertEquals(Short.class.getName(),attribute.getType());
        
        attribute = attributes[1];
        assertEquals(TestConstants.SURNAME_ATTRIBUTE_NAME,attribute.getName());
        assertEquals(TestConstants.SURNAME_ATTRIBUTE_DESCRIPTION,attribute.getDescription());
        assertFalse(attribute.isWritable());
        assertTrue(attribute.isReadable());
        assertEquals(Short.class.getName(),attribute.getType());
    }
        
    /**
     * Tests the behaviour of the class when a schema request can't be made.
     * 
     * <br>precondition : class must be in "schema-not-requested" state when incoming data arrives.
     * <br>postcondition : no exception is thrown and no state transition happens.
     */
    public void testStateChange_withRequestSchemaFailure()
    {
    	_class= new QpidClass(TestConstants.EXCHANGE_CLASS_NAME,TestConstants.HASH,_package)
    	{
    		@Override
    		void requestSchema() throws Exception {
    			throw new Exception();
    		}
    		
    		@Override
    		void setSchema(
    				List<Map<String, Object>> propertyDefinitions,
    				List<Map<String, Object>> statisticDefinitions,
    				List<MethodOrEventDataTransferObject> methodDefinitions) throws UnableToBuildFeatureException 
    		{
    		}
    	};
    	
    	assertSame(
    			"Initial state must be schema-not-requested.",
    			_class._schemaNotRequested,
    			_class._state);

    	_class.addInstrumentationData(TestConstants.OBJECT_ID, TestConstants.TEST_RAW_DATA);
    	
    	assertSame(
    			"Current state must be still schema-not-requested.",
    			_class._schemaNotRequested,
    			_class._state);
    }

    /**
     * Tests the behaviour of the class when a schema injection fails.
     * 
     * <br>precondition : class must be in "schema-not-requested" state when incoming data arrives.
     * <br>postcondition : an exception is thrown and no state transition happens.
     */
    public void testStateChange_withSchemaInjectionFailure()
    {
    	_class = new QpidClass(TestConstants.EXCHANGE_CLASS_NAME,TestConstants.HASH,_package)
    	{
    		@Override
    		void requestSchema() throws Exception 
    		{
    			// DO NOTHING.
    		}
    		
    		@Override
    		void setSchema(List<Map<String, Object>> propertyDefinitions,
    				List<Map<String, Object>> statisticDefinitions,
    				List<MethodOrEventDataTransferObject> methodDefinitions)
    				throws UnableToBuildFeatureException 
    		{
    			throw new UnableToBuildFeatureException("");
    		}
    	};
    	
    	assertSame(
    			"Initial state must be schema-not-requested.",
    			_class._schemaNotRequested,
    			_class._state);
    	
    	_class.addInstrumentationData(TestConstants.OBJECT_ID, TestConstants.TEST_RAW_DATA);
    	
      	assertSame(
    			"Request schema has been requested but not yet injected. The current state is not indicating that!",
    			_class._schemaRequestedButNotYetInjected,
    			_class._state);
      	
      	try {
			_class.setSchema(
					TestConstants.EMPTY_PROPERTIES_SCHEMA,
					TestConstants.EMPTY_STATISTICS_SCHEMA,
					TestConstants.EMPTY_METHODS_SCHEMA);
			fail("If we are here something was wrong becuase the setSchema() of this event is throwing an exception...");
		} catch (UnableToBuildFeatureException expected) {
	      	assertSame(
	    			"Request schema has been requested but not yet injected. The current state is not indicating that!",
	    			_class._schemaRequestedButNotYetInjected,
	    			_class._state);
		}
    }    
    
    private Map<String,Object> createProperty(
    		String name,
    		Integer accessCode, 
    		String unit,
    		Integer min, 
    		Integer max, 
    		Integer maxLength,
    		String description,
    		Integer type,
    		boolean optional,
    		Integer index)
    {
        Map <String,Object> result = new HashMap<String, Object>();
        result.put(QpidFeatureBuilder.Attribute.name.name(),name);
        result.put(QpidFeatureBuilder.Attribute.access.name(), accessCode);

        if (unit != null)
        {
        	result.put(QpidFeatureBuilder.Attribute.unit.name(),unit);
        }
        
        if (min != null)
        {
        	result.put(QpidFeatureBuilder.Attribute.min.name(), min);
        }
        
        if (max != null) 
        {
            result.put(QpidFeatureBuilder.Attribute.max.name(),max);        	
        }

        if (maxLength != null) 
        {
            result.put(QpidFeatureBuilder.Attribute.maxlen.name(),maxLength);        	
        }
        
        result.put(QpidFeatureBuilder.Attribute.desc.name(), description);
        result.put(QpidFeatureBuilder.Attribute.type.name(), type);
        result.put(QpidFeatureBuilder.Attribute.optional.name(),optional ? 1 : 0);

        if (index != null)
        {
        	result.put(QpidFeatureBuilder.Attribute.index.name(), index);
        }
    	return result;
    }    
}
