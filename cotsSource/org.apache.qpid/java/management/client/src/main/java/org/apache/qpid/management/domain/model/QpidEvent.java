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

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.ObjectName;
import javax.management.ReflectionException;
import javax.management.RuntimeOperationsException;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.model.type.Binary;
import org.apache.qpid.management.jmx.EntityLifecycleNotification;
import org.apache.qpid.transport.codec.BBDecoder;

/**
 * Qpid event definition.
 */
class QpidEvent extends QpidEntity implements QpidEventMBean
{        
    
    /**
     * State interface for this event definition.
     * Each state is responsible to handle the injection of the data and / or schema. 
     */
    interface State 
    {    
        /**
         * Adds the given data for the object instance associated to the given object identifier.
         * 
         * @param rawData the raw configuration data.
         */
        void addNewEventData (byte[] rawData, long currentTimestamp, int severity);

        /**
         * Inject the schema into this class definition.
         * 
         * @param propertyDefinitions
         * @param statisticDefinitions
         * @param methodDefinitions
         * @throws UnableToBuildFeatureException when it's not possibile to parse schema and build the class definition.
         */
        public void setSchema (List<Map<String, Object>> agumentDefinitions) throws UnableToBuildFeatureException;
    };

	
    /**
     * This is the initial state of every qpid class. 
     * The class definition instance is created but its schema has not been injected. 
     * Incoming configuration & instrumentation data will be stored in raw format because we don't know how to 
     * parse it until the schema arrives.
     * In addition, this state is responsible (when data arrives) to request its schema.
     */
    final State _schemaNotRequested = new State() {

        /**
         * Stores the incoming data in raw format and request the schema for this class.
         * After that a transition to the next state is made.
         * 
         * @param objectId the object instance identifier.
         * @param rawData incoming configuration data.
         */
        public synchronized void addNewEventData (byte[] rawData, long currentTimestamp, int severity)
        {
            try
            {
                requestSchema();  
                _state = _schemaRequestedButNotYetInjected;
            } catch (Exception exception)
            {
                _logger.error(
                		exception,
                        Messages.QMAN_100015_UNABLE_TO_SEND_SCHEMA_REQUEST, 
                        _parent.getName(),
                        _name);
            } finally {
                createEventInstance(rawData,currentTimestamp,severity);
            }
        }

        /**
         * This method only throws an illegal state exception because when a schema arrives 
         * this state is no longer valid.
         */
        public  void  setSchema (List<Map<String, Object>> agumentDefinitions) throws UnableToBuildFeatureException
        {
            throw new IllegalStateException("When a schema arrives it's not possible for this event to be in this state.");
        }
    };
    
    /**
     * This is the first state of this class definition : the schema is not yet injected so each injection of object data will be 
     * retained in raw format.
     */
    final State _schemaRequestedButNotYetInjected = new State()
    {
        /**
         * Stores the incoming data in raw format and request the schema for this class.
         * After that a transition to the next state is made.
         * 
         * @param objectId the object instance identifier.
         * @param rawData incoming configuration data.
         */
        public synchronized void addNewEventData (byte[] rawData,long currentTimestamp, int severity)
        {
        	createEventInstance(rawData,currentTimestamp, severity);
        }

        /**
         * When a schema is injected into this defintiion the following should happen :
         * 1) the incoming schema is parsed and the class definition is built;
         * 2) the retained raw data is converted into object instance(s)
         * 3) the internal state of this class changes;
         * 
         * If someting is wrong during that process the schema is not built and the state don't change.
         */
        public synchronized void setSchema (List<Map<String, Object>> argumentDefinitions) throws UnableToBuildFeatureException
        {
        	MBeanAttributeInfo [] attributesMetadata = new MBeanAttributeInfo[argumentDefinitions.size()+2];
                
        	buildArguments(argumentDefinitions, attributesMetadata);

        	_metadata = new MBeanInfo(_name,_name,attributesMetadata,null,null,null);
            
            // Converting stored object instances into JMX MBean and removing raw instance data.
            for (QManManagedEvent instance : _eventInstances)
            {                    
            	updateEventInstanceWithData(instance);
                registerEventInstance(instance,_parent.getOwnerId(),_parent.getName(),_name);
            }
            _state = _schemaInjected;
            
            EntityLifecycleNotification notification = new EntityLifecycleNotification(
              		 EntityLifecycleNotification.SCHEMA_INJECTED_NOTIFICATION_TYPE,
              		 _parent.getName(), 
              		 _name, 
              		 Names.EVENT,
              		 _objectName);
               
               sendNotification(notification);
        }
    };
    
    /**
     * After a schema is built into this definition this is the current state of the class.
     */
    final State _schemaInjected = new State()
    {
        /**
         * Updates the configuration state of the object instance associates with the given object identifier.
         * 
         * @param objectId the object identifier.
         * @param rawData the configuration data (raw format).
         */
        public void addNewEventData (byte[] rawData,long currentTimestamp, int severity)
        {
            QManManagedEvent instance = createEventInstance(rawData,currentTimestamp, severity);            
            updateEventInstanceWithData(instance);
            registerEventInstance(instance,_parent.getOwnerId(),_parent.getName(),_name);
        }
        
        /**
         * Never called when the class definition has this state.
         */
        public  void  setSchema (List<Map<String, Object>> agumentDefinitions) throws UnableToBuildFeatureException
        {
           // N.A. : Schema is already injected.
        }
    };
    
    /**
     * MBean used for representing remote broker object instances.
     * This is the core component of the QMan domain model
     * 
     * @author Andrea Gazzarini
     */
    class QManManagedEvent extends QManManagedEntity
    {        


        // Arrays used for storing raw data before this mbean is registered to mbean server.
        final byte[] _rawEventData;
        final long _timestamp;
        final int _severity;
        
        /**
         * Builds a new managed object with the given object identifier.
         * 
         * @param objectId the object identifier.
         */
        private QManManagedEvent(byte [] data, long timestamp, int severity)
        {
        	this._rawEventData = data;
        	this._timestamp = timestamp;
        	this._severity = severity;
        	_attributes.put(SEVERITY_ATTR_NAME, _severity);
        	_attributes.put(TIMESTAMP_ATTR_NAME, new Date(_timestamp));
        }
        
        /**
         * Returns the value of the given attribute.s
         * 
         * @throws AttributeNotFoundException when no attribute is found with the given name.
         */
        public Object getAttribute (String attributeName) throws AttributeNotFoundException, MBeanException, ReflectionException
        {
            if (attributeName == null) 
            {
                throw new RuntimeOperationsException(new IllegalArgumentException("Attribute name must not be null."));
            }
            
            if (_arguments.containsKey(attributeName) || SEVERITY_ATTR_NAME.equals(attributeName) || TIMESTAMP_ATTR_NAME.equals(attributeName))
            {
                return _attributes.get(attributeName);  
            } else 
            {
                throw new AttributeNotFoundException(attributeName);
            }        
        }

        /**
         * Executes an operation on this object instance.
         * 
         * @param actionName the name of the method.
         * @param params the method parameters 
         * @param signature the method signature.
         */
        public Object invoke (String actionName, Object[] params, String[] signature) throws MBeanException,ReflectionException
        {
           throw new ReflectionException(new NoSuchMethodException(actionName));
        }

        /**
         * Sets the value of the given attribute on this object instance.
         * 
         * @param attribute contains the new value of the attribute.
         * @throws AttributeNotFoundException when the given attribute is not found on this object instance.
         * @throws InvalidAttributeValueException when the given value is violating one attribute invariant.
         */
        public void setAttribute (Attribute attribute) throws AttributeNotFoundException,
                InvalidAttributeValueException, MBeanException, ReflectionException
        {
            throw new ReflectionException(new NoSuchMethodException());
        }

        /**
         * Sets the values of several attributes of this MBean.
         *
         * @param attributes a list of attributes: The identification of the attributes to be set and the values they are to be set to.
         * @return  The list of attributes that were set, with their new values.
         */
        public AttributeList setAttributes (AttributeList attributes)
        {
            throw new RuntimeException();
        }
    }

    final static String SEVERITY_ATTR_NAME = "Severity";
    final static String TIMESTAMP_ATTR_NAME = "Date";
    
    private List<QpidProperty> _schemaOrderedArguments = new ArrayList<QpidProperty>();
    
    Map<String, QpidProperty> _arguments  = new HashMap<String, QpidProperty>();     
    List<QManManagedEvent> _eventInstances = new LinkedList<QManManagedEvent>();
    State _state = _schemaNotRequested;;
        
    /**
     * Builds a new class with the given name and package as parent.
     * 
     * @param className the name of the class.
     * @param hash the class schema hash.
     * @param parentPackage the parent of this class.
     */
    QpidEvent(String eventClassName, Binary hash, QpidPackage parentPackage)
    {
    	super(eventClassName,hash,parentPackage,Names.EVENT);
    }
    
    /**
     * Adds the configuration data for the object instance associated to the given object identifier.
     * 
     * @param objectId the object identifier.
     * @param rawData the raw configuration data.
     */
    void addEventData (byte[] rawData, long currentTimestamp, int severity)
    {
        _logger.debug(
        		Messages.QMAN_200021_INCOMING_EVENT_DATA,
                _parent.getOwnerId(),
                _parent.getName(),
                _name);        
        _state.addNewEventData(rawData, currentTimestamp, severity);
    }
    
    /**
     * Sets the schema for this class definition. 
     * A schema is basically a metadata description of all properties, statistics, methods and events of this class.
     * 
     * @param propertyDefinitions properties metadata.
     * @param statisticDefinitions statistics metadata.
     * @param methodDefinitions methods metadata.
     * @throws UnableToBuildFeatureException when some error occurs while parsing the incoming schema.
     */
     void setSchema (List<Map<String, Object>> argumentDefinitions) throws UnableToBuildFeatureException
    {
         _logger.info(Messages.QMAN_000010_INCOMING_SCHEMA,_parent.getOwnerId(),_parent.getName(),_name);
        _state.setSchema(argumentDefinitions);
    }    

    /**
     * Internal method used for building attributes definitions.
     * 
     * @param props the map contained in the properties schema.
     * @param stats the map contained in the statistics schema.
     * @param attributes the management metadata for attributes.
     * @throws UnableToBuildFeatureException  when it's not possibile to build one attribute definition.
     */
    void buildArguments (
            List<Map<String, Object>> arguments,MBeanAttributeInfo[] attributes) throws UnableToBuildFeatureException
    {
        int index = 0;
        
        for (Map<String, Object> argumentDefinition : arguments)
        {
        	// Force metadata attributes. It is needed because arguments are "similar" to properties but they 
        	// aren't properties and then they haven't optional, index and access metadata attributes 
        	// (mandatory for build a property definition).
        	argumentDefinition.put(QpidFeatureBuilder.Attribute.optional.name(),0);
        	argumentDefinition.put(QpidFeatureBuilder.Attribute.index.name(),1);        	
        	argumentDefinition.put(QpidFeatureBuilder.Attribute.access.name(),3);        	        	
            
        	QpidFeatureBuilder builder = QpidFeatureBuilder.createPropertyBuilder(argumentDefinition);
            builder.build();
            
            QpidProperty argument = (QpidProperty) builder.getQpidFeature();           
                        
            _arguments.put(argument.getName(),argument);
            _schemaOrderedArguments.add(argument);
            attributes[index++]=(MBeanAttributeInfo) builder.getManagementFeature();
                       
            _logger.debug(
                    Messages.QMAN_200019_EVENT_ARGUMENT_DEFINITION_HAS_BEEN_BUILT,
                    _parent.getName(),
                    _name,
                    argument);
        }

        attributes[index++] = new MBeanAttributeInfo(
        		SEVERITY_ATTR_NAME,
			      	Integer.class.getName(),
			      	Messages.EVENT_SEVERITY_ATTRIBUTE_DESCRIPTION,
			      	true,
			      	false,
		      	false);
        
        attributes[index++] = new MBeanAttributeInfo(
        		TIMESTAMP_ATTR_NAME,
			      	Date.class.getName(),
			      	Messages.EVENT_TIMESTAMP_ATTRIBUTE_DESCRIPTION,
			      	true,
			      	false,
		      	false);
    }    
    
    /**
     * Returns the object instance associated to the given identifier.
     * Note that if the identifier is not associated to any obejct instance, a new one will be created.
     * 
     * @param objectId the object identifier.
     * @param registration a flag indicating whenever the (new ) instance must be registered with MBean server.
     * @return the object instance associated to the given identifier.
     */
    QManManagedEvent createEventInstance(byte [] data, long timestamp, int severity) 
    {
        QManManagedEvent eventInstance = new QManManagedEvent(data, timestamp, severity);
        _eventInstances.add(eventInstance);
        return eventInstance;
    }
    
    /**
     * Updates the given obejct instance with the given incoming configuration data.
     * 
     * @param instance the managed object instance.
     * @param rawData the incoming configuration data which contains new values for instance properties.
     */
    void updateEventInstanceWithData(QManManagedEvent instance)
    {
        BBDecoder decoder = new BBDecoder();
        decoder.init(ByteBuffer.wrap(instance._rawEventData));

        for (QpidProperty property : _schemaOrderedArguments)
        {                  
            try {
                Object value = property.decodeValue(decoder);
                instance.createOrReplaceAttributeValue(property.getName(),value);             
            } catch(Exception ignore) {
                _logger.error(Messages.QMAN_100016_UNABLE_TO_DECODE_FEATURE_VALUE, _parent.getName(),_name,property.getName());
            }
        }
    }
       
    @Override
    public String toString ()
    {
        return new StringBuilder()
            .append(_parent.getOwnerId())
            .append("::")
            .append(_parent.getName())
            .append(".")
            .append(_name)
            .toString();
    }

    /**
     * Deregisters all the object instances and release all previously acquired resources.
     */
    void releaseResources ()
    {
    	_eventInstances.clear();
    	JMX_SERVICE.unregisterEvents();
    	JMX_SERVICE.unregisterClassDefinitions();   
        _service.close();
    }

    /**
     * Checks if this event definition contains event instance(s).
     * 
     * @return true if there is one or more managed instances.
     */
	boolean hasNoInstances() 
	{
		return _eventInstances.isEmpty();
	}
	
	/**
	 * Compose method used for registering an mbean (event) instance.
	 * 
	 * @param instance the mbean event.
	 * @param brokerId the broker identifier.
	 * @param packageName the package name.
	 * @param eventClassName the event class name.
	 */
	private void registerEventInstance(
			QManManagedEvent instance,
			UUID brokerId, 
			String packageName, 
			String eventClassName) 
	{
		ObjectName objectName = JMX_SERVICE.registerEventInstance(instance,brokerId,packageName,eventClassName);
		
   	 EntityLifecycleNotification notification = new EntityLifecycleNotification(
			 EntityLifecycleNotification.INSTANCE_ADDED_NOTIFICATION_TYPE,
			 packageName,
			 eventClassName,
			 Names.EVENT,
			 objectName);
	 
	 sendNotification(notification);
	}
}
