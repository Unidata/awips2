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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.Map.Entry;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanRegistration;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.ReflectionException;
import javax.management.RuntimeOperationsException;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.handler.impl.IMethodInvocationListener;
import org.apache.qpid.management.domain.handler.impl.InvocationResult;
import org.apache.qpid.management.domain.handler.impl.MethodOrEventDataTransferObject;
import org.apache.qpid.management.domain.model.type.Binary;
import org.apache.qpid.management.domain.services.SequenceNumberGenerator;
import org.apache.qpid.management.jmx.EntityLifecycleNotification;
import org.apache.qpid.management.jmx.OperationHasBeenInvokedNotification;
import org.apache.qpid.transport.codec.BBDecoder;
import org.apache.qpid.transport.util.Logger;

/**
 * Qpid Class definition.
 * A type definition for a manageable object.
 * This class is also responsible to manage incoming obejct instance data (configuration & instrumentation). 
 * How can we handle data before schema is injected into this class? simply we must retain that data in raw format.
 * This class has 3 states : 
 * 1) first state is when schema is not yet injected. In this case the incoming object data is retained as is (in raw format) 
 * and a schema request is sent;
 * 2) second state is when schema has been requested but not yet injected. The incoming object data is still retained as is 
 * (in raw format) 
 * 3) third state is when schema is injected. Each injection of data will result in an update / create / delete of 
 * the corresponding object instance. In addition, the first time the state change, the old retained raw data is cnverted in 
 * object instance(s).
 */ 
class QpidClass extends QpidEntity implements QpidClassMBean 
{        		
    /**
     * State interface for this class definition.
     * Each state is responsible to handle the injection of the data and / or schema. 
     * 
     * @author Andrea Gazzarini
     */
    interface State 
    {    
        /**
         * Adds configuration data for the object instance associated to the given object identifier.
         * 
         * @param objectId the object identifier.
         * @param rawData the raw configuration data.
         */
        void addInstrumentationData (Binary objectId, byte[] rawData);

        /**
         * Adds instrumentation data for the object instance associated to the given object identifier.
         * 
         * @param objectId the object identifier.
         * @param rawData the raw instrumentation data.
         */
        void addConfigurationData (Binary objectId, byte[] rawData);       
        
        /**
         * Inject the schema into this class definition.
         * 
         * @param propertyDefinitions
         * @param statisticDefinitions
         * @param methodDefinitions
         * @throws UnableToBuildFeatureException when it's not possibile to parse schema and build the class definition.
         */
        public  void  setSchema (
                List<Map<String, Object>> propertyDefinitions, 
                List<Map<String, Object>> statisticDefinitions,
                List<MethodOrEventDataTransferObject> methodDefinitions) throws UnableToBuildFeatureException;
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
        public synchronized void addConfigurationData (Binary objectId, byte[] rawData)
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
                QManManagedObject instance = getObjectInstance(objectId,false);
                instance._rawConfigurationData.add(rawData);       
            }
        }

        /**
         * Stores the incoming data in raw format and request the schema for this class.
         * After that a transition to the next state is made.
         * 
         * @param objectId the object instance identifier.
         * @param rawData incoming instrumentation data.
         */
        public synchronized void addInstrumentationData (Binary objectId, byte[] rawData)
        {
            try
            {
                requestSchema();  
                _state = _schemaRequestedButNotYetInjected;
            } catch (Exception e)
            {
                _logger.error(
                        Messages.QMAN_100015_UNABLE_TO_SEND_SCHEMA_REQUEST, 
                        _parent.getName(),
                        _name);
            } finally {
                QManManagedObject instance = getObjectInstance(objectId,false);
                instance._rawConfigurationData.add(rawData);
            }
        }

        /**
         * This method only throws an illegal state exception because when a schema arrives 
         * this state is no longer valid.
         */
        public void setSchema (
                List<Map<String, Object>> propertyDefinitions,
                List<Map<String, Object>> statisticDefinitions, 
                List<MethodOrEventDataTransferObject> methodDefinitions) throws UnableToBuildFeatureException
        {
            throw new IllegalStateException("When a schema arrives it's not possible for this class to be in this state.");
        }
    };
    
    /**
     * This is the first state of this class definition : the schema is not yet injected so each injection of object data will be 
     * retained in raw format.
     */
    final State _schemaRequestedButNotYetInjected = new State()
    {
        /**
         * Stores the incoming data in raw format.
         * 
         * @param objectId the object instance identifier.
         * @param rawData incoming configuration data.
         */
        public void addConfigurationData (Binary objectId, byte[] rawData)
        {
            QManManagedObject instance = getObjectInstance(objectId,false);
            instance._rawConfigurationData.add(rawData);
        }

        /**
         * Stores the incoming data in raw format.
         * 
         * @param objectId the object instance identifier.
         * @param rawData incoming instrumentation data.
         */
        public void addInstrumentationData (Binary objectId, byte[] rawData)
        {
            QManManagedObject instance = getObjectInstance(objectId,false);
            instance._rawInstrumentationData.add(rawData);
        }

        /**
         * When a schema is injected into this defintiion the following should happen :
         * 1) the incoming schema is parsed and the class definition is built;
         * 2) the retained raw data is converted into object instance(s)
         * 3) the internal state of this class changes;
         * 
         * If someting is wrong during that process the schema is not built and the state don't change.
         */
        public synchronized void setSchema (
                List<Map<String, Object>> propertyDefinitions,
                List<Map<String, Object>> statisticDefinitions, 
                List<MethodOrEventDataTransferObject> methodDefinitions) throws UnableToBuildFeatureException
        {
                
                MBeanAttributeInfo [] attributesMetadata = new MBeanAttributeInfo[propertyDefinitions.size()+statisticDefinitions.size()];
                MBeanOperationInfo [] operationsMetadata = new MBeanOperationInfo[methodDefinitions.size()];
                
                buildAttributes(propertyDefinitions,statisticDefinitions,attributesMetadata);
                buildMethods(methodDefinitions,operationsMetadata);
                
                _metadata = new MBeanInfo(_name,_name,attributesMetadata,null,operationsMetadata,null);

                EntityLifecycleNotification notification = new EntityLifecycleNotification(
                  		 EntityLifecycleNotification.SCHEMA_INJECTED_NOTIFICATION_TYPE,
                  		 _parent.getName(), 
                  		 _name, 
                  		 Names.CLASS,
                  		 _objectName);
                   
                   sendNotification(notification);
                
                // Converting stored object instances into JMX MBean and removing raw instance data.
                for (Entry<Binary, QManManagedObject> instanceEntry : _objectInstances.entrySet())
                {
                    Binary objectId = instanceEntry.getKey();
                    QManManagedObject instance = instanceEntry.getValue();
                    
                    for (Iterator<byte[]> iterator = instance._rawInstrumentationData.iterator(); iterator.hasNext();)
                    {
                        updateInstanceWithInstrumentationData(instance,iterator.next());
                        iterator.remove();
                    } 

                    for (Iterator<byte[]> iterator = instance._rawConfigurationData.iterator(); iterator.hasNext();)
                    {
                        updateInstanceWithConfigurationData(instance, iterator.next());
                        iterator.remove();
                    }

                    registerMBean(instance,_parent.getOwnerId(),_parent.getName(),_name,objectId);
                }
            _state = _schemaInjected;     
            
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
        public void addConfigurationData (Binary objectId, byte[] rawData)
        {
            QManManagedObject instance = getObjectInstance(objectId,true);            
            updateInstanceWithConfigurationData(instance, rawData);
        }

        /**
         * Updates the instrumentation state of the object instance associates with the given object identifier.
         * 
         * @param objectId the object identifier.
         * @param rawData the instrumentation data (raw format).
         */
        public void addInstrumentationData (Binary objectId, byte[] rawData)
        {
            QManManagedObject instance = getObjectInstance(objectId,true);            
            updateInstanceWithInstrumentationData(instance, rawData);
        }

        /**
         * Never called when the class definition has this state.
         */
        public void setSchema (
                List<Map<String, Object>> propertyDefinitions,
                List<Map<String, Object>> statisticDefinitions, 
                List<MethodOrEventDataTransferObject> methodDefinitions) throws UnableToBuildFeatureException
        {
            throw new IllegalStateException("When a schema arrives it's not possible for this class to be in this state.");
        }
    };
    
    /**
     * MBean used for representing remote broker object instances.
     * This is the core component of the QMan domain model
     */
    class QManManagedObject extends QManManagedEntity implements MBeanRegistration
    {
        private Binary _objectId;
        
        // Arrays used for storing raw data before this mbean is registered to mbean server.
        List<byte[]> _rawInstrumentationData = new ArrayList<byte[]>();
        List<byte[]>  _rawConfigurationData = new ArrayList<byte[]>();
         
        /**
         * Builds a new managed object with the given object identifier.
         * 
         * @param objectId the object identifier.
         */
        QManManagedObject(Binary objectId)
        {
            this._objectId = objectId;
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
                throw new RuntimeOperationsException(new IllegalArgumentException("attribute name must not be null"));
            }
            
            if (_properties.containsKey(attributeName) || _statistics.containsKey(attributeName))
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
        	OperationHasBeenInvokedNotification notification = null;
        	try 
        	{
	            QpidMethod method = _methods.get(actionName);
	            if (method != null) 
	            {
	                try
	                {
	                    method.validate(params);
	                    InvocationResult result = invokeMethod(_objectId, method, params);
	                    notification = new OperationHasBeenInvokedNotification(actionName,params,signature,result);
	                    return result;
	                } catch (Exception ex)
	                {
	                	MBeanException exception = new MBeanException(ex);
	                    notification = new OperationHasBeenInvokedNotification(actionName,params,signature,exception);
	                    throw exception;
	                }
	            } else 
	            {
	            	ReflectionException exception = new ReflectionException(new NoSuchMethodException(actionName));
	                notification = new OperationHasBeenInvokedNotification(actionName,params,signature,exception);
	                throw exception;
	            } 
        	} finally 
        	{
        		sendNotification(notification);
        	}
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
            QpidProperty property = _properties.get(attribute.getName());
            try 
            {
                property.validate(attribute.getValue());
            } catch(ValidationException exception) 
            {
                throw new InvalidAttributeValueException(exception.getMessage());
            }
            throw new RuntimeException("Not yet implemented.");
        }

        /**
         * Sets the values of several attributes of this MBean.
         *
         * @param attributes a list of attributes: The identification of the attributes to be set and the values they are to be set to.
         * @return  The list of attributes that were set, with their new values.
         */
        public AttributeList setAttributes (AttributeList attributes)
        {
            throw new RuntimeException("Not yet implemented.");
        }

        /**
         * MBean server callback after deregistration.
         */
        public void postDeregister ()
        {
        }

        /**
         * After the object is registered the raw data is set to null.
         * This is done because we no longer need this data : it has already been 
         * injected into this object instance.
         * 
         * @param registrationDone a flag indicating if the instance has been registered to mbean server.
         */
        public void postRegister (Boolean registrationDone)
        {
            if (registrationDone) 
            {
                _rawConfigurationData = null;
                _rawInstrumentationData = null;
            }
        }

        /**
         * MBean server callback before deregistration.
         */
        public void preDeregister () throws Exception
        {
        }
        
        /**
         * MBean server callback before registration.
         */
        public ObjectName preRegister (MBeanServer server, ObjectName name) throws Exception
        {
            return name;
        }
    }
    
    Map<String, QpidProperty> _properties = new HashMap<String, QpidProperty>(); 
    Map<String, QpidStatistic> _statistics = new HashMap<String, QpidStatistic>();
    private Map<String, QpidMethod> _methods = new HashMap<String, QpidMethod>();
    
    private List<QpidProperty> _schemaOrderedProperties = new ArrayList<QpidProperty>();
    private List<QpidStatistic> _schemaOrderedStatistics= new ArrayList<QpidStatistic>();
    
    private int _howManyPresenceBitMasks;
    private BlockingQueue<InvocationResult> _exchangeChannelForMethodInvocations;
    private final IMethodInvocationListener _methodInvocationListener;
    
    Map<Binary, QManManagedObject> _objectInstances = new HashMap<Binary, QManManagedObject>();
    State _state = _schemaNotRequested;;
    
    private final static class Log 
    {
    	private final static Logger LOGGER = Logger.get(QpidClass.class);
        final static void logMethodInvocationResult(InvocationResult result)
        {
            if (LOGGER.isDebugEnabled())
            {
                LOGGER.debug(String.valueOf(result));
            }
        }
    }
    
    /**
     * Builds a new class with the given name and package as parent.
     * 
     * @param className the name of the class.
     * @param hash the class schema hash.
     * @param parentPackage the parent of this class.
     */
    QpidClass(String className, Binary hash, QpidPackage parentPackage)
    {
    	super(className,hash, parentPackage,Names.CLASS);
        this._methodInvocationListener = _parent.getMethodInvocationListener();
        this._exchangeChannelForMethodInvocations = new SynchronousQueue<InvocationResult>();
    }
    
    /**
     * Adds the configuration data for the object instance associated to the given object identifier.
     * 
     * @param objectId the object identifier.
     * @param rawData the raw configuration data.
     */
    void addInstrumentationData (Binary objectId, byte[] rawData)
    {
        _logger.debug(
        		Messages.QMAN_200014_INCOMING_INSTRUMENTATION_DATA,
                _parent.getOwnerId(),
                _parent.getName(),
                _name,
                objectId);        
        _state.addInstrumentationData(objectId, rawData);
    }
    
    /**
     * Adds the instrumentation data for the object instance associated to the given object identifier.
     * 
     * @param objectId the object identifier.
     * @param rawData the raw instrumentation data.
     */
    void addConfigurationData (Binary objectId, byte[] rawData)
    {
        _logger.debug(
        		Messages.QMAN_200015_INCOMING_CONFIGURATION_DATA,
                _parent.getOwnerId(),
                _parent.getName(),
                _name,
                objectId);        
        _state.addConfigurationData(objectId, rawData);
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
     void setSchema (
            List<Map<String, Object>> propertyDefinitions, 
            List<Map<String, Object>> statisticDefinitions,
            List<MethodOrEventDataTransferObject> methodDefinitions) throws UnableToBuildFeatureException
    {
         _logger.info(Messages.QMAN_000010_INCOMING_SCHEMA,_parent.getOwnerId(),_parent.getName(),_name);
        _state.setSchema(propertyDefinitions, statisticDefinitions, methodDefinitions);
    }    

    /**
     * Internal method used for building attributes definitions.
     * 
     * @param props the map contained in the properties schema.
     * @param stats the map contained in the statistics schema.
     * @param attributes the management metadata for attributes.
     * @throws UnableToBuildFeatureException  when it's not possibile to build one attribute definition.
     */
    void buildAttributes (
            List<Map<String, Object>> props,
            List<Map<String, Object>> stats,
            MBeanAttributeInfo[] attributes) throws UnableToBuildFeatureException
    {
        int index = 0;
        int howManyOptionalProperties = 0;
        
        for (Map<String, Object> propertyDefinition : props)
        {
            QpidFeatureBuilder builder = QpidFeatureBuilder.createPropertyBuilder(propertyDefinition);
            builder.build();
            
            QpidProperty property = (QpidProperty) builder.getQpidFeature();           
            
            howManyOptionalProperties += (property.isOptional()) ? 1 : 0;
            
            _properties.put(property.getName(),property);
            _schemaOrderedProperties.add(property);
            attributes[index++]=(MBeanAttributeInfo) builder.getManagementFeature();
            
            _logger.debug(
                    Messages.QMAN_200016_PROPERTY_DEFINITION_HAS_BEEN_BUILT,
                    _parent.getName(),
                    _name,
                    property);
        }
                
        _howManyPresenceBitMasks =  (int)Math.ceil((double)howManyOptionalProperties / 8);
        
        _logger.debug(
                Messages.QMAN_200018_OPTIONAL_PROPERTIES_INFO,
                _parent.getOwnerId(),
                _parent.getName(),
                _name,
                _howManyPresenceBitMasks);
        
        for (Map<String, Object> statisticDefinition : stats)
        {
            QpidFeatureBuilder builder = QpidFeatureBuilder.createStatisticBuilder(statisticDefinition);
            builder.build();
            QpidStatistic statistic = (QpidStatistic) builder.getQpidFeature();
            
            _statistics.put(statistic.getName(),statistic);
            _schemaOrderedStatistics.add(statistic);
            attributes[index++]=(MBeanAttributeInfo) builder.getManagementFeature();
            
            _logger.debug(
                    Messages.QMAN_200017_STATISTIC_DEFINITION_HAS_BEEN_BUILT,
                    _parent.getName(),
                    _name,
                    statistic);            
        }
    }    
    
    /**
     * Returns the object instance associated to the given identifier.
     * Note that if the identifier is not associated to any obejct instance, a new one will be created.
     * 
     * @param objectId the object identifier.
     * @param registration a flag indicating whenever the (new ) instance must be registered with MBean server.
     * @return the object instance associated to the given identifier.
     */
    QManManagedObject getObjectInstance(Binary objectId, boolean registration) 
    {
        QManManagedObject objectInstance = _objectInstances.get(objectId);
        if (objectInstance == null) 
        {
            objectInstance = new QManManagedObject(objectId);
            _objectInstances.put(objectId, objectInstance);
            if (registration)
            {
            	registerMBean(objectInstance,_parent.getOwnerId(),_parent.getName(),_name,objectId);
            }
        }
        return objectInstance;
    }
    
    /**
     * Internal method used for building method defintiions.
     * 
     * @param definitions the properties map contained in the incoming schema.
     * @param operationsMetadata 
     * @throws UnableToBuildFeatureException  when it's not possibile to build one or more definitions.
     */
    void buildMethods (List<MethodOrEventDataTransferObject> definitions, MBeanOperationInfo[] operationsMetadata) throws UnableToBuildFeatureException
    {
        int index = 0;
        for (MethodOrEventDataTransferObject definition: definitions)
        {
            QpidFeatureBuilder builder = QpidFeatureBuilder.createMethodBuilder(definition);
            builder.build();
            operationsMetadata [index++]= (MBeanOperationInfo) builder.getManagementFeature(); 
            QpidMethod method = (QpidMethod) builder.getQpidFeature();
            _methods.put(method.getName(),method);
        }
    }    
    
    /**
     * Header (opcode='M') 
     * ObjectId of target object (128 bits) 
     * Package name (str8) 
     * Class name (str8) 
     * Class hash (bin128) 
     * Method name (str8) [as defined in the schema] 
     * Now encode all input ("I") and i/o (IO) arguments in the order in which they are defined in the schema. 
     * (i.e. make one pass over the argument list and encode arguments that are either input or inptu/output). 

     * @param objectId
     * @param method
     * @param parameters
     * @throws Exception
     */
    private InvocationResult invokeMethod(Binary objectId,QpidMethod method,Object [] parameters) throws Exception
    {
        try
        {
            _service.connect();
            
            int sequenceNumber = SequenceNumberGenerator.getNextSequenceNumber();
            _methodInvocationListener.operationIsGoingToBeInvoked(new InvocationEvent(this,sequenceNumber,_exchangeChannelForMethodInvocations));
           _service.invoke(_parent.getName(), _name, _hash,objectId,parameters, method,sequenceNumber,objectId.getBankId(),objectId.getBrokerId());
             
            InvocationResult result = _exchangeChannelForMethodInvocations.poll(5000,TimeUnit.MILLISECONDS);
            
            if (result == null) 
            {
            	throw new TimeoutException();
            }
            
            Map<String, Object> output = method.decodeParameters(result.getOutputAndBidirectionalArgumentValues());
            result.setOutputSection(output);
            
            Log.logMethodInvocationResult(result);
            
            if (result.isException()) 
            {
                result.createAndThrowException();
            }
            return result;
        } finally
        {
            _service.close();
        }                
    }
    
    /**
     * Updates the given obejct instance with the given incoming configuration data.
     * 
     * @param instance the managed object instance.
     * @param rawData the incoming configuration data which contains new values for instance properties.
     */
    void updateInstanceWithConfigurationData(QManManagedObject instance,byte [] rawData)
    {
        BBDecoder decoder = new BBDecoder();
        decoder.init(ByteBuffer.wrap(rawData));

        byte [] presenceBitMasks = decoder.readBytes(_howManyPresenceBitMasks);
        for (QpidProperty property : _schemaOrderedProperties)
        {                  
            try {
                Object value = property.decodeValue(decoder,presenceBitMasks);
                instance.createOrReplaceAttributeValue(property.getName(),value);             
            } catch(Exception ignore) {
                _logger.error(Messages.QMAN_100016_UNABLE_TO_DECODE_FEATURE_VALUE, _parent.getName(),_name,property.getName());
            }
        }
    }
    
    /**
     * Updates the given object instance with the given incoming instrumentation data.
     * 
     * @param instance the managed object instance.
     * @param rawData the incoming instrumentation data which contains new values for instance properties.
     */
    void updateInstanceWithInstrumentationData(QManManagedObject instance,byte [] rawData)
    {
    	BBDecoder decoder = new BBDecoder();
        decoder.init(ByteBuffer.wrap(rawData));

        for (QpidStatistic statistic : _schemaOrderedStatistics)
        {                  
            try {
                Object value = statistic.decodeValue(decoder);
                instance.createOrReplaceAttributeValue(statistic.getName(),value);             
            } catch(Exception ignore) {
                _logger.error(Messages.QMAN_100016_UNABLE_TO_DECODE_FEATURE_VALUE, _parent.getName(),_name,statistic.getName());
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
            .append('.')
            .append(_name)
            .toString();
    }

    /**
     * Removes the object instance associated to the given identifier.
     * 
     * @param objectId the object identifier.
     */
    void removeObjectInstance (Binary objectId)
    {
        QManManagedObject toBeRemoved = _objectInstances.remove(objectId);
        if (toBeRemoved != null)
        {
            ObjectName objectName = JMX_SERVICE.unregisterObjectInstance(_parent.getOwnerId(),_parent.getName(),_name,toBeRemoved._objectId);
            
       	 EntityLifecycleNotification notification = new EntityLifecycleNotification(
    			 EntityLifecycleNotification.INSTANCE_REMOVED_NOTIFICATION_TYPE,
    			 _parent.getName(),
    			 _name,
    			 Names.CLASS,
    			 objectName);
    	 
    	 sendNotification(notification);
        }
    }

    /**
     * Deregisters all the object instances and release all previously acquired resources.
     */
    void releaseResources ()
    {
    	_objectInstances.clear();
    	JMX_SERVICE.unregisterObjectInstances();
    	JMX_SERVICE.unregisterClassDefinitions();    	
        _service.close();
    }
    
    /**
     * Compose method used for registering mbean instance.
     * 
     * @param instance the mbean instance.
     * @param brokerId the broker identifier.
     * @param packageName the package name.
     * @param className the class name.
     * @param objectId the object identifier.
     */
    private void registerMBean(
    		QManManagedObject instance,
            UUID brokerId,
            String packageName, 
            String className, 
            Binary objectId)
    {
    	 ObjectName objectName = JMX_SERVICE.registerObjectInstance(instance,_parent.getOwnerId(),_parent.getName(),_name,objectId);
    	 
    	 EntityLifecycleNotification notification = new EntityLifecycleNotification(
    			 EntityLifecycleNotification.INSTANCE_ADDED_NOTIFICATION_TYPE,
    			 packageName,
    			 className,
    			 Names.CLASS,
    			 objectName);
    	 
    	 sendNotification(notification);
    }
}
