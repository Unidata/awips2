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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.Map.Entry;

import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.domain.handler.base.IMessageHandler;
import org.apache.qpid.management.domain.model.AccessMode;
import org.apache.qpid.management.domain.model.type.AbsTime;
import org.apache.qpid.management.domain.model.type.DeltaTime;
import org.apache.qpid.management.domain.model.type.ObjectReference;
import org.apache.qpid.management.domain.model.type.Str16;
import org.apache.qpid.management.domain.model.type.Str8;
import org.apache.qpid.management.domain.model.type.Type;
import org.apache.qpid.management.domain.model.type.Uint16;
import org.apache.qpid.management.domain.model.type.Uint32;
import org.apache.qpid.management.domain.model.type.Uint64;
import org.apache.qpid.management.domain.model.type.Uint8;
import org.apache.qpid.transport.DeliveryProperties;
import org.apache.qpid.transport.Header;
import org.apache.qpid.transport.MessageProperties;
import org.apache.qpid.transport.ReplyTo;
import org.apache.qpid.transport.util.Logger;

/**
 * Qpid Management bridge configuration.
 * Basically iy is a singleton that is holding all the configurtion data loaded at startup.
 */
public final class Configuration
{    
    private final static Logger LOGGER = Logger.get(Configuration.class);
    private static Configuration INSTANCE = new Configuration();
    
    // Work Manager default settings
    private int _poolSize = 5;
    private int _maxPoolSize = 15;
    private long _keepAliveTime = 5000;
    
    Map<Integer, Type> _typeMappings = new HashMap<Integer,Type>();
    Map<Integer,AccessMode> _accessModes = new HashMap<Integer, AccessMode>();
    Map<Type,String> _validators = new HashMap<Type, String>();
    
    Map<UUID,BrokerConnectionData> _brokerConnectionInfos = new HashMap<UUID, BrokerConnectionData>();
    
    Map<Character, IMessageHandler> _managementQueueHandlers = new HashMap<Character, IMessageHandler>();
    Map<Character, IMessageHandler> _methodReplyQueueHandlers = new HashMap<Character, IMessageHandler>();
    
    private String _managementQueueName;
    private String _methodReplyQueueName;
    
    private Header _headerForCommandMessages;
    private DeliveryProperties _deliveryProperties = new DeliveryProperties();
    private MessageProperties _messageProperties = new MessageProperties();
        
    // Private constructor.
    private Configuration()
    {
        defineQueueNames();
        
        createHeaderForCommandMessages();
        
        addAccessModeMappings();
        
        addTypeMappings();
    }

    void clean()
    {
    	INSTANCE = new Configuration();
    }
    
    /**
     * Returns the singleton instance.
     * 
     * @return the singleton instance.
     */
    public static Configuration getInstance ()
    {
        return INSTANCE;
    }  
    
    /**
     * Returns true if this configuration has at least 
     * one broker configured.
     * 
     * @return true if this configuration has at least one 
     * 				broker configured.
     */
    public boolean hasOneOrMoreBrokersDefined()
    {
    	return !_brokerConnectionInfos.isEmpty();
    }
    
    /**
     * Returns the type associated to the given code.
     * 
     * @param code the code used as search criteria.
     * @return the type associated to the given code.
     * @throws UnknownTypeCodeException when the given code is not associated to any type.
     */
    public Type getType(int code) throws UnknownTypeCodeException 
    {
        Type result = _typeMappings.get(code);
        if (result == null) 
        {
            throw new UnknownTypeCodeException(code);
        }
        return result;
    }
    
    /**
     * Returns the access mode associated to the given code.
     * 
     * @param code the code used as search criteria.
     * @return the access mode associated to the given code.
     * @throws UnknownAccessCodeException when the given code is not associated to any access mode.
     */
    public AccessMode getAccessMode(int code) throws UnknownAccessCodeException
    {
        AccessMode result = _accessModes.get(code);
        if (result == null) 
        {
            throw new UnknownAccessCodeException(code);
        }
        return result;
    }

    /**
     * Returns the validator class name associated to the given type.
     * 
     * @param type the type. 
     * @return the validator class name associated to the given type.
     */
    public String getValidatorClassName (Type type)
    {
        return _validators.get(type);
    }
    
    /**
     * Gets from this configuration the list of known broker (I mean, only their connection data).
     * 
     * @return the list of known broker 
     */
    public Set<Entry<UUID, BrokerConnectionData>> getConnectionInfos(){
        return _brokerConnectionInfos.entrySet();
    }

    /**
     * Gets from this configuration the connection data of the broker associated with the given id.
     * 
     * @param brokerId the broker identifier.
     * @return the connection data of the broker associated with the given id.
     * @throws UnknownBrokerException when the given id is not associated with any broker.
     */
    public BrokerConnectionData getBrokerConnectionData (UUID brokerId) throws UnknownBrokerException
    {
        BrokerConnectionData connectionData = _brokerConnectionInfos.get(brokerId);
        if (connectionData == null)
        {
            throw new UnknownBrokerException(brokerId);
        }
        return _brokerConnectionInfos.get(brokerId);
    }
    
    /**
     * Returns the name of the management queue.
     *  
     * @return the name of the management queue.
     */
    public String getManagementQueueName() {
        return _managementQueueName;
    }
    
    /**
     * Returns the name of the method-reply queue.
     * 
     * @return the name of the method-reply queue.
     */
    public String getMethodReplyQueueName() {
        return _methodReplyQueueName;
    }
    
    /**
     * Returns a map containing all the configured management message handlers.
     * A management message handler it is a basically a processor for a management queue incoming message associated 
     * with a specific opcode.
     * 
     * @return a map containing all the configured management message handlers.
     */
    public Map<Character, IMessageHandler> getManagementQueueHandlers() 
    {
    	return _managementQueueHandlers;
    }

    /**
     * Returns a map containing all the configured method-reply message handlers.
     * A management message handler it is a basically a processor for a method-reply queue incoming message associated 
     * with a specific opcode.
     * 
     * @return a map containing all the configured method-reply  message handlers.
     */
    public Map<Character, IMessageHandler> getMethodReplyQueueHandlers() 
    {
    	return _methodReplyQueueHandlers;
    }

    /**
     * Returns the message header used for sending command message on management queue.
     * 
     * @return the message header used for sending command message on management queue.
     */
    public Header getCommandMessageHeader ()
    {
        return _headerForCommandMessages;
    }

    /**
     * Returns the command message properties.
     *  
     * @return the command message properties.
     */
    public MessageProperties getCommandMessageProperties ()
    {
        return _messageProperties;
    }

    /**
     * Returns the command message delivery properties.
     *  
     * @return the command message delivery properties.
     */
    public DeliveryProperties getCommandDeliveryProperties ()
    {
        return _deliveryProperties;
    }        
    
    /**
     * Adds a new type mapping to this configuration.
     * 
     * @param code the code that will be associated with the declared type.
     * @param type the type.
     * @param vailidatorClassName the FQN of the validator class that will be 
     * 				associated with the given type.
     */
    void addTypeMapping(int code, Type type, String validatorClassName) {
    	_typeMappings.put(code, type);
        _validators.put(type, validatorClassName);
        
        LOGGER.info(
        		Messages.QMAN_000005_TYPE_MAPPING_CONFIGURED, 
        		code,
        		type,
        		validatorClassName);
    }


    /**
     * Adds a new type mapping to this configuration.
     * 
     * @param code the code that will be associated with the declared type.
     * @param type the type.
     */
    void addTypeMapping(int code, Type type) {
        _typeMappings.put(code, type);
        
        LOGGER.info(
        		Messages.QMAN_000005_TYPE_MAPPING_CONFIGURED, 
        		code,
        		type,
        		"not configured for this type.");
    }

    /**
     * Adds a new access mode mapping to this configuration.
     * 
     * @param code the code that will be associated with the access mode,
     * @param accessMode the accessMode.
     */
    void addAccessModeMapping(int code, AccessMode accessMode){
        _accessModes.put(code, accessMode);
        
        LOGGER.info(Messages.QMAN_000006_ACCESS_MODE_MAPPING_CONFIGURED, code,accessMode);        
    }    
    
    /**
     * Adds a new management message handler to this configuration.
     * The incoming mapping object will contains an opcode and the class (as a string) of the message handler that will be used
     * for processing incoming messages with that opcode.
     * 
     * @param mapping the message handler mapping.
     */
    void addManagementMessageHandlerMapping (MessageHandlerMapping mapping)
    {
        Character opcode = mapping.getOpcode();
        IMessageHandler handler = mapping.getMessageHandler();
        _managementQueueHandlers.put(opcode, handler);
        
        LOGGER.info(Messages.QMAN_000007_MANAGEMENT_HANDLER_MAPPING_CONFIGURED, opcode,handler.getClass().getName()); 
    }

    /**
     * Adds a new method-reply message handler to this configuration.
     * The incoming mapping object will contains an opcode and the class (as a string) of the message handler that will be used
     * for processing incoming messages with that opcode.
     * 
     * @param mapping the message handler mapping.
     */
    void addMethodReplyMessageHandlerMapping (MessageHandlerMapping mapping)
    {
        Character opcode = mapping.getOpcode();
        IMessageHandler handler = mapping.getMessageHandler();
        _methodReplyQueueHandlers.put(opcode, handler);
        
        LOGGER.info(Messages.QMAN_000008_METHOD_REPLY_HANDLER_MAPPING_CONFIGURED, opcode,handler.getClass().getName());     
    }
    
    /**
     * Adds to this configuration a new broker connection data.
     * 
     * @param brokerId the broker identifier.
     * @param connectionData the connection data.
     * @throws BrokerAlreadyConnectedException when the broker is already connected.
     * @throws BrokerConnectionException when a connection cannot be estabilished.
     */
    void addBrokerConnectionData (UUID brokerId, BrokerConnectionData connectionData) throws BrokerAlreadyConnectedException, BrokerConnectionException 
    {
    	if (_brokerConnectionInfos.containsValue(connectionData))
    	{
    		throw new BrokerAlreadyConnectedException(connectionData);
    	}
    	
    	try 
    	{
        	QpidDatasource.getInstance().addConnectionPool(brokerId, connectionData);
            _brokerConnectionInfos.put(brokerId,connectionData);

            LOGGER.info(Messages.QMAN_000009_BROKER_DATA_CONFIGURED,brokerId,connectionData);        
    	} catch(Exception exception)
    	{
    		throw new BrokerConnectionException(exception);
    	}
      
    }
    
    /**
     * Header for command messages is created once because it only contains static values.
     */
    private void createHeaderForCommandMessages ()
    {
        ReplyTo replyTo=new ReplyTo();
        replyTo.setRoutingKey(_methodReplyQueueName);
        _messageProperties.setReplyTo(replyTo);
        _deliveryProperties.setRoutingKey(Names.AGENT_ROUTING_KEY);        
        _headerForCommandMessages = new Header(_deliveryProperties, _messageProperties);
    }
    
    /**
     * Creates the name of the queues used by this service. 
     * This is done because if a broker should be managed by one or more management client, then each of them
     * must have its own channels to communicate with.
     */
    private void defineQueueNames() 
    {
        UUID uuid = UUID.randomUUID();
        _managementQueueName = Names.MANAGEMENT_QUEUE_PREFIX+uuid;
        _methodReplyQueueName = Names.METHOD_REPLY_QUEUE_PREFIX+uuid;
        
        LOGGER.debug(Messages.QMAN_200004_MANAGEMENT_QUEUE_NAME,_managementQueueName);
        LOGGER.debug(Messages.QMAN_200005_METHOD_REPLY_QUEUE_NAME,_methodReplyQueueName);        
    }

    /**
     * Returns the worker manager thread pool size.
     * 
     * @return the worker manager thread pool size.
     */
	public int getWorkerManagerPoolSize()
	{
		return _poolSize;
	}

	/**
	 * Sets the size of the worker manager thread pool.
	 * 
	 * @param poolSize the size of the worker manager thread pool.
	 */
	void setWorkerManagerPoolSize(int poolSize)
	{
		this._poolSize = poolSize;
	}

	/**
	 * Returns the maximum size of the worker manager 
	 * thread pool size.
	 * 
	 * @return the max size of the worker manager thread pool.
	 */
	public int getWorkerManagerMaxPoolSize()
	{
		return _maxPoolSize;
	}

	/**
	 * Sets the maximum size of the worker manager 
	 * thread pool size.
	 * 
	 * @param maxPoolSize the max size of the worker manager thread pool.
	 */	
	void setWorkerManagerMaxPoolSize(int maxPoolSize)
	{
		this._maxPoolSize = maxPoolSize;
	}

	/**
	 * Returns the max amount of time that an excess thread
	 * can be idle before purging from the pool.
	 * 
	 * @return the max keep alive time.
	 */
	public long getWorkerManagerKeepAliveTime()
	{
		return _keepAliveTime;
	}

	/**
	 * Sets the max amount of time that an excess thread
	 * can be idle before purging from the pool.
	 * 
	 * @param keepAliveTime the max keep alive time.
	 */
	void setWorkerManagerKeepAliveTime(long keepAliveTime)
	{
		this._keepAliveTime = keepAliveTime;
	}
	
	/**
     * Configures access mode mappings.
     * An access mode mapping is an association between a code and an access mode.
     */
    private void addAccessModeMappings() {
    	addAccessModeMapping(1,AccessMode.RC);
    	addAccessModeMapping(2,AccessMode.RW);
    	addAccessModeMapping(3,AccessMode.RO);
	}	
    
	/**
     * Configures type mappings.
     * A type mapping is an association between a code and a management type.
     */
    private void addTypeMappings()
    {
    	addTypeMapping(1,new Uint8(),Names.NUMBER_VALIDATOR);
    	addTypeMapping(2,new Uint16(),Names.NUMBER_VALIDATOR);
    	addTypeMapping(3,new Uint32(),Names.NUMBER_VALIDATOR);
    	addTypeMapping(4,new Uint64(),Names.NUMBER_VALIDATOR);
    	addTypeMapping(6,new Str8(),Names.STRING_VALIDATOR);
    	addTypeMapping(7,new Str16(),Names.STRING_VALIDATOR);
    	addTypeMapping(8,new AbsTime());
    	addTypeMapping(9,new DeltaTime());
    	addTypeMapping(10,new ObjectReference());
    	addTypeMapping(11,new org.apache.qpid.management.domain.model.type.Boolean());
    	addTypeMapping(14,new org.apache.qpid.management.domain.model.type.Uuid());
    	addTypeMapping(15,new org.apache.qpid.management.domain.model.type.Map());
    }        
}