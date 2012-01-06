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
package org.apache.qpid.management.domain.services;

import java.util.UUID;

import org.apache.qpid.QpidException;
import org.apache.qpid.management.Messages;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.configuration.BrokerConnectionData;
import org.apache.qpid.management.configuration.Configuration;
import org.apache.qpid.management.domain.model.DomainModel;
import org.apache.qpid.transport.util.Logger;

/**
 * This is the Object representation of a management client.
 * According to specification : "A software component that is separate from the messaging broker, connected to the 
 * management broker via an AMQP connection, which allows any software component to be managed remotely by QPID."
 *
 * @author Andrea Gazzarini
 */
public final class ManagementClient
{      
    private final static Logger LOGGER = Logger.get(ManagementClient.class);
    
    private final String _managementQueueName;
    private final String _methodReplyQueueName;
    
    private DomainModel _domainModel;
    private QpidService _service;
   
    private final BrokerConnectionData _connectionData;
    
    /**
     * Builds a new <code>ManagementClient</code> with the given identifier and connection data.
     * 
     * @param brokerId the broker identifier.
     * @param connectionData the broker connection data (host, port, etc...)
     */
    ManagementClient(UUID brokerId,BrokerConnectionData connectionData)
    {
    	_connectionData = connectionData;
        _service = new QpidService(brokerId);
        _domainModel = new DomainModel(brokerId);
        _managementQueueName = Configuration.getInstance().getManagementQueueName();
        _methodReplyQueueName = Configuration.getInstance().getMethodReplyQueueName();
    }

    @Override
    public String toString()
    {
    	return _connectionData.toString();
    }
    
    /**
     * Returns the connection data associated with this management client.
     * 
     * @return the connection data associated with this management client.
     */
    public BrokerConnectionData getBrokerConnectionData()
    {
    	return _connectionData;
    }
    
    /**
     * Establishing initial communication Between Client and Broker.
     * According to specification :
     * "Communication is established between the management client and management agent using normal AMQP procedures. 
     * The client creates a connection to the broker and then establishes a session with its corresponding channel.
     * Two private queues are then declared. 
     * A management queue is declared and bound to the qpid.management exchange with "mgmt.#" as routing key; in that 
     * way all management-related messages sent to the exchange will be received by this client. 
     * When a client successfully binds to the qpid.management exchange, the management agent schedules a schema 
     * broadcast to be sent to the exchange. 
     * The agent will publish, via the exchange, a description of the schema for all manageable objects in its control. That 
     * schema is therefore received by this service and it will be part of service's domain model."
     * 
     * @throws StartupFailureException when this management client cannot perform startup operations due to an error.
     */
    void estabilishFirstConnectionWithBroker() throws StartupFailureException{
        try {
            connectWithBroker();
            
            createAndBindMethodReplyQueue();
            createAndBindManagementQueue();
                    
            registerConsumerOnManagementQueue();
            registerConsumerOnMethodReplyQueue();
            
            synchronize();
        } catch(Exception exception) 
        {
            try {
                _service.close();
            } catch(Exception ignore) 
            {                
            }            
            throw new StartupFailureException(exception);
        }
    }

    /**
     * Shutdown procedure for this management client.
     */
    void shutdown ()
    {        
        LOGGER.info(Messages.QMAN_000011_SHUTDOWN_INITIATED,_domainModel.getBrokerId());
        
        removeMethodReplyConsumer();
        destroyAndUnbingMethodReplyQueue();
        
        removeManagementConsumer();
        destroyAndUnbingManagementQueue();
        
        _domainModel.releaseResources();
        
        _service.close();

        LOGGER.info(Messages.QMAN_000012_MANAGEMENT_CLIENT_SHUT_DOWN,_domainModel.getBrokerId());
    }

    /**
     * Registers a consumer (that is, a listener) on the method-reply queue.
     */
    private void registerConsumerOnMethodReplyQueue ()
    {
        BrokerMessageListener methodReplyChannelListener = new BrokerMessageListener(_domainModel);
        methodReplyChannelListener.setHandlers(Configuration.getInstance().getMethodReplyQueueHandlers());
        _service.createSubscription(_methodReplyQueueName, _methodReplyQueueName, methodReplyChannelListener);
        
        LOGGER.info(Messages.QMAN_000013_METHOD_REPLY_CONSUMER_INSTALLED, _domainModel.getBrokerId());             
    }

    /**
     * Registers a consumer (listener) on the management queue.
     */
    private void registerConsumerOnManagementQueue () throws QpidException
    {  
        BrokerMessageListener managementChannelListener = new BrokerMessageListener(_domainModel);
        managementChannelListener.setHandlers(Configuration.getInstance().getManagementQueueHandlers());        
        _service.createSubscription(_managementQueueName, _managementQueueName, managementChannelListener);
        
        LOGGER.info(Messages.QMAN_000014_MANAGEMENT_CONSUMER_INSTALLED, _domainModel.getBrokerId());               
    }

    /**
     * Declares a management queue and bound it to the "qpid.management" exchange with "mgmt.#" as routing key; 
     */
    private void createAndBindManagementQueue ()
    {
        _service.declareQueue(_managementQueueName);
        _service.declareBinding(
                _managementQueueName, 
                Names.MANAGEMENT_EXCHANGE, 
                Names.MANAGEMENT_ROUTING_KEY);

        LOGGER.info(Messages.QMAN_000015_MANAGEMENT_QUEUE_DECLARED,_managementQueueName,_domainModel.getBrokerId());       
    }
    
    /**
     * Declares a private queue for receiving method replies (after method invocations). 
     * This queue is bound to the amq.direct exchange using a routing key equal to the name of the queue.
     */
    private void createAndBindMethodReplyQueue ()
    {
        _service.declareQueue(_methodReplyQueueName);
        _service.declareBinding(_methodReplyQueueName, Names.AMQ_DIRECT_QUEUE, _methodReplyQueueName);
        
        LOGGER.info(Messages.QMAN_000016_METHOD_REPLY_QUEUE_DECLARED,_methodReplyQueueName, _domainModel.getBrokerId());       
    }    
    
    /**
     * Removes the method-reply queue consumer. 
     */
    private void removeMethodReplyConsumer()
    {
        _service.removeSubscription(_methodReplyQueueName); 
        
        LOGGER.info(Messages.QMAN_000017_CONSUMER_HAS_BEEN_REMOVED,_methodReplyQueueName,_domainModel.getBrokerId());
    }
    
    /**
     * Unbind the method reply queue and after that destroy it from remote broker.
     */
    private void destroyAndUnbingMethodReplyQueue()
    {
        _service.declareUnbinding(_methodReplyQueueName, Names.AMQ_DIRECT_QUEUE, _methodReplyQueueName);
        _service.deleteQueue(_methodReplyQueueName);        
        
        LOGGER.info(Messages.QMAN_000018_QUEUE_UNDECLARED,_methodReplyQueueName,_domainModel.getBrokerId());
    }
    
    /**
     * Removes the management queue consumer. 
     */
    private void removeManagementConsumer()
    {
        _service.removeSubscription(_managementQueueName);  

        LOGGER.info(Messages.QMAN_000017_CONSUMER_HAS_BEEN_REMOVED,_managementQueueName,_domainModel.getBrokerId());
    }
    
    /**
     * Unbind the management queue and after that destroy it from remote broker.
     */
    private void destroyAndUnbingManagementQueue()
    {
        _service.declareUnbinding(_managementQueueName, Names.MANAGEMENT_EXCHANGE, Names.MANAGEMENT_ROUTING_KEY);
        _service.deleteQueue(_managementQueueName);        

        LOGGER.info(Messages.QMAN_000018_QUEUE_UNDECLARED, _managementQueueName,_domainModel.getBrokerId());
    }    
    
    /**
     * Connects this client with the broker.
     * 
     * @throws QpidException when it's not possibile to connect with the broker.
     */
    private void connectWithBroker() throws Exception 
    {
        _service.connect();
    }

    /**
     * All the Management client commands are asynchronous. 
     * Synchronous behavior is achieved through invoking the sync method.
     */
    private void synchronize() 
    {
        _service.sync();
    }
}
