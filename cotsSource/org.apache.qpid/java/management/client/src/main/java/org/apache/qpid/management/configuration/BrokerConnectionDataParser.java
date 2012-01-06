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

import java.util.UUID;

import org.apache.qpid.management.Messages;
import org.apache.qpid.transport.util.Logger;

/**
 * Parser used for building broker connection data settings.
 * The corresponding section on the configuration file is :
 * 
        <broker>
            <host>192.168.148.131</host>
            <port>5672</port>
            <virtual-host>test</virtual-host>
            <user>guest</user>
            <password>guest</password>
            <max-pool-capacity>4</max-pool-capacity>
            <initial-pool-capacity>4</initial-pool-capacity>
            <max-wait-timeout>-1</max-wait-timeout>
        </broker>   
 * 
 * @author Andrea Gazzarini
 */
class BrokerConnectionDataParser implements IParser
{
    private final static Logger LOGGER = Logger.get(Configuration.class);
    private BrokerConnectionData  _connectionData = new BrokerConnectionData();
    private String _currentValue;
    
    /**
     * Callback : the given value is the text content of the current node.
     */
    public void setCurrrentAttributeValue (String value)
    {
        this._currentValue = value;
    }

    /**
     * Callback: each time the end of an element is reached this method is called.
     * It's here that the built mapping is injected into the configuration.
     *      <broker>
            <host>192.168.61.130</host>
            <port>5673</port>
            <virtual-host>test</virtual-host>
            <user>andrea</user>
            <password>andrea</password>
        </broker>
     */
    public void setCurrentAttributeName (String name)
    {
        switch (Tag.get(name))
        {
            case HOST: 
            {
                _connectionData.setHost(_currentValue);
                break;
            }
            case PORT : 
            {
                _connectionData.setPort(_currentValue);
                break;
            }
            case VIRTUAL_HOST: 
            {
                _connectionData.setVirtualHost(_currentValue);
                break;
            }
            case USER : 
            {
                _connectionData.setUsername(_currentValue);
                break;
            }
            case MAX_POOL_CAPACITY: 
            {
                _connectionData.setMaxPoolCapacity (_currentValue);
                break;
            }            
            case INITIAL_POOL_CAPACITY: 
            {
                _connectionData.setInitialPoolCapacity(_currentValue);
                break;
            }            
            case MAX_WAIT_TIMEOUT: 
            {
                _connectionData.setMaxWaitTimeout(_currentValue);
                break;
            }            
            case PASSWORD: 
            {
                _connectionData.setPassword(_currentValue);
                break;
            }
            case BROKER: 
            {
                try 
                {
                    Configuration.getInstance().addBrokerConnectionData(getUUId(),_connectionData);
                } catch(Exception exception) 
                {
                    LOGGER.error(exception, Messages.QMAN_100007_UNABLE_TO_CONNECT_WITH_BROKER, _connectionData);
                }
                _connectionData = new BrokerConnectionData();
                break;
            }
        }
    }
    
    /**
     * Gets an uuid in order to associate current connection data with a broker.
     * @return
     */
    UUID getUUId(){
      return UUID.randomUUID();  
    }
}
