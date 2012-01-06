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
 * Parser used for building worker manager settings.
 * The corresponding section of the configuration file is :
 * 
	<work-manager>
		<pool-capacity>5</pool-capacity>
	  	<max-pool-capacity>15</max-pool-capacity>
	  	<keep-alive-time>5000</keep-alive-time>
  </work-manager>

 * 
 * @author Andrea Gazzarini
 */
class WorkerManagerConfigurationParser implements IParser
{
    private final static Logger LOGGER = Logger.get(Configuration.class);
    private String _currentValue;
    
    private String _poolSizeAsString;
    private String _maxPoolSizeAsString;
    private String _keepAliveTimeAsString;
    
    /**
     * Callback : the given value is the text content of the current node.
     */
    public void setCurrrentAttributeValue (String value)
    {
        this._currentValue = value;
    }

    /**
     * Callback: each time the end of an element is reached 
     * this method is called.
     */
    public void setCurrentAttributeName (String name)
    {
        switch (Tag.get(name))
        {
            case POOL_CAPACITY: 
            {
                _poolSizeAsString = _currentValue.trim();
                break;
            }
            case MAX_POOL_CAPACITY : 
            {
            	_maxPoolSizeAsString = _currentValue;
            }
            case KEEP_ALIVE_TIME: 
            {
            	_keepAliveTimeAsString = _currentValue;
                break;
            }
            case WORK_MANAGER: 
            {
            	Configuration configuration = Configuration.getInstance();
                try 
                {
                	configuration.setWorkerManagerPoolSize(Integer.parseInt(_poolSizeAsString));
                    configuration.setWorkerManagerMaxPoolSize(Integer.parseInt(_maxPoolSizeAsString));
                    configuration.setWorkerManagerKeepAliveTime(Long.parseLong(_keepAliveTimeAsString));                    
                } catch(Exception exception) 
                {
                    LOGGER.error(Messages.QMAN_100039_UNABLE_TO_CONFIGURE_PROPERLY_WORKER_MANAGER);
                } finally {
                    LOGGER.info(Messages.QMAN_000035_WORK_MANAGER_POOL_SIZE,configuration.getWorkerManagerPoolSize());
                    LOGGER.info(Messages.QMAN_000036_WORK_MANAGER_MAX_POOL_SIZE,configuration.getWorkerManagerMaxPoolSize());
                    LOGGER.info(Messages.QMAN_000037_WORK_MANAGER_KEEP_ALIVE_TIME,configuration.getWorkerManagerKeepAliveTime());                	
                }
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
