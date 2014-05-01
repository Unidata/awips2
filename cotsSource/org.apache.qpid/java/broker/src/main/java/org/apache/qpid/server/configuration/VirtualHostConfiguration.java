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
package org.apache.qpid.server.configuration;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.store.MemoryMessageStore;

public class VirtualHostConfiguration
{
	private Configuration _config;
	private String _name;
	private Map<String, QueueConfiguration> _queues = new HashMap<String, QueueConfiguration>();
	private Map<String, ExchangeConfiguration> _exchanges = new HashMap<String, ExchangeConfiguration>();

	public VirtualHostConfiguration(String name, Configuration config) throws ConfigurationException
	{
		_config = config;
		_name = name;
		Iterator i = _config.getList("queues.queue.name").iterator();
		
		while (i.hasNext())
		{
			String queueName = (String) i.next();
			CompositeConfiguration mungedConf = new CompositeConfiguration();
			mungedConf.addConfiguration(_config.subset("queues.queue." + queueName));
			mungedConf.addConfiguration(_config.subset("queues"));
			_queues.put(queueName, new QueueConfiguration(queueName, mungedConf, this));
        }

		i = _config.getList("exchanges.exchange.name").iterator();
		int count = 0;
		while (i.hasNext())
        {
			CompositeConfiguration mungedConf = new CompositeConfiguration();
			mungedConf.addConfiguration(config.subset("exchanges.exchange(" + count++ + ")"));
			mungedConf.addConfiguration(_config.subset("exchanges"));
			String exchName = (String) i.next();
			_exchanges.put(exchName, new ExchangeConfiguration(exchName, mungedConf));
        }

    }

	public String getName()
	{
        return _name;
    }

	public long getHousekeepingExpiredMessageCheckPeriod()
	{
		return _config.getLong("housekeeping.expiredMessageCheckPeriod", ApplicationRegistry.getInstance().getConfiguration().getHousekeepingCheckPeriod());
	}

	public String getAuthenticationDatabase()
	{
		return _config.getString("security.authentication.name");
	}
	
	public List getCustomExchanges()
	{
		return _config.getList("custom-exchanges.class-name");
	}
	
	public SecurityConfiguration getSecurityConfiguration()
	{
		return new SecurityConfiguration(_config.subset("security"));
	}

	public Configuration getStoreConfiguration()
	{
		return _config.subset("store");
	}

	public String getMessageStoreClass()
	{
		return _config.getString("store.class", MemoryMessageStore.class.getName());
	}

	public List getExchanges()
	{
		return _config.getList("exchanges.exchange.name");
	}

	public String[] getQueueNames()
	{
		return _queues.keySet().toArray(new String[_queues.size()]);
	}

    public ExchangeConfiguration getExchangeConfiguration(String exchangeName)
    {
        return _exchanges.get(exchangeName);
    }

    public QueueConfiguration getQueueConfiguration(String queueName)
    {
        // We might be asked for the config for a queue we don't know about,
        // such as one that's been dynamically created. Those get the defaults by default.
        if (_queues.containsKey(queueName)) 
        {
            return _queues.get(queueName);
        } 
        else
        {
            return new QueueConfiguration(queueName, new PropertiesConfiguration(), this);
        }
    }

    public long getMemoryUsageMaximum()
    {
        return _config.getLong("queues.maximumMemoryUsage", 0);
    }

    public long getMemoryUsageMinimum()
    {
        return _config.getLong("queues.minimumMemoryUsage", 0);
    }
    
    public int getMaximumMessageAge()
    {
        return _config.getInt("queues.maximumMessageAge", 0);
    }

    public Long getMaximumQueueDepth()
    {
        return _config.getLong("queues.maximumQueueDepth", 0);
    }

    public Long getMaximumMessageSize()
    {
        return _config.getLong("queues.maximumMessageSize", 0);
    }

    public Long getMaximumMessageCount()
    {
        return _config.getLong("queues.maximumMessageCount", 0);
    }

    public Long getMinimumAlertRepeatGap()
    {
        return _config.getLong("queues.minimumAlertRepeatGap", 0);
    }


    public long getCapacity()
    {
        return _config.getLong("queues.capacity", 0l);
    }

    public long getFlowResumeCapacity()
    {
        return _config.getLong("queues.flowResumeCapacity", getCapacity());
    }

}
