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
/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.apache.qpid.server;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanException;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.management.common.mbeans.ManagedBroker;
import org.apache.qpid.management.common.mbeans.ManagedQueue;
import org.apache.qpid.management.common.mbeans.annotations.MBeanConstructor;
import org.apache.qpid.management.common.mbeans.annotations.MBeanDescription;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.exchange.ExchangeFactory;
import org.apache.qpid.server.exchange.ExchangeRegistry;
import org.apache.qpid.server.exchange.ExchangeType;
import org.apache.qpid.server.management.AMQManagedObject;
import org.apache.qpid.server.management.ManagedObject;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.AMQQueueFactory;
import org.apache.qpid.server.queue.AMQQueueMBean;
import org.apache.qpid.server.queue.QueueRegistry;
import org.apache.qpid.server.store.DurableConfigurationStore;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.virtualhost.VirtualHostImpl;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.actors.ManagementActor;

/**
 * This MBean implements the broker management interface and exposes the
 * Broker level management features like creating and deleting exchanges and queue.
 */
@MBeanDescription("This MBean exposes the broker level management features")
public class AMQBrokerManagerMBean extends AMQManagedObject implements ManagedBroker
{
    private final QueueRegistry _queueRegistry;
    private final ExchangeRegistry _exchangeRegistry;
    private final ExchangeFactory _exchangeFactory;
    private final DurableConfigurationStore _durableConfig;

    private final VirtualHostImpl.VirtualHostMBean _virtualHostMBean;

    @MBeanConstructor("Creates the Broker Manager MBean")
    public AMQBrokerManagerMBean(VirtualHostImpl.VirtualHostMBean virtualHostMBean) throws JMException
    {
        super(ManagedBroker.class, ManagedBroker.TYPE, ManagedBroker.VERSION);

        _virtualHostMBean = virtualHostMBean;
        VirtualHost virtualHost = virtualHostMBean.getVirtualHost();

        _queueRegistry = virtualHost.getQueueRegistry();
        _exchangeRegistry = virtualHost.getExchangeRegistry();
        _durableConfig = virtualHost.getDurableConfigurationStore();
        _exchangeFactory = virtualHost.getExchangeFactory();
    }

    public String getObjectInstanceName()
    {
        return _virtualHostMBean.getVirtualHost().getName();
    }

    /**
     * Returns an array of the exchange types available for creation.
     * @since Qpid JMX API 1.3
     * @throws IOException
     */
    public String[] getExchangeTypes() throws IOException
    {
        ArrayList<String> exchangeTypes = new ArrayList<String>();
        for(ExchangeType<? extends Exchange> ex : _exchangeFactory.getRegisteredTypes())
        {
            exchangeTypes.add(ex.getName().toString());
        }

        return exchangeTypes.toArray(new String[0]);
    }

    /**
     * Returns a list containing the names of the attributes available for the Queue mbeans.
     * @since Qpid JMX API 1.3
     * @throws IOException
     */
    public List<String> retrieveQueueAttributeNames() throws IOException
    {
        List<String> attributeList = new ArrayList<String>();
        for(String attr : ManagedQueue.QUEUE_ATTRIBUTES)
        {
            attributeList.add(attr);
        }

        Collections.sort(attributeList);

        return attributeList;
    }

    /**
     * Returns a List of Object Lists containing the requested attribute values (in the same sequence requested) for each queue in the virtualhost.
     * If a particular attribute cant be found or raises an mbean/reflection exception whilst being gathered its value is substituted with the String "-".
     * @since Qpid JMX API 1.3
     * @throws IOException
     */
    public List<List<Object>> retrieveQueueAttributeValues(String[] attributes) throws IOException
    {
        if(_queueRegistry.getQueues().size() == 0)
        {
            return new ArrayList<List<Object>>();
        }

        List<List<Object>> queueAttributesList = new ArrayList<List<Object>>(_queueRegistry.getQueues().size());

        int attributesLength = attributes.length;

        for(AMQQueue queue : _queueRegistry.getQueues())
        {
            AMQQueueMBean mbean = (AMQQueueMBean) queue.getManagedObject();

            if(mbean == null)
            {
                continue;
            }

            List<Object> attributeValues = new ArrayList<Object>(attributesLength);

            for(int i=0; i < attributesLength; i++)
            {
                try
                {
                    attributeValues.add(mbean.getAttribute(attributes[i]));
                }
                catch (Exception e)
                {
                    attributeValues.add(new String("-"));
                }
            }

            queueAttributesList.add(attributeValues);
        }

        return queueAttributesList;
    }

    /**
     * Creates new exchange and registers it with the registry.
     *
     * @param exchangeName
     * @param type
     * @param durable
     * @throws JMException
     */
    public void createNewExchange(String exchangeName, String type, boolean durable) throws JMException
    {
        CurrentActor.set(new ManagementActor(_logActor.getRootMessageLogger()));
        try
        {
            synchronized (_exchangeRegistry)
            {
                Exchange exchange = _exchangeRegistry.getExchange(new AMQShortString(exchangeName));
                if (exchange == null)
                {
                    exchange = _exchangeFactory.createExchange(new AMQShortString(exchangeName), new AMQShortString(type),
                                                               durable, false, 0);
                    _exchangeRegistry.registerExchange(exchange);
                    if (durable)
                    {
                        _durableConfig.createExchange(exchange);
                    }
                }
                else
                {
                    throw new JMException("The exchange \"" + exchangeName + "\" already exists.");
                }
            }
        }
        catch (AMQException ex)
        {
            throw new MBeanException(ex, "Error in creating exchange " + exchangeName);
        }
        finally
        {
            CurrentActor.remove();
        }
    }

    /**
     * Unregisters the exchange from registry.
     *
     * @param exchangeName
     * @throws JMException
     */
    public void unregisterExchange(String exchangeName) throws JMException
    {
        // TODO
        // Check if the exchange is in use.
        // boolean inUse = false;
        // Check if there are queue-bindings with the exchange and unregister
        // when there are no bindings.
        CurrentActor.set(new ManagementActor(_logActor.getRootMessageLogger()));
        try
        {
            _exchangeRegistry.unregisterExchange(new AMQShortString(exchangeName), false);
        }
        catch (AMQException ex)
        {
            throw new MBeanException(ex, "Error in unregistering exchange " + exchangeName);
        }
        finally
        {
            CurrentActor.remove();
        }
    }

    /**
     * Creates a new queue and registers it with the registry and puts it
     * in persistance storage if durable queue.
     *
     * @param queueName
     * @param durable
     * @param owner
     * @throws JMException
     */
    public void createNewQueue(String queueName, String owner, boolean durable) throws JMException
    {
        AMQQueue queue = _queueRegistry.getQueue(new AMQShortString(queueName));
        if (queue != null)
        {
            throw new JMException("The queue \"" + queueName + "\" already exists.");
        }

        CurrentActor.set(new ManagementActor(_logActor.getRootMessageLogger()));
        try
        {
            AMQShortString ownerShortString = null;
            if (owner != null)
            {
                ownerShortString = new AMQShortString(owner);
            }

            queue = AMQQueueFactory.createAMQQueueImpl(new AMQShortString(queueName), durable, ownerShortString, false, getVirtualHost(),
                                                       null);
            if (queue.isDurable() && !queue.isAutoDelete())
            {
                _durableConfig.createQueue(queue);
            }

            _queueRegistry.registerQueue(queue);
        }
        catch (AMQException ex)
        {
            JMException jme = new JMException(ex.getMessage());
            jme.initCause(ex);
            throw new MBeanException(jme, "Error in creating queue " + queueName);
        }
        finally
        {
            CurrentActor.remove();
        }
    }

    private VirtualHost getVirtualHost()
    {
        return _virtualHostMBean.getVirtualHost();
    }

    /**
     * Deletes the queue from queue registry and persistant storage.
     *
     * @param queueName
     * @throws JMException
     */
    public void deleteQueue(String queueName) throws JMException
    {
        AMQQueue queue = _queueRegistry.getQueue(new AMQShortString(queueName));
        if (queue == null)
        {
            throw new JMException("The Queue " + queueName + " is not a registerd queue.");
        }

        CurrentActor.set(new ManagementActor(_logActor.getRootMessageLogger()));
        try
        {
            queue.delete();
            if (queue.isDurable())
            {
                _durableConfig.removeQueue(queue);
            }
        }
        catch (AMQException ex)
        {
            JMException jme = new JMException(ex.getMessage());
            jme.initCause(ex);
            throw new MBeanException(jme, "Error in deleting queue " + queueName);
        }
        finally
        {
            CurrentActor.remove();
        }
    }

    public ManagedObject getParentObject()
    {
        return _virtualHostMBean;
    }

    // This will have a single instance for a virtual host, so not having the name property in the ObjectName
    public ObjectName getObjectName() throws MalformedObjectNameException
    {
        return getObjectNameForSingleInstanceMBean();
    }
} // End of MBean class
