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
package org.apache.qpid.server.exchange;

import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import javax.management.JMException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularType;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.ArrayType;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.management.common.mbeans.ManagedExchange;
import org.apache.qpid.server.management.AMQManagedObject;
import org.apache.qpid.server.management.Managable;
import org.apache.qpid.server.management.ManagedObject;
import org.apache.qpid.server.management.ManagedObjectRegistry;
import org.apache.qpid.server.queue.QueueRegistry;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.messages.ExchangeMessages;
import org.apache.qpid.server.logging.subjects.ExchangeLogSubject;
import org.apache.qpid.server.logging.LogSubject;
import org.apache.log4j.Logger;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public abstract class AbstractExchange implements Exchange, Managable
{
    private AMQShortString _name;

    private Exchange _alternateExchange;

    protected boolean _durable;
    protected String _exchangeType;
    protected int _ticket;

    private VirtualHost _virtualHost;

    protected ExchangeMBean _exchangeMbean;

    /**
     * Whether the exchange is automatically deleted once all queues have detached from it
     */
    protected boolean _autoDelete;

    //The logSubject for ths exchange
    private LogSubject _logSubject;
    private Map<ExchangeReferrer,Object> _referrers = new ConcurrentHashMap<ExchangeReferrer,Object>();

    /**
     * Abstract MBean class. This has some of the methods implemented from
     * management intrerface for exchanges. Any implementaion of an
     * Exchange MBean should extend this class.
     */
    protected abstract class ExchangeMBean extends AMQManagedObject implements ManagedExchange
    {
        // open mbean data types for representing exchange bindings
        protected OpenType[] _bindingItemTypes;
        protected CompositeType _bindingDataType;
        protected TabularType _bindinglistDataType;
        protected TabularDataSupport _bindingList;

        public ExchangeMBean() throws NotCompliantMBeanException
        {
            super(ManagedExchange.class, ManagedExchange.TYPE, ManagedExchange.VERSION);
        }

        protected void init() throws OpenDataException
        {
            _bindingItemTypes = new OpenType[2];
            _bindingItemTypes[0] = SimpleType.STRING;
            _bindingItemTypes[1] = new ArrayType(1, SimpleType.STRING);
            _bindingDataType = new CompositeType("Exchange Binding", "Binding key and Queue names",
                    COMPOSITE_ITEM_NAMES, COMPOSITE_ITEM_DESCRIPTIONS, _bindingItemTypes);
            _bindinglistDataType = new TabularType("Exchange Bindings", "Exchange Bindings for " + getName(),
                                                   _bindingDataType, TABULAR_UNIQUE_INDEX);
        }

        public ManagedObject getParentObject()
        {
            return _virtualHost.getManagedObject();
        }

        public String getObjectInstanceName()
        {
            return _name.toString();
        }

        public String getName()
        {
            return _name.toString();
        }

        public String getExchangeType()
        {
            return _exchangeType;
        }

        public Integer getTicketNo()
        {
            return _ticket;
        }

        public boolean isDurable()
        {
            return _durable;
        }

        public boolean isAutoDelete()
        {
            return _autoDelete;
        }

        // Added exchangetype in the object name lets maangement apps to do any customization required
        public ObjectName getObjectName() throws MalformedObjectNameException
        {
            String objNameString = super.getObjectName().toString();
            objNameString = objNameString + ",ExchangeType=" + _exchangeType;
            return new ObjectName(objNameString);
        }

        protected ManagedObjectRegistry getManagedObjectRegistry()
        {
            return ApplicationRegistry.getInstance().getManagedObjectRegistry();
        }
    } // End of MBean class

    public AMQShortString getName()
    {
        return _name;
    }

    /**
     * Concrete exchanges must implement this method in order to create the managed representation. This is
     * called during initialisation (template method pattern).
     * @return the MBean
     */
    protected abstract ExchangeMBean createMBean() throws JMException;

    public void initialise(VirtualHost host, AMQShortString name, boolean durable, int ticket, boolean autoDelete)
            throws AMQException
    {
        _virtualHost = host;
        _name = name;
        _durable = durable;
        _autoDelete = autoDelete;
        _ticket = ticket;
        try
        {
            _exchangeMbean = createMBean();
            _exchangeMbean.register();
        }
        catch (JMException e)
        {
            getLogger().error(e);
        }
        _logSubject = new ExchangeLogSubject(this, this.getVirtualHost());

        // Log Exchange creation
        CurrentActor.get().message(ExchangeMessages.EXH_CREATED(String.valueOf(getType()), String.valueOf(name), durable));
    }

    public abstract Logger getLogger();

    public boolean isDurable()
    {
        return _durable;
    }

    public boolean isAutoDelete()
    {
        return _autoDelete;
    }

    public int getTicket()
    {
        return _ticket;
    }

    public void close() throws AMQException
    {
        if (_exchangeMbean != null)
        {
            _exchangeMbean.unregister();
        }
        if(_alternateExchange != null)
        {
            _alternateExchange.removeReference(this);
        }

        CurrentActor.get().message(_logSubject, ExchangeMessages.EXH_DELETED());
    }

    public String toString()
    {
        return getClass().getSimpleName() + "[" + getName() +"]";
    }

    public ManagedObject getManagedObject()
    {
        return _exchangeMbean;
    }

    public VirtualHost getVirtualHost()
    {
        return _virtualHost;
    }

    public QueueRegistry getQueueRegistry()
    {
        return getVirtualHost().getQueueRegistry();
    }

    public boolean isBound(String bindingKey, Map<String,Object> arguments, AMQQueue queue)
    {
        return isBound(new AMQShortString(bindingKey), queue);
    }


    public boolean isBound(String bindingKey, AMQQueue queue)
    {
        return isBound(new AMQShortString(bindingKey), queue);
    }

    public boolean isBound(String bindingKey)
    {
        return isBound(new AMQShortString(bindingKey));
    }

    public Exchange getAlternateExchange()
    {
        return _alternateExchange;
    }

    public void setAlternateExchange(Exchange exchange)
    {
        if(_alternateExchange != null)
        {
            _alternateExchange.removeReference(this);
        }
        if(exchange != null)
        {
            exchange.addReference(this);
        }
        _alternateExchange = exchange;

    }

    public void removeReference(ExchangeReferrer exchange)
    {
        _referrers.remove(exchange);
    }

    public void addReference(ExchangeReferrer exchange)
    {
        _referrers.put(exchange, Boolean.TRUE);
    }

    public boolean hasReferrers()
    {
        return !_referrers.isEmpty();
    }
}
