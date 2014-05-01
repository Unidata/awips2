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

import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.management.common.mbeans.annotations.MBeanConstructor;
import org.apache.qpid.management.common.mbeans.annotations.MBeanDescription;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.AMQTypedValue;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.message.InboundMessage;
import org.apache.qpid.server.message.AMQMessageHeader;

import javax.management.JMException;
import javax.management.openmbean.ArrayType;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ConcurrentHashMap;

/**
 * An exchange that binds queues based on a set of required headers and header values
 * and routes messages to these queues by matching the headers of the message against
 * those with which the queues were bound.
 * <p/>
 * <pre>
 * The Headers Exchange
 *
 *  Routes messages according to the value/presence of fields in the message header table.
 *  (Basic and JMS content has a content header field called "headers" that is a table of
 *   message header fields).
 *
 *  class = "headers"
 *  routing key is not used
 *
 *  Has the following binding arguments:
 *
 *  the X-match field - if "all", does an AND match (used for GRM), if "any", does an OR match.
 *  other fields prefixed with "X-" are ignored (and generate a console warning message).
 *  a field with no value or empty value indicates a match on presence only.
 *  a field with a value indicates match on field presence and specific value.
 *
 *  Standard instances:
 *
 *  amq.match - pub/sub on field content/value
 *  </pre>
 */
public class HeadersExchange extends AbstractExchange
{

    private static final Logger _logger = Logger.getLogger(HeadersExchange.class);

    public static final ExchangeType<HeadersExchange> TYPE = new ExchangeType<HeadersExchange>()
    {

        public AMQShortString getName()
        {
            return ExchangeDefaults.HEADERS_EXCHANGE_CLASS;
        }

        public Class<HeadersExchange> getExchangeClass()
        {
            return HeadersExchange.class;
        }

        public HeadersExchange newInstance(VirtualHost host, AMQShortString name, boolean durable, int ticket,
                boolean autoDelete) throws AMQException
        {
            HeadersExchange exch = new HeadersExchange();

            exch.initialise(host, name, durable, ticket, autoDelete);
            return exch;
        }

        public AMQShortString getDefaultExchangeName()
        {

            return ExchangeDefaults.HEADERS_EXCHANGE_NAME;
        }
    };


    private final List<Registration> _bindings = new CopyOnWriteArrayList<Registration>();
    private Map<AMQShortString, Registration> _bindingByKey = new ConcurrentHashMap<AMQShortString, Registration>();

    /**
     * HeadersExchangeMBean class implements the management interface for the
     * Header Exchanges.
     */
    @MBeanDescription("Management Bean for Headers Exchange")
    private final class HeadersExchangeMBean extends ExchangeMBean
    {
        @MBeanConstructor("Creates an MBean for AMQ Headers exchange")
        public HeadersExchangeMBean() throws JMException
        {
            super();
            _exchangeType = "headers";
            init();
        }

        /**
         * initialises the OpenType objects.
         */
        protected void init() throws OpenDataException
        {

            _bindingItemTypes = new OpenType[3];
            _bindingItemTypes[0] = SimpleType.INTEGER;
            _bindingItemTypes[1] = SimpleType.STRING;
            _bindingItemTypes[2] = new ArrayType(1, SimpleType.STRING);
            _bindingDataType = new CompositeType("Exchange Binding", "Queue name and header bindings",
                    HEADERS_COMPOSITE_ITEM_NAMES, HEADERS_COMPOSITE_ITEM_DESC, _bindingItemTypes);
            _bindinglistDataType = new TabularType("Exchange Bindings", "List of exchange bindings for " + getName(),
                                                   _bindingDataType, HEADERS_TABULAR_UNIQUE_INDEX);
        }

        public TabularData bindings() throws OpenDataException
        {
            _bindingList = new TabularDataSupport(_bindinglistDataType);
            int count = 1;
            for (Iterator<Registration> itr = _bindings.iterator(); itr.hasNext();)
            {
                Registration registration = itr.next();
                String queueName = registration.queue.getName().toString();

                HeadersBinding headers = registration.binding;
                FieldTable headerMappings = headers.getMappings();
                final List<String> mappingList = new ArrayList<String>();

                headerMappings.processOverElements(new FieldTable.FieldTableElementProcessor()
                {

                    public boolean processElement(String propertyName, AMQTypedValue value)
                    {
                        mappingList.add(propertyName + "=" + value.getValue());
                        return true;
                    }

                    public Object getResult()
                    {
                        return mappingList;
                    }
                });


                Object[] bindingItemValues = {count++, queueName, mappingList.toArray(new String[0])};
                CompositeData bindingData = new CompositeDataSupport(_bindingDataType, HEADERS_COMPOSITE_ITEM_NAMES, bindingItemValues);
                _bindingList.put(bindingData);
            }

            return _bindingList;
        }

        /**
         * Creates bindings. Binding pattern is as follows-
         * <attributename>=<value>,<attributename>=<value>,...
         * @param queueName
         * @param binding
         * @throws javax.management.JMException
         */
        public void createNewBinding(String queueName, String binding) throws JMException
        {
            AMQQueue queue = getQueueRegistry().getQueue(new AMQShortString(queueName));

            if (queue == null)
            {
                throw new JMException("Queue \"" + queueName + "\" is not registered with the exchange.");
            }

            String[] bindings = binding.split(",");
            FieldTable bindingMap = new FieldTable();
            for (int i = 0; i < bindings.length; i++)
            {
                String[] keyAndValue = bindings[i].split("=");
                if (keyAndValue == null || keyAndValue.length == 0 || keyAndValue.length > 2)
                {
                    throw new JMException("Format for headers binding should be \"<attribute1>=<value1>,<attribute2>=<value2>\" ");
                }

                if(keyAndValue.length ==1)
                {
                    //no value was given, only a key. Use an empty value
                    //to signal match on key presence alone
                    bindingMap.setString(keyAndValue[0], "");
                }
                else
                {
                    bindingMap.setString(keyAndValue[0], keyAndValue[1]);
                }
            }

            _bindings.add(new Registration(new HeadersBinding(bindingMap), queue, new AMQShortString(binding)));
        }

    } // End of MBean class

    public AMQShortString getType()
    {
        return ExchangeDefaults.HEADERS_EXCHANGE_CLASS;
    }

    public void registerQueue(AMQShortString routingKey, AMQQueue queue, FieldTable args) throws AMQException
    {
        _logger.debug("Exchange " + getName() + ": Binding " + queue.getName() + " with " + args);

        Registration registration = new Registration(new HeadersBinding(args), queue, routingKey);
        _bindings.add(registration);

    }

    public void deregisterQueue(AMQShortString routingKey, AMQQueue queue, FieldTable args) throws AMQException
    {
        _logger.debug("Exchange " + getName() + ": Unbinding " + queue.getName());

        if(!_bindings.remove(new Registration(args == null ? null : new HeadersBinding(args), queue, routingKey)))
        {
            throw new AMQException(AMQConstant.NOT_FOUND, "Queue " + queue + " was not registered with exchange " + this.getName()
                                   + " with headers args " + args);
        }
    }

    public ArrayList<AMQQueue> route(InboundMessage payload)
    {
        AMQMessageHeader header = payload.getMessageHeader();
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Exchange " + getName() + ": routing message with headers " + header);
        }
        boolean routed = false;
        ArrayList<AMQQueue> queues = new ArrayList<AMQQueue>();
        for (Registration e : _bindings)
        {

            if (e.binding.matches(header))
            {
                if (_logger.isDebugEnabled())
                {
                    _logger.debug("Exchange " + getName() + ": delivering message with headers " +
                                  header + " to " + e.queue.getName());
                }
                queues.add(e.queue);

                routed = true;
            }
        }
        return queues;
    }

    public boolean isBound(AMQShortString routingKey, FieldTable arguments, AMQQueue queue)
    {
        //fixme isBound here should take the arguements in to consideration.
        return isBound(routingKey, queue);
    }

    public boolean isBound(AMQShortString routingKey, AMQQueue queue)
    {
        return isBound(queue);
    }

    public boolean isBound(AMQShortString routingKey)
    {
        return hasBindings();
    }

    public boolean isBound(AMQQueue queue)
    {
        for (Registration r : _bindings)
        {
            if (r.queue.equals(queue))
            {
                return true;
            }
        }
        return false;
    }

    public boolean hasBindings()
    {
        return !_bindings.isEmpty();
    }

    protected FieldTable getHeaders(ContentHeaderBody contentHeaderFrame)
    {
        //what if the content type is not 'basic'? 'file' and 'stream' content classes also define headers,
        //but these are not yet implemented.
        return ((BasicContentHeaderProperties) contentHeaderFrame.properties).getHeaders();
    }

    protected ExchangeMBean createMBean() throws JMException
    {
        return new HeadersExchangeMBean();
    }

    public Map<AMQShortString, List<AMQQueue>> getBindings()
    {
        return null;
    }

    public Logger getLogger()
    {
        return _logger;
    }


    private static class Registration
    {
        private final HeadersBinding binding;
        private final AMQQueue queue;
        private final AMQShortString routingKey;

        Registration(HeadersBinding binding, AMQQueue queue, AMQShortString routingKey)
        {
            this.binding = binding;
            this.queue = queue;
            this.routingKey = routingKey;
        }

        public int hashCode()
        {
            int queueHash = queue.hashCode();
            int routingHash = routingKey == null ? 0 : routingKey.hashCode();
            return queueHash + routingHash;
        }

        public boolean equals(Object o)
        {
            return o instanceof Registration
                   && ((Registration) o).queue.equals(queue)
                   && (routingKey == null ? ((Registration)o).routingKey == null
                                          : routingKey.equals(((Registration)o).routingKey));
        }
    }
}
