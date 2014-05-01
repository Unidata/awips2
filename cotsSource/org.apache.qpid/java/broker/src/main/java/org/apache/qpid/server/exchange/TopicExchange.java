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
import org.apache.qpid.common.AMQPFilterTypes;
import org.apache.qpid.management.common.mbeans.annotations.MBeanConstructor;
import org.apache.qpid.management.common.mbeans.annotations.MBeanDescription;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.AMQShortStringTokenizer;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.exchange.topic.TopicParser;
import org.apache.qpid.server.exchange.topic.TopicMatcherResult;
import org.apache.qpid.server.filter.MessageFilter;
import org.apache.qpid.server.filter.JMSSelectorFilter;
import org.apache.qpid.server.message.InboundMessage;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.actors.ManagementActor;

import javax.management.JMException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.lang.ref.WeakReference;

public class TopicExchange extends AbstractExchange
{

    public static final ExchangeType<TopicExchange> TYPE = new ExchangeType<TopicExchange>()
    {

        public AMQShortString getName()
        {
            return ExchangeDefaults.TOPIC_EXCHANGE_CLASS;
        }

        public Class<TopicExchange> getExchangeClass()
        {
            return TopicExchange.class;
        }

        public TopicExchange newInstance(VirtualHost host,
                                            AMQShortString name,
                                            boolean durable,
                                            int ticket,
                                            boolean autoDelete) throws AMQException
        {
            TopicExchange exch = new TopicExchange();
            exch.initialise(host, name, durable, ticket, autoDelete);
            return exch;
        }

        public AMQShortString getDefaultExchangeName()
        {
            return ExchangeDefaults.TOPIC_EXCHANGE_NAME;
        }
    };


    private static final Logger _logger = Logger.getLogger(TopicExchange.class);

/*
    private final ConcurrentHashMap<AMQShortString, List<AMQQueue>> _bindingKey2queues =
            new ConcurrentHashMap<AMQShortString, List<AMQQueue>>();
    private final ConcurrentHashMap<AMQShortString, List<AMQQueue>> _simpleBindingKey2queues =
            new ConcurrentHashMap<AMQShortString, List<AMQQueue>>();
    private final ConcurrentHashMap<AMQShortString, List<AMQQueue>> _wildCardBindingKey2queues =
            new ConcurrentHashMap<AMQShortString, List<AMQQueue>>();
*/
    // private ConcurrentHashMap<AMQShortString, AMQQueue> _routingKey2queue = new ConcurrentHashMap<AMQShortString, AMQQueue>();
    private static final byte TOPIC_SEPARATOR = (byte)'.';
    private static final AMQShortString TOPIC_SEPARATOR_AS_SHORTSTRING = new AMQShortString(".");
    private static final AMQShortString AMQP_STAR_TOKEN = new AMQShortString("*");
    private static final AMQShortString AMQP_HASH_TOKEN = new AMQShortString("#");

    private static final byte HASH_BYTE = (byte)'#';
    private static final byte STAR_BYTE = (byte)'*';

    private final TopicParser _parser = new TopicParser();

    private final Map<AMQShortString, TopicExchangeResult> _topicExchangeResults =
            new ConcurrentHashMap<AMQShortString, TopicExchangeResult>();

    private final Map<Binding, FieldTable> _bindings = new HashMap<Binding, FieldTable>();

    private final Map<String, WeakReference<JMSSelectorFilter>> _selectorCache = new WeakHashMap<String, WeakReference<JMSSelectorFilter>>();

    public static class Binding
    {
        private final AMQShortString _bindingKey;
        private final AMQQueue _queue;
        private final FieldTable _args;

        public Binding(AMQShortString bindingKey, AMQQueue queue, FieldTable args)
        {
            _bindingKey = bindingKey;
            _queue = queue;
            _args = args;
        }

        public AMQShortString getBindingKey()
        {
            return _bindingKey;
        }

        public AMQQueue getQueue()
        {
            return _queue;
        }

        public int hashCode()
        {
            return (_bindingKey == null ? 1 : _bindingKey.hashCode())*31 +_queue.hashCode();
        }

        public boolean equals(Object o)
        {
            if(this == o)
            {
                return true;
            }
            if(o instanceof Binding)
            {
                Binding other = (Binding) o;
                return (_queue == other._queue)
                        && ((_bindingKey == null) ? other._bindingKey == null : _bindingKey.equals(other._bindingKey));
            }
            return false;
        }
    }



    private final class TopicExchangeResult implements TopicMatcherResult
    {
        private final Map<AMQQueue, Integer> _unfilteredQueues = new ConcurrentHashMap<AMQQueue, Integer>();
        private final ConcurrentHashMap<AMQQueue, Map<MessageFilter,Integer>> _filteredQueues = new ConcurrentHashMap<AMQQueue, Map<MessageFilter, Integer>>();

        public void addUnfilteredQueue(AMQQueue queue)
        {
            Integer instances = _unfilteredQueues.get(queue);
            if(instances == null)
            {
                _unfilteredQueues.put(queue, 1);
            }
            else
            {
                _unfilteredQueues.put(queue, instances + 1);
            }
        }

        public void removeUnfilteredQueue(AMQQueue queue)
        {
            Integer instances = _unfilteredQueues.get(queue);
            if(instances == 1)
            {
                _unfilteredQueues.remove(queue);
            }
            else
            {
                _unfilteredQueues.put(queue,instances - 1);
            }

        }


        public void addFilteredQueue(AMQQueue queue, MessageFilter filter)
        {
            Map<MessageFilter,Integer> filters = _filteredQueues.get(queue);
            if(filters == null)
            {
                filters = new ConcurrentHashMap<MessageFilter,Integer>();
                _filteredQueues.put(queue, filters);
            }
            Integer instances = filters.get(filter);
            if(instances == null)
            {
                filters.put(filter,1);
            }
            else
            {
                filters.put(filter, instances + 1);
            }

        }

        public void removeFilteredQueue(AMQQueue queue, MessageFilter filter)
        {
            Map<MessageFilter,Integer> filters = _filteredQueues.get(queue);
            if(filters != null)
            {
                Integer instances = filters.get(filter);
                if(instances != null)
                {
                    if(instances == 1)
                    {
                        filters.remove(filter);
                        if(filters.isEmpty())
                        {
                            _filteredQueues.remove(queue);
                        }
                    }
                    else
                    {
                        filters.put(filter, instances - 1);
                    }
                }

            }

        }

        public void replaceQueueFilter(AMQQueue queue,
                                       MessageFilter oldFilter,
                                       MessageFilter newFilter)
        {
            Map<MessageFilter,Integer> filters = _filteredQueues.get(queue);
            Map<MessageFilter,Integer> newFilters = new ConcurrentHashMap<MessageFilter,Integer>(filters);
            Integer oldFilterInstances = filters.get(oldFilter);
            if(oldFilterInstances == 1)
            {
                newFilters.remove(oldFilter);
            }
            else
            {
                newFilters.put(oldFilter, oldFilterInstances-1);
            }
            Integer newFilterInstances = filters.get(newFilter);
            if(newFilterInstances == null)
            {
                newFilters.put(newFilter, 1);
            }
            else
            {
                newFilters.put(newFilter, newFilterInstances+1);
            }
            _filteredQueues.put(queue,newFilters);
        }

        public Collection<AMQQueue> processMessage(InboundMessage msg, Collection<AMQQueue> queues)
        {
            if(queues == null)
            {
                if(_filteredQueues.isEmpty())
                {
                    return new ArrayList<AMQQueue>(_unfilteredQueues.keySet());
                }
                else
                {
                    queues = new HashSet<AMQQueue>();
                }
            }
            else if(!(queues instanceof Set))
            {
                queues = new HashSet<AMQQueue>(queues);
            }

            queues.addAll(_unfilteredQueues.keySet());
            if(!_filteredQueues.isEmpty())
            {
                for(Map.Entry<AMQQueue, Map<MessageFilter, Integer>> entry : _filteredQueues.entrySet())
                {
                    if(!queues.contains(entry.getKey()))
                    {
                        for(MessageFilter filter : entry.getValue().keySet())
                        {
                            if(filter.matches(msg))
                            {
                                queues.add(entry.getKey());
                            }
                        }
                    }
                }
            }
            return queues;
        }

    }


    /** TopicExchangeMBean class implements the management interface for the Topic exchanges. */
    @MBeanDescription("Management Bean for Topic Exchange")
    private final class TopicExchangeMBean extends ExchangeMBean
    {
        @MBeanConstructor("Creates an MBean for AMQ topic exchange")
        public TopicExchangeMBean() throws JMException
        {
            super();
            _exchangeType = "topic";
            init();
        }

        /** returns exchange bindings in tabular form */
        public TabularData bindings() throws OpenDataException
        {
            _bindingList = new TabularDataSupport(_bindinglistDataType);
            Map<String, List<String>> bindingData = new HashMap<String, List<String>>();
            for (Binding binding : _bindings.keySet())
            {
                String key = binding.getBindingKey().toString();
                List<String> queueNames = bindingData.get(key);
                if(queueNames == null)
                {
                    queueNames = new ArrayList<String>();
                    bindingData.put(key, queueNames);
                }
                queueNames.add(binding.getQueue().getName().toString());

            }
            for(Map.Entry<String, List<String>> entry : bindingData.entrySet())
            {
                Object[] bindingItemValues = {entry.getKey(), entry.getValue().toArray(new String[entry.getValue().size()]) };
                CompositeData bindingCompositeData = new CompositeDataSupport(_bindingDataType, COMPOSITE_ITEM_NAMES, bindingItemValues);
                _bindingList.put(bindingCompositeData);
            }

            return _bindingList;
        }

        public void createNewBinding(String queueName, String binding) throws JMException
        {
            AMQQueue queue = getQueueRegistry().getQueue(new AMQShortString(queueName));
            if (queue == null)
            {
                throw new JMException("Queue \"" + queueName + "\" is not registered with the exchange.");
            }

            CurrentActor.set(new ManagementActor(_logActor.getRootMessageLogger()));
            try
            {
                queue.bind(TopicExchange.this, new AMQShortString(binding), null);
            }
            catch (AMQException ex)
            {
                throw new MBeanException(ex);
            }
            finally
            {
                CurrentActor.remove();
            }
        }

    } // End of MBean class

    public AMQShortString getType()
    {
        return ExchangeDefaults.TOPIC_EXCHANGE_CLASS;
    }

    public synchronized void registerQueue(AMQShortString rKey, AMQQueue queue, FieldTable args) throws AMQException
    {
        assert queue != null;
        assert rKey != null;

        _logger.debug("Registering queue " + queue.getName() + " with routing key " + rKey);


        AMQShortString routingKey;

        if(rKey.contains(HASH_BYTE) || rKey.contains(STAR_BYTE))
        {
            routingKey = normalize(rKey);
        }
        else
        {
            routingKey = rKey;
        }

        Binding binding = new Binding(rKey, queue, args);

        if(_bindings.containsKey(binding))
        {
            FieldTable oldArgs = _bindings.get(binding);
            TopicExchangeResult result = _topicExchangeResults.get(routingKey);

            if(argumentsContainSelector(args))
            {
                if(argumentsContainSelector(oldArgs))
                {
                    result.replaceQueueFilter(queue,createSelectorFilter(oldArgs), createSelectorFilter(args));
                }
                else
                {
                    result.addFilteredQueue(queue,createSelectorFilter(args));
                    result.removeUnfilteredQueue(queue);
                }
            }
            else
            {
                if(argumentsContainSelector(oldArgs))
                {
                    result.addUnfilteredQueue(queue);
                    result.removeFilteredQueue(queue, createSelectorFilter(oldArgs));
                }
                else
                {
                    // TODO - fix control flow
                    return;
                }
            }

        }
        else
        {

            TopicExchangeResult result = _topicExchangeResults.get(routingKey);
            if(result == null)
            {
                result = new TopicExchangeResult();
                if(argumentsContainSelector(args))
                {
                    result.addFilteredQueue(queue, createSelectorFilter(args));
                }
                else
                {
                    result.addUnfilteredQueue(queue);
                }
                _parser.addBinding(routingKey, result);
                _topicExchangeResults.put(routingKey,result);
            }
            else
            {
                if(argumentsContainSelector(args))
                {
                    result.addFilteredQueue(queue, createSelectorFilter(args));
                }
                else
                {
                    result.addUnfilteredQueue(queue);
                }
            }
            _bindings.put(binding, args);
        }


    }

    private JMSSelectorFilter createSelectorFilter(final FieldTable args)
            throws AMQException
    {

        final String selectorString = args.getString(AMQPFilterTypes.JMS_SELECTOR.getValue());
        WeakReference<JMSSelectorFilter> selectorRef = _selectorCache.get(selectorString);
        JMSSelectorFilter selector = null;

        if(selectorRef == null || (selector = selectorRef.get())==null)
        {
            selector = new JMSSelectorFilter(selectorString);
            _selectorCache.put(selectorString, new WeakReference<JMSSelectorFilter>(selector));
        }
        return selector;
    }

    private static boolean argumentsContainSelector(final FieldTable args)
    {
        return args != null && args.containsKey(AMQPFilterTypes.JMS_SELECTOR.getValue()) && args.getString(AMQPFilterTypes.JMS_SELECTOR.getValue()).trim().length() != 0;
    }

    private AMQShortString normalize(AMQShortString routingKey)
    {
        if(routingKey == null)
        {
            routingKey = AMQShortString.EMPTY_STRING;
        }

        AMQShortStringTokenizer routingTokens = routingKey.tokenize(TOPIC_SEPARATOR);

        List<AMQShortString> subscriptionList = new ArrayList<AMQShortString>();

        while (routingTokens.hasMoreTokens())
        {
            subscriptionList.add(routingTokens.nextToken());
        }

        int size = subscriptionList.size();

        for (int index = 0; index < size; index++)
        {
            // if there are more levels
            if ((index + 1) < size)
            {
                if (subscriptionList.get(index).equals(AMQP_HASH_TOKEN))
                {
                    if (subscriptionList.get(index + 1).equals(AMQP_HASH_TOKEN))
                    {
                        // we don't need #.# delete this one
                        subscriptionList.remove(index);
                        size--;
                        // redo this normalisation
                        index--;
                    }

                    if (subscriptionList.get(index + 1).equals(AMQP_STAR_TOKEN))
                    {
                        // we don't want #.* swap to *.#
                        // remove it and put it in at index + 1
                        subscriptionList.add(index + 1, subscriptionList.remove(index));
                    }
                }
            } // if we have more levels
        }



        AMQShortString normalizedString = AMQShortString.join(subscriptionList, TOPIC_SEPARATOR_AS_SHORTSTRING);

        return normalizedString;
    }

    public ArrayList<AMQQueue> route(InboundMessage payload)
    {

        final AMQShortString routingKey = payload.getRoutingKey() == null
                                          ? AMQShortString.EMPTY_STRING
                                          : new AMQShortString(payload.getRoutingKey());

        // The copy here is unfortunate, but not too bad relevant to the amount of
        // things created and copied in getMatchedQueues
        ArrayList<AMQQueue> queues = new ArrayList<AMQQueue>();
        queues.addAll(getMatchedQueues(payload, routingKey));

        if(queues == null || queues.isEmpty())
        {
            _logger.info("Message routing key: " + payload.getRoutingKey() + " No routes.");
        }

        return queues;

    }

    public boolean isBound(AMQShortString routingKey, FieldTable arguments, AMQQueue queue)
    {
        Binding binding = new Binding(routingKey, queue, arguments);
        if (arguments == null)
        {
            return _bindings.containsKey(binding);
        }
        else
        {
            FieldTable o = _bindings.get(binding);
            if (o != null)
            {
                return o.equals(arguments);
            }
            else
            {
                return false;
            }

        }
    }

    public boolean isBound(AMQShortString routingKey, AMQQueue queue)
    {
        return isBound(routingKey, null, queue);
    }

    public boolean isBound(AMQShortString routingKey)
    {
        for(Binding b : _bindings.keySet())
        {
            if(b.getBindingKey().equals(routingKey))
            {
                return true;
            }
        }

        return false;
    }

    public boolean isBound(AMQQueue queue)
    {
        for(Binding b : _bindings.keySet())
        {
            if(b.getQueue().equals(queue))
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

    public synchronized void deregisterQueue(AMQShortString rKey, AMQQueue queue, FieldTable args) throws AMQException
    {
        assert queue != null;
        assert rKey != null;

        Binding binding = new Binding(rKey, queue, args);


        if (!_bindings.containsKey(binding))
        {
            throw new AMQException(AMQConstant.NOT_FOUND, "Queue " + queue.getName() + " was not registered with exchange " + this.getName()
                                   + " with routing key " + rKey + ".");
        }

        FieldTable bindingArgs = _bindings.remove(binding);
        AMQShortString bindingKey = normalize(rKey);
        TopicExchangeResult result = _topicExchangeResults.get(bindingKey);
        if(argumentsContainSelector(bindingArgs))
        {
            result.removeFilteredQueue(queue, createSelectorFilter(bindingArgs));
        }
        else
        {
            result.removeUnfilteredQueue(queue);
        }

    }

    protected ExchangeMBean createMBean() throws JMException
    {
        return new TopicExchangeMBean();
    }

    public Logger getLogger()
    {
        return _logger;
    }

    private Collection<AMQQueue> getMatchedQueues(InboundMessage message, AMQShortString routingKey)
    {

        Collection<TopicMatcherResult> results = _parser.parse(routingKey);
        if(results.isEmpty())
        {
            return Collections.EMPTY_SET;
        }
        else
        {
            Collection<AMQQueue> queues = results.size() == 1 ? null : new HashSet<AMQQueue>();
            for(TopicMatcherResult result : results)
            {

                queues = ((TopicExchangeResult)result).processMessage(message, queues);
            }
            return queues;
        }


    }
}
