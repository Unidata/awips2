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
package org.apache.qpid.jndi;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.Queue;
import javax.jms.Topic;
import javax.naming.Context;
import javax.naming.NamingException;
import javax.naming.spi.InitialContextFactory;

import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQHeadersExchange;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.url.AMQBindingURL;
import org.apache.qpid.url.BindingURL;
import org.apache.qpid.url.URLSyntaxException;
import org.apache.qpid.util.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PropertiesFileInitialContextFactory implements InitialContextFactory
{
    protected final Logger _logger = LoggerFactory.getLogger(PropertiesFileInitialContextFactory.class);

    private String CONNECTION_FACTORY_PREFIX = "connectionfactory.";
    private String DESTINATION_PREFIX = "destination.";
    private String QUEUE_PREFIX = "queue.";
    private String TOPIC_PREFIX = "topic.";

    public Context getInitialContext(Hashtable environment) throws NamingException
    {
        Map data = new ConcurrentHashMap();

        try
        {

            String file = null;
            if (environment.containsKey(Context.PROVIDER_URL))
            {
                file = (String) environment.get(Context.PROVIDER_URL);
            }
            else
            {
                file = System.getProperty(Context.PROVIDER_URL);
            }

            if (file != null)
            {
                _logger.info("Loading Properties from:" + file);
                // Load the properties specified
                Properties p = new Properties();

                p.load(new BufferedInputStream(new FileInputStream(file)));
                Strings.Resolver resolver = new Strings.ChainedResolver
                    (Strings.SYSTEM_RESOLVER, new Strings.PropertiesResolver(p));

                for (Map.Entry me : p.entrySet())
                {
                    String key = (String) me.getKey();
                    String value = (String) me.getValue();
                    String expanded = Strings.expand(value, resolver);
                    environment.put(key, expanded);
                    if (System.getProperty(key) == null)
                    {
                        System.setProperty(key, expanded);
                    }
                }
                _logger.info("Loaded Context Properties:" + environment.toString());
            }
            else
            {
                _logger.info("No Provider URL specified.");
            }
        }
        catch (IOException ioe)
        {
            _logger.warn("Unable to load property file specified in Provider_URL:" + environment.get(Context.PROVIDER_URL) +"\n" +
                         "Due to:"+ioe.getMessage());
        }

        createConnectionFactories(data, environment);

        createDestinations(data, environment);

        createQueues(data, environment);

        createTopics(data, environment);

        return createContext(data, environment);
    }

    // Implementation methods
    // -------------------------------------------------------------------------
    protected ReadOnlyContext createContext(Map data, Hashtable environment)
    {
        return new ReadOnlyContext(environment, data);
    }

    protected void createConnectionFactories(Map data, Hashtable environment)
    {
        for (Iterator iter = environment.entrySet().iterator(); iter.hasNext();)
        {
            Map.Entry entry = (Map.Entry) iter.next();
            String key = entry.getKey().toString();
            if (key.startsWith(CONNECTION_FACTORY_PREFIX))
            {
                String jndiName = key.substring(CONNECTION_FACTORY_PREFIX.length());
                ConnectionFactory cf = createFactory(entry.getValue().toString().trim());
                if (cf != null)
                {
                    data.put(jndiName, cf);
                }
            }
        }
    }

    protected void createDestinations(Map data, Hashtable environment)
    {
        for (Iterator iter = environment.entrySet().iterator(); iter.hasNext();)
        {
            Map.Entry entry = (Map.Entry) iter.next();
            String key = entry.getKey().toString();
            if (key.startsWith(DESTINATION_PREFIX))
            {
                String jndiName = key.substring(DESTINATION_PREFIX.length());
                Destination dest = createDestination(entry.getValue().toString().trim());
                if (dest != null)
                {
                    data.put(jndiName, dest);
                }
            }
        }
    }

    protected void createQueues(Map data, Hashtable environment)
    {
        for (Iterator iter = environment.entrySet().iterator(); iter.hasNext();)
        {
            Map.Entry entry = (Map.Entry) iter.next();
            String key = entry.getKey().toString();
            if (key.startsWith(QUEUE_PREFIX))
            {
                String jndiName = key.substring(QUEUE_PREFIX.length());
                Queue q = createQueue(entry.getValue().toString().trim());
                if (q != null)
                {
                    data.put(jndiName, q);
                }
            }
        }
    }

    protected void createTopics(Map data, Hashtable environment)
    {
        for (Iterator iter = environment.entrySet().iterator(); iter.hasNext();)
        {
            Map.Entry entry = (Map.Entry) iter.next();
            String key = entry.getKey().toString();
            if (key.startsWith(TOPIC_PREFIX))
            {
                String jndiName = key.substring(TOPIC_PREFIX.length());
                Topic t = createTopic(entry.getValue().toString().trim());
                if (t != null)
                {
                    if (_logger.isDebugEnabled())
                    {
                        StringBuffer b = new StringBuffer();
                        b.append("Creating the topic: " + jndiName +  " with the following binding keys ");
                        for (AMQShortString binding:((AMQTopic)t).getBindingKeys())
                        {
                            b.append(binding.toString()).append(",");
                        }

                        _logger.debug(b.toString());
                    }
                    data.put(jndiName, t);
                }
            }
        }
    }

    /**
     * Factory method to create new Connection Factory instances
     */
    protected ConnectionFactory createFactory(String url)
    {
        try
        {
            return new AMQConnectionFactory(url);
        }
        catch (URLSyntaxException urlse)
        {
            _logger.warn("Unable to createFactories:" + urlse);
        }

        return null;
    }

    /**
     * Factory method to create new Destination instances from an AMQP BindingURL
     */
    protected Destination createDestination(String bindingURL)
    {
        AMQBindingURL binding;
        try
        {
            binding = new AMQBindingURL(bindingURL);
        }
        catch (URISyntaxException urlse)
        {
            _logger.warn("Unable to create destination:" + urlse, urlse);

            return null;
        }

        try
        {
            return AMQDestination.createDestination(binding);
        }
        catch (IllegalArgumentException iaw)
        {
            _logger.warn("Binding: '" + binding + "' not supported");

            return null;
        }
    }

    /**
     * Factory method to create new Queue instances
     */
    protected Queue createQueue(Object value)
    {
        if (value instanceof AMQShortString)
        {
            return new AMQQueue(ExchangeDefaults.DIRECT_EXCHANGE_NAME, (AMQShortString) value);
        }
        else if (value instanceof String)
        {
            return new AMQQueue(ExchangeDefaults.DIRECT_EXCHANGE_NAME, new AMQShortString((String) value));
        }
        else if (value instanceof BindingURL)
        {
            return new AMQQueue((BindingURL) value);
        }

        return null;
    }

    /**
     * Factory method to create new Topic instances
     */
    protected Topic createTopic(Object value)
    {
        if (value instanceof AMQShortString)
        {
            return new AMQTopic(ExchangeDefaults.TOPIC_EXCHANGE_NAME, (AMQShortString) value);
        }
        else if (value instanceof String)
        {
            String[] keys = ((String)value).split(",");
            AMQShortString[] bindings = new AMQShortString[keys.length];
            int i = 0;
            for (String key:keys)
            {
                bindings[i] = new AMQShortString(key.trim());
                i++;
            }
            // The Destination has a dual nature. If this was used for a producer the key is used
            // for the routing key. If it was used for the consumer it becomes the bindingKey
            return new AMQTopic(ExchangeDefaults.TOPIC_EXCHANGE_NAME,bindings[0],null,bindings);
        }
        else if (value instanceof BindingURL)
        {
            return new AMQTopic((BindingURL) value);
        }

        return null;
    }

    /**
     * Factory method to create new HeaderExcahnge instances
     */
    protected Destination createHeaderExchange(Object value)
    {
        if (value instanceof String)
        {
            return new AMQHeadersExchange((String) value);
        }
        else if (value instanceof BindingURL)
        {
            return new AMQHeadersExchange((BindingURL) value);
        }

        return null;
    }

    // Properties
    // -------------------------------------------------------------------------
    public String getConnectionPrefix()
    {
        return CONNECTION_FACTORY_PREFIX;
    }

    public void setConnectionPrefix(String connectionPrefix)
    {
        this.CONNECTION_FACTORY_PREFIX = connectionPrefix;
    }

    public String getDestinationPrefix()
    {
        return DESTINATION_PREFIX;
    }

    public void setDestinationPrefix(String destinationPrefix)
    {
        this.DESTINATION_PREFIX = destinationPrefix;
    }

    public String getQueuePrefix()
    {
        return QUEUE_PREFIX;
    }

    public void setQueuePrefix(String queuePrefix)
    {
        this.QUEUE_PREFIX = queuePrefix;
    }

    public String getTopicPrefix()
    {
        return TOPIC_PREFIX;
    }

    public void setTopicPrefix(String topicPrefix)
    {
        this.TOPIC_PREFIX = topicPrefix;
    }
}
