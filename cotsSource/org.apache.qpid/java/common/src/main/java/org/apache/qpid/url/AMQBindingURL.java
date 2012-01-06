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
package org.apache.qpid.url;

import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AMQBindingURL implements BindingURL
{
    private static final Logger _logger = LoggerFactory.getLogger(AMQBindingURL.class);

    String _url;
    AMQShortString _exchangeClass = ExchangeDefaults.DIRECT_EXCHANGE_CLASS;
    AMQShortString _exchangeName = new AMQShortString("");
    AMQShortString _destinationName = new AMQShortString("");;
    AMQShortString _queueName = new AMQShortString("");
    AMQShortString[] _bindingKeys = new AMQShortString[0];
    private HashMap<String, String> _options;

    public AMQBindingURL(String url) throws URISyntaxException
    {
        // format:
        // <exch_class>://<exch_name>/[<destination>]/[<queue>]?<option>='<value>'[,<option>='<value>']*
        _logger.debug("Parsing URL: " + url);
        _url = url;
        _options = new HashMap<String, String>();

        parseBindingURL();
    }

    private void parseBindingURL() throws URISyntaxException
    {
        BindingURLParser parser = new BindingURLParser(_url,this);
        processOptions();
        _logger.debug("URL Parsed: " + this);
    }

    public void setExchangeClass(String exchangeClass)
    {
        setExchangeClass(new AMQShortString(exchangeClass));
    }

    public void setQueueName(String name)
    {
        setQueueName(new AMQShortString(name));
    }

    public void setDestinationName(String name)
    {
        setDestinationName(new AMQShortString(name));
    }

    public void setExchangeName(String exchangeName)
    {
        setExchangeName(new AMQShortString(exchangeName));
    }

    private void processOptions() throws URISyntaxException
    {
    }

    public String getURL()
    {
        return _url;
    }

    public AMQShortString getExchangeClass()
    {
        return _exchangeClass;
    }

    private void setExchangeClass(AMQShortString exchangeClass)
    {

        _exchangeClass = exchangeClass;
        if (exchangeClass.equals(ExchangeDefaults.TOPIC_EXCHANGE_CLASS))
        {
            setOption(BindingURL.OPTION_EXCLUSIVE, "true");
        }

    }

    public AMQShortString getExchangeName()
    {
        return _exchangeName;
    }

    private void setExchangeName(AMQShortString name)
    {
        _exchangeName = name;
    }

    public AMQShortString getDestinationName()
    {
        return _destinationName;
    }

    private void setDestinationName(AMQShortString name)
    {
        _destinationName = name;
    }

    public AMQShortString getQueueName()
    {
        return _queueName;
    }

    public void setQueueName(AMQShortString name)
    {
        _queueName = name;
    }

    public String getOption(String key)
    {
        return _options.get(key);
    }

    public void setOption(String key, String value)
    {
        _options.put(key, value);
    }

    public boolean containsOption(String key)
    {
        return _options.containsKey(key);
    }

    public AMQShortString getRoutingKey()
    {
        if (_exchangeClass.equals(ExchangeDefaults.DIRECT_EXCHANGE_CLASS))
        {
            if (containsOption(BindingURL.OPTION_ROUTING_KEY))
            {
                return new AMQShortString((String)getOption(OPTION_ROUTING_KEY));
            }
            else
            {
                return getQueueName();
            }
        }

        if (containsOption(BindingURL.OPTION_ROUTING_KEY))
        {
            return new AMQShortString((String)getOption(OPTION_ROUTING_KEY));
        }

        return getDestinationName();
    }

    public AMQShortString[] getBindingKeys()
    {
        if (_bindingKeys != null && _bindingKeys.length>0)
        {
            return _bindingKeys;
        }
        else
        {
            return new AMQShortString[]{getRoutingKey()};
        }
    }

    public void setBindingKeys(AMQShortString[] keys)
    {
        _bindingKeys = keys;
    }

    public void setRoutingKey(AMQShortString key)
    {
        setOption(OPTION_ROUTING_KEY, key.toString());
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer();

        sb.append(_exchangeClass);
        sb.append("://");
        sb.append(_exchangeName);
        sb.append('/');
        sb.append(_destinationName);
        sb.append('/');
        sb.append(_queueName);

        sb.append(URLHelper.printOptions(_options));

        // temp hack
        if (getRoutingKey() == null || getRoutingKey().toString().equals(""))
        {

            if (sb.toString().indexOf("?") == -1)
            {
                sb.append("?");
            }
            else
            {
                sb.append("&");
            }

            for (AMQShortString key :_bindingKeys)
            {
                sb.append(BindingURL.OPTION_BINDING_KEY).append("='").append(key.toString()).append("'&");
            }

            return sb.toString().substring(0,sb.toString().length()-1);
        }
        else
        {
            return sb.toString();
        }
    }
}
