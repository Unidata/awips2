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
package org.apache.qpid.client;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.qpid.client.url.URLParser;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ProtocolVersion;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.url.URLHelper;
import org.apache.qpid.url.URLSyntaxException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AMQConnectionURL implements ConnectionURL
{
    private static final Logger _logger = LoggerFactory.getLogger(AMQConnectionURL.class);

    private String _url;
    private String _failoverMethod;
    private Map<String, String> _failoverOptions;
    private Map<String, String> _options;
    private List<BrokerDetails> _brokers;
    private String _clientName;
    private String _username;
    private String _password;
    private String _virtualHost;
    private AMQShortString _defaultQueueExchangeName;
    private AMQShortString _defaultTopicExchangeName;
    private AMQShortString _temporaryTopicExchangeName;
    private AMQShortString _temporaryQueueExchangeName;

    public AMQConnectionURL(String fullURL) throws URLSyntaxException
    {
        if (fullURL == null) throw new IllegalArgumentException("URL cannot be null");
        _url = fullURL;
        _options = new HashMap<String, String>();
        _brokers = new LinkedList<BrokerDetails>();
        _failoverOptions = new HashMap<String, String>();
        new URLParser(this);
    }

    public String getURL()
    {
        return _url;
    }

    public Map<String,String> getOptions()
    {
        return _options;
    }

    public String getFailoverMethod()
    {
        return _failoverMethod;
    }

    public void setFailoverMethod(String failoverMethod)
    {
        _failoverMethod = failoverMethod;
    }

    public Map<String,String> getFailoverOptions()
    {
        return _failoverOptions;
    }

    public String getFailoverOption(String key)
    {
        return _failoverOptions.get(key);
    }

    public void setFailoverOption(String key, String value)
    {
        _failoverOptions.put(key, value);
    }

    public int getBrokerCount()
    {
        return _brokers.size();
    }

    public BrokerDetails getBrokerDetails(int index)
    {
        if (index < _brokers.size())
        {
            return _brokers.get(index);
        }
        else
        {
            return null;
        }
    }

    public void addBrokerDetails(BrokerDetails broker)
    {
        if (!(_brokers.contains(broker)))
        {
            _brokers.add(broker);
        }
    }

    public void setBrokerDetails(List<BrokerDetails> brokers)
    {
        _brokers = brokers;
    }

    public List<BrokerDetails> getAllBrokerDetails()
    {
        return _brokers;
    }

    public String getClientName()
    {
        return _clientName;
    }

    public void setClientName(String clientName)
    {
        _clientName = clientName;
    }

    public String getUsername()
    {
        return _username;
    }

    public void setUsername(String username)
    {
        _username = username;
    }

    public String getPassword()
    {
        return _password;
    }

    public void setPassword(String password)
    {
        _password = password;
    }

    public String getVirtualHost()
    {
        return _virtualHost;
    }

    public void setVirtualHost(String virtuaHost)
    {
        _virtualHost = virtuaHost;
    }

    public String getOption(String key)
    {
        return _options.get(key);
    }

    public void setOption(String key, String value)
    {
        _options.put(key, value);
    }

    public AMQShortString getDefaultQueueExchangeName()
    {
        return _defaultQueueExchangeName;
    }

    public void setDefaultQueueExchangeName(AMQShortString defaultQueueExchangeName)
    {
        _defaultQueueExchangeName = defaultQueueExchangeName;
    }

    public AMQShortString getDefaultTopicExchangeName()
    {
        return _defaultTopicExchangeName;
    }

    public void setDefaultTopicExchangeName(AMQShortString defaultTopicExchangeName)
    {
        _defaultTopicExchangeName = defaultTopicExchangeName;
    }

    public AMQShortString getTemporaryQueueExchangeName()
    {
        return _temporaryQueueExchangeName;
    }

    public void setTemporaryQueueExchangeName(AMQShortString temporaryQueueExchangeName)
    {
        _temporaryQueueExchangeName = temporaryQueueExchangeName;
    }

    public AMQShortString getTemporaryTopicExchangeName()
    {
        return _temporaryTopicExchangeName;
    }

    public void setTemporaryTopicExchangeName(AMQShortString temporaryTopicExchangeName)
    {
        _temporaryTopicExchangeName = temporaryTopicExchangeName;
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer();

        sb.append(AMQ_PROTOCOL);
        sb.append("://");

        if (_username != null)
        {
            sb.append(_username);

            if (_password != null)
            {
                sb.append(':');
                sb.append("********");
            }

            sb.append('@');
        }

        sb.append(_clientName);

        sb.append(_virtualHost);

        sb.append(optionsToString());

        return sb.toString();
    }

    private String optionsToString()
    {
        StringBuffer sb = new StringBuffer();

        sb.append("?" + OPTIONS_BROKERLIST + "='");

        for (BrokerDetails service : _brokers)
        {
            sb.append(service.toString());
            sb.append(';');
        }

        sb.deleteCharAt(sb.length() - 1);
        sb.append("'");

        if (_failoverMethod != null)
        {
            sb.append(URLHelper.DEFAULT_OPTION_SEPERATOR);
            sb.append(OPTIONS_FAILOVER + "='");
            sb.append(_failoverMethod);
            sb.append(URLHelper.printOptions(_failoverOptions));
            sb.append("'");
        }

        return sb.toString();
    }

    public static void main(String[] args) throws URLSyntaxException
    {
        String url2 =
            "amqp://ritchiem:bob@temp/testHost?brokerlist='tcp://localhost:5672;tcp://fancyserver:3000/',failover='roundrobin'";
        // "amqp://user:pass@clientid/virtualhost?brokerlist='tcp://host:1?option1=\'value\',option2=\'value\';vm://:3?option1=\'value\'',failover='method?option1=\'value\',option2='value''";

        ConnectionURL connectionurl2 = new AMQConnectionURL(url2);

        System.out.println(url2);
        System.out.println(connectionurl2);

    }
}
