package org.apache.qpid.client.url;
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


import java.net.URI;
import java.net.URISyntaxException;
import java.util.StringTokenizer;

import org.apache.qpid.client.AMQBrokerDetails;
import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.client.AMQConnectionURL;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.url.URLHelper;
import org.apache.qpid.url.URLSyntaxException;

public class URLParser
{
    private AMQConnectionURL _url;

    public URLParser(AMQConnectionURL url)throws URLSyntaxException
    {
        _url = url;
        parseURL(_url.getURL());
    }

    private void parseURL(String fullURL) throws URLSyntaxException
    {
        // Connection URL format
        // amqp://[user:pass@][clientid]/virtualhost?brokerlist='tcp://host:port?option=\'value\',option=\'value\';vm://:3/virtualpath?option=\'value\'',failover='method?option=\'value\',option='value''"
        // Options are of course optional except for requiring a single broker in the broker list.
        try
        {
            URI connection = new URI(fullURL);

            if ((connection.getScheme() == null) || !(connection.getScheme().equalsIgnoreCase(AMQConnectionURL.AMQ_PROTOCOL)))
            {
                throw new URISyntaxException(fullURL, "Not an AMQP URL");
            }

            if ((connection.getHost() == null) || connection.getHost().equals(""))
            {
                String tmp = connection.getAuthority();
                // hack to read a clientid such as "my_clientID"
                if (tmp != null && tmp.indexOf('@') < tmp.length()-1)
                {                   
                    _url.setClientName(tmp.substring(tmp.indexOf('@')+1,tmp.length()));
                }
                else
                {
                    String uid = AMQConnectionFactory.getUniqueClientID();
                    if (uid == null)
                    {
                        throw URLHelper.parseError(-1, "Client Name not specified", fullURL);
                    }
                    else
                    {
                        _url.setClientName(uid);
                    }
                }

            }            
            else
            {
                _url.setClientName(connection.getHost());
            }
            
            String userInfo = connection.getUserInfo();

            if (userInfo == null)
            {
                // Fix for Java 1.5 which doesn't parse UserInfo for non http URIs
                userInfo = connection.getAuthority();

                if (userInfo != null)
                {
                    int atIndex = userInfo.indexOf('@');

                    if (atIndex != -1)
                    {
                        userInfo = userInfo.substring(0, atIndex);
                    }
                    else
                    {
                        userInfo = null;
                    }
                }

            }

            if (userInfo == null)
            {
                throw URLHelper.parseError(AMQConnectionURL.AMQ_PROTOCOL.length() + 3, "User information not found on url", fullURL);
            }
            else
            {
                parseUserInfo(userInfo);
            }

            String virtualHost = connection.getPath();

            if ((virtualHost != null) && (!virtualHost.equals("")))
            {
                _url.setVirtualHost(virtualHost);
            }
            else
            {
                int authLength = connection.getAuthority().length();
                int start = AMQConnectionURL.AMQ_PROTOCOL.length() + 3;
                int testIndex = start + authLength;
                if ((testIndex < fullURL.length()) && (fullURL.charAt(testIndex) == '?'))
                {
                    throw URLHelper.parseError(start, testIndex - start, "Virtual host found", fullURL);
                }
                else
                {
                    throw URLHelper.parseError(-1, "Virtual host not specified", fullURL);
                }

            }

            URLHelper.parseOptions(_url.getOptions(), connection.getQuery());

            processOptions();
        }
        catch (URISyntaxException uris)
        {
            if (uris instanceof URLSyntaxException)
            {
                throw (URLSyntaxException) uris;
            }

            int slash = fullURL.indexOf("\\");

            if (slash == -1)
            {
                throw URLHelper.parseError(uris.getIndex(), uris.getReason(), uris.getInput());
            }
            else
            {
                if ((slash != 0) && (fullURL.charAt(slash - 1) == ':'))
                {
                    throw URLHelper.parseError(slash - 2, fullURL.indexOf('?') - slash + 2,
                        "Virtual host looks like a windows path, forward slash not allowed in URL", fullURL);
                }
                else
                {
                    throw URLHelper.parseError(slash, "Forward slash not allowed in URL", fullURL);
                }
            }

        }
    }

    private void parseUserInfo(String userinfo) throws URLSyntaxException
    {
        // user info = user:pass

        int colonIndex = userinfo.indexOf(':');

        if (colonIndex == -1)
        {
            throw URLHelper.parseError(AMQConnectionURL.AMQ_PROTOCOL.length() + 3, userinfo.length(),
                                       "Null password in user information not allowed.", _url.getURL());
        }
        else
        {
            _url.setUsername(userinfo.substring(0, colonIndex));
            _url.setPassword(userinfo.substring(colonIndex + 1));
        }

    }

    private void processOptions() throws URLSyntaxException
    {
        if (_url.getOptions().containsKey(AMQConnectionURL.OPTIONS_BROKERLIST))
        {
            String brokerlist = _url.getOptions().get(AMQConnectionURL.OPTIONS_BROKERLIST);

            // brokerlist tcp://host:port?option='value',option='value';vm://:3/virtualpath?option='value'
            StringTokenizer st = new StringTokenizer(brokerlist, "" + URLHelper.BROKER_SEPARATOR);

            while (st.hasMoreTokens())
            {
                String broker = st.nextToken();

                _url.addBrokerDetails(new AMQBrokerDetails(broker));
            }

            _url.getOptions().remove(AMQConnectionURL.OPTIONS_BROKERLIST);
        }

        if (_url.getOptions().containsKey(AMQConnectionURL.OPTIONS_FAILOVER))
        {
            String failover = _url.getOptions().get(AMQConnectionURL.OPTIONS_FAILOVER);

            // failover='method?option='value',option='value''

            int methodIndex = failover.indexOf('?');

            if (methodIndex > -1)
            {
                _url.setFailoverMethod(failover.substring(0, methodIndex));
                URLHelper.parseOptions(_url.getFailoverOptions(), failover.substring(methodIndex + 1));
            }
            else
            {
                _url.setFailoverMethod(failover);
            }

            _url.getOptions().remove(AMQConnectionURL.OPTIONS_FAILOVER);
        }

        if (_url.getOptions().containsKey(AMQConnectionURL.OPTIONS_DEFAULT_TOPIC_EXCHANGE))
        {
            _url.setDefaultTopicExchangeName(new AMQShortString(_url.getOptions().get(AMQConnectionURL.OPTIONS_DEFAULT_TOPIC_EXCHANGE)));
        }

        if (_url.getOptions().containsKey(AMQConnectionURL.OPTIONS_DEFAULT_QUEUE_EXCHANGE))
        {
            _url.setDefaultQueueExchangeName(new AMQShortString(_url.getOptions().get(AMQConnectionURL.OPTIONS_DEFAULT_QUEUE_EXCHANGE)));
        }

        if (_url.getOptions().containsKey(AMQConnectionURL.OPTIONS_TEMPORARY_QUEUE_EXCHANGE))
        {
            _url.setTemporaryQueueExchangeName(new AMQShortString(_url.getOptions().get(AMQConnectionURL.OPTIONS_TEMPORARY_QUEUE_EXCHANGE)));
        }

        if (_url.getOptions().containsKey(AMQConnectionURL.OPTIONS_TEMPORARY_TOPIC_EXCHANGE))
        {
            _url.setTemporaryTopicExchangeName(new AMQShortString(_url.getOptions().get(AMQConnectionURL.OPTIONS_TEMPORARY_TOPIC_EXCHANGE)));
        }
    }

}
