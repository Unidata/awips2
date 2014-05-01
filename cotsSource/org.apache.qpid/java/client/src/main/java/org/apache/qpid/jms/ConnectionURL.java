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
package org.apache.qpid.jms;

import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ProtocolVersion;

import java.util.List;

/**
 Connection URL format
 amqp://[user:pass@][clientid]/virtualhost?brokerlist='tcp://host:port?option=\'value\'&option=\'value\';vm://:3/virtualpath?option=\'value\''&failover='method?option=\'value\'&option='value''"
 Options are of course optional except for requiring a single broker in the broker list.
 The option seperator is defined to be either '&' or ','
  */
public interface ConnectionURL
{
    public static final String AMQ_PROTOCOL = "amqp";
    public static final String OPTIONS_SYNC_PERSISTENCE = "sync_persistence";
    public static final String OPTIONS_MAXPREFETCH = "maxprefetch";
    public static final String OPTIONS_SYNC_ACK = "sync_ack";    
    public static final String OPTIONS_SYNC_PUBLISH = "sync_publish";
    public static final String OPTIONS_BROKERLIST = "brokerlist";
    public static final String OPTIONS_FAILOVER = "failover";
    public static final String OPTIONS_FAILOVER_CYCLE = "cyclecount";
    public static final String OPTIONS_SSL = "ssl";
    public static final String OPTIONS_DEFAULT_TOPIC_EXCHANGE = "defaultTopicExchange";
    public static final String OPTIONS_DEFAULT_QUEUE_EXCHANGE = "defaultQueueExchange";
    public static final String OPTIONS_TEMPORARY_TOPIC_EXCHANGE = "temporaryTopicExchange";
    public static final String OPTIONS_TEMPORARY_QUEUE_EXCHANGE = "temporaryQueueExchange";
    public static final byte  URL_0_8 = 1;
    public static final byte  URL_0_10 = 2;
    
    String getURL();

    String getFailoverMethod();

    String getFailoverOption(String key);

    int getBrokerCount();

    BrokerDetails getBrokerDetails(int index);

    void addBrokerDetails(BrokerDetails broker);

    void setBrokerDetails(List<BrokerDetails> brokers);

    List<BrokerDetails> getAllBrokerDetails();

    String getClientName();

    void setClientName(String clientName);

    String getUsername();

    void setUsername(String username);

    String getPassword();

    void setPassword(String password);

    String getVirtualHost();

    void setVirtualHost(String virtualHost);

    String getOption(String key);

    void setOption(String key, String value);

    AMQShortString getDefaultQueueExchangeName();

    AMQShortString getDefaultTopicExchangeName();

    AMQShortString getTemporaryQueueExchangeName();

    AMQShortString getTemporaryTopicExchangeName();

}
