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
package org.apache.qpid.console;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Agent
{
    private static Logger log = LoggerFactory.getLogger(Agent.class);

    public static String AgentKey(long AgentBank, long BrokerBank)
    {
        return String.format("%s:%s", AgentBank, BrokerBank);
    }

    public static long getAgentBank(String routingKey)
    {
        String delim = ".";
        return Long.parseLong(routingKey.split(java.util.regex.Pattern
                .quote(delim))[3]);
    }

    public static long getBrokerBank(String routingKey)
    {
        String delim = ".";
        return Long.parseLong(routingKey.split(java.util.regex.Pattern
                .quote(delim))[2]);
    }

    public static String routingCode(long AgentBank, long BrokerBank)
    {
        return String.format("agent.%s.%s", BrokerBank, AgentBank);
    }

    private long agentBank;
    private Broker broker;
    private long brokerBank;
    private String label;

    public Agent(Broker broker, long agentBank, String label)
    {
        this.setBroker(broker);
        this.setBrokerBank(broker.brokerBank());
        this.setAgentBank(agentBank);
        this.setlabel(label);
    }

    public final String agentKey()
    {
        return Agent.AgentKey(getAgentBank(), getBrokerBank());
    }

    public final long getAgentBank()
    {
        return agentBank;
    }

    public final Broker getBroker()
    {
        return broker;
    }

    public final long getBrokerBank()
    {
        return brokerBank;
    }

    public final String getlabel()
    {
        return label;
    }

    public final String routingCode()
    {
        return Agent.routingCode(getAgentBank(), getBrokerBank());
    }

    public final void setAgentBank(long value)
    {
        agentBank = value;
    }

    public final void setBroker(Broker value)
    {
        broker = value;
    }

    public final void setBrokerBank(long value)
    {
        brokerBank = value;
    }

    public final void setlabel(String value)
    {
        label = value;
    }
}