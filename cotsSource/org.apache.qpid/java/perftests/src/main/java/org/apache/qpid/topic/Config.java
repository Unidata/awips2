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
package org.apache.qpid.topic;

import org.apache.qpid.client.AMQSession;
import org.apache.qpid.config.ConnectorConfig;
import org.apache.qpid.config.Connector;
import org.apache.qpid.config.AbstractConfig;

import javax.jms.Connection;

public class Config extends AbstractConfig implements ConnectorConfig
{

    private String host = "localhost";
    private int port = 5672;
    private String factory = null;

    private int payload = 256;
    private int messages = 1000;
    private int clients = 1;
    private int batch = 1;
    private long delay = 1;
    private int warmup;
    private int ackMode= AMQSession.NO_ACKNOWLEDGE;
    private String clientId;
    private String subscriptionId;
    private String selector;
    private String destinationName;
    private boolean persistent;
    private boolean transacted;
    private int destinationsCount;
    private int batchSize;
    private int rate;
    private boolean ispubsub;
    private long timeout;

    public Config()
    {
    }

    public int getAckMode()
    {
        return ackMode;
    }

    public void setPayload(int payload)
    {
        this.payload = payload;
    }

    public int getPayload()
    {
        return payload;
    }

    void setClients(int clients)
    {
        this.clients = clients;
    }

    int getClients()
    {
        return clients;
    }

    void setMessages(int messages)
    {
        this.messages = messages;
    }

    public int getMessages()
    {
        return messages;
    }

    public int getBatchSize()
    {
        return batchSize;
    }

    public int getRate()
    {
        return rate;
    }

    public int getDestinationsCount()
    {
        return destinationsCount;
    }

    public String getHost()
    {
        return host;
    }

    public void setHost(String host)
    {
        this.host = host;
    }

    public int getPort()
    {
        return port;
    }

    public String getFactory()
    {
        return factory;
    }

    public void setPort(int port)
    {
        this.port = port;
    }

    int getBatch()
    {
        return batch;
    }

    void setBatch(int batch)
    {
        this.batch = batch;
    }

    int getWarmup()
    {
        return warmup;
    }

    void setWarmup(int warmup)
    {
        this.warmup = warmup;
    }

    public long getDelay()
    {
        return delay;
    }

    public void setDelay(long delay)
    {
        this.delay = delay;
    }

    public long getTimeout()
    {
        return timeout;
    }

    public void setTimeout(long time)
    {
        this.timeout = time;
    }

    public String getClientId()
    {
        return clientId;
    }

    public String getSubscriptionId()
    {
        return subscriptionId;
    }

    public String getSelector()
    {
        return selector;
    }

    public String getDestination()
    {
        return destinationName;
    }

    public boolean usePersistentMessages()
    {
        return persistent;
    }

    public boolean isTransacted()
    {
        return transacted;
    }

    public boolean isPubSub()
    {
        return ispubsub;
    }

    public void setOption(String key, String value)
    {
        if("-host".equalsIgnoreCase(key))
        {
            setHost(value);
        }
        else if("-port".equalsIgnoreCase(key))
        {
            try
            {
                setPort(Integer.parseInt(value));
            }
            catch(NumberFormatException e)
            {
                throw new RuntimeException("Bad port number: " + value, e);
            }
        }
        else if("-payload".equalsIgnoreCase(key))
        {
            setPayload(parseInt("Bad payload size", value));
        }
        else if("-messages".equalsIgnoreCase(key))
        {
            setMessages(parseInt("Bad message count", value));
        }
        else if("-clients".equalsIgnoreCase(key))
        {
            setClients(parseInt("Bad client count", value));
        }
        else if("-batch".equalsIgnoreCase(key))
        {
            setBatch(parseInt("Bad batch count", value));
        }
        else if("-delay".equalsIgnoreCase(key))
        {
            setDelay(parseLong("Bad batch delay", value));
        }
        else if("-warmup".equalsIgnoreCase(key))
        {
            setWarmup(parseInt("Bad warmup count", value));
        }
        else if("-ack".equalsIgnoreCase(key))
        {
            ackMode = parseInt("Bad ack mode", value);
        }
        else if("-factory".equalsIgnoreCase(key))
        {
            factory = value;
        }
        else if("-clientId".equalsIgnoreCase(key))
        {
            clientId = value;
        }
        else if("-subscriptionId".equalsIgnoreCase(key))
        {
            subscriptionId = value;
        }
        else if("-persistent".equalsIgnoreCase(key))
        {
            persistent = "true".equalsIgnoreCase(value);
        }
        else if("-transacted".equalsIgnoreCase(key))
        {
            transacted = "true".equalsIgnoreCase(value);
        }
        else if ("-destinationscount".equalsIgnoreCase(key))
        {
            destinationsCount = parseInt("Bad destinations count", value);
        }
        else if ("-batchsize".equalsIgnoreCase(key))
        {
            batchSize = parseInt("Bad batch size", value);
        }
        else if ("-rate".equalsIgnoreCase(key))
        {
            rate = parseInt("MEssage rate", value);
        }
        else if("-pubsub".equalsIgnoreCase(key))
        {
            ispubsub = "true".equalsIgnoreCase(value);
        }
        else if("-selector".equalsIgnoreCase(key))
        {
            selector = value;
        }
        else if("-destinationname".equalsIgnoreCase(key))
        {
            destinationName = value;
        }
        else if("-timeout".equalsIgnoreCase(key))
        {
            setTimeout(parseLong("Bad timeout data", value));
        }
        else
        {
            System.out.println("Ignoring unrecognised option: " + key);
        }
    }

    static String getAckModeDescription(int ackMode)
    {
        switch(ackMode)
        {
            case AMQSession.NO_ACKNOWLEDGE: return "NO_ACKNOWLEDGE";
            case AMQSession.AUTO_ACKNOWLEDGE: return "AUTO_ACKNOWLEDGE";
            case AMQSession.CLIENT_ACKNOWLEDGE: return "CLIENT_ACKNOWLEDGE";
            case AMQSession.DUPS_OK_ACKNOWLEDGE: return "DUPS_OK_ACKNOWELDGE";
            case AMQSession.PRE_ACKNOWLEDGE: return "PRE_ACKNOWLEDGE";
        }
        return "AckMode=" + ackMode;
    }

    public Connection createConnection() throws Exception
    {
        return new Connector().createConnection(this);
    }
}
