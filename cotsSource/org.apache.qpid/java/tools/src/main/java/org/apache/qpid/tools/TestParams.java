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
package org.apache.qpid.tools;

import javax.jms.Session;

public class TestParams
{
    private String initialContextFactory = "org.apache.qpid.jndi.PropertiesFileInitialContextFactory";

    private String providerURL = System.getenv("QPID_TEST_HOME") + "/etc/jndi.properties";

    private String connectionFactory = "connectionFactory";

    private String transientDest = "transientQueue";

    private String durableDest = "durableQueue";

    private int msg_size = 1024;

    private int msg_type = 1;   // not used yet

    private boolean cacheMessage = false;

    private boolean disableMessageID = false;

    private boolean disableTimestamp = false;

    private boolean durable = false;

    private boolean transacted = false;

    private int transaction_size = 1000;

    private int ack_mode = Session.AUTO_ACKNOWLEDGE;

    private int msg_count = 10;

    private int warmup_count = 1;
    
    private boolean random_msg_size = false;

    public TestParams()
    {
        initialContextFactory = System.getProperty("java.naming.factory.initial",initialContextFactory);
        providerURL = System.getProperty("java.naming.provider.url",providerURL);

        transientDest = System.getProperty("transDest",transientDest);
        durableDest = System.getProperty("durableDest",durableDest);

        msg_size  = Integer.getInteger("msg_size", 1024);
        msg_type = Integer.getInteger("msg_type",1);
        cacheMessage = Boolean.getBoolean("cache_msg");
        disableMessageID = Boolean.getBoolean("disableMessageID");
        disableTimestamp = Boolean.getBoolean("disableTimestamp");
        durable = Boolean.getBoolean("durable");
        transacted = Boolean.getBoolean("transacted");
        transaction_size = Integer.getInteger("trans_size",1000);
        ack_mode = Integer.getInteger("ack_mode",Session.AUTO_ACKNOWLEDGE);
        msg_count = Integer.getInteger("msg_count",msg_count);
        warmup_count = Integer.getInteger("warmup_count",warmup_count);
        random_msg_size = Boolean.getBoolean("random_msg_size");
    }

    public int getAckMode()
    {
        return ack_mode;
    }

    public String getConnectionFactory()
    {
        return connectionFactory;
    }

    public String getTransientDestination()
    {
        return transientDest;
    }

    public String getDurableDestination()
    {
        return durableDest;
    }

    public String getInitialContextFactory()
    {
        return initialContextFactory;
    }

    public int getMsgCount()
    {
        return msg_count;
    }

    public int getMsgSize()
    {
        return msg_size;
    }

    public int getMsgType()
    {
        return msg_type;
    }

    public boolean isDurable()
    {
        return durable;
    }

    public String getProviderURL()
    {
        return providerURL;
    }

    public boolean isTransacted()
    {
        return transacted;
    }

    public int getTransactionSize()
    {
        return transaction_size;
    }

    public int getWarmupCount()
    {
        return warmup_count;
    }

    public boolean isCacheMessage()
    {
        return cacheMessage;
    }

    public boolean isDisableMessageID()
    {
        return disableMessageID;
    }

    public boolean isDisableTimestamp()
    {
        return disableTimestamp;
    }
    
    public boolean isRandomMsgSize()
    {
        return random_msg_size;
    }

}
