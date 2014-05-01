/*
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
 */
package org.apache.qpid.example.publisher;


import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import javax.jms.DeliveryMode;
import javax.jms.JMSException;

/**
 * Class that sends heartbeat messages to allow monitoring of message consumption Sends regular (currently 20 seconds
 * apart) heartbeat message
 */
public class MonitorMessageDispatcher
{

    private static final Logger _logger = LoggerFactory.getLogger(MonitorMessageDispatcher.class);

    protected static MonitorPublisher _monitorPublisher = null;

    protected static final String DEFAULT_MONITOR_PUB_NAME = "MonitorPublisher";

    /**
     * Easy entry point for running a message dispatcher for monitoring consumption
     * Sends 1000 messages with no delay 
     *
     * @param args
     */
    public static void main(String[] args)
    {
        //Switch on logging appropriately for your app
        try
        {
            int i =0;
            while (i < 1000)
            {
                try
                {
                    //endlessly publish messages to monitor queue
                    publish();

                    if (_logger.isDebugEnabled())
                    {
                        _logger.debug("Dispatched monitor message");
                    }

                    //sleep for twenty seconds and then publish again - change if appropriate
                    //Thread.sleep(1000);
                    i++   ;
                }
                catch (UndeliveredMessageException a)
                {
                    //trigger application specific failure handling here
                    _logger.error("Problem delivering monitor message");
                    break;
                }
            }
        }
        catch (Exception e)
        {
            _logger.error("Error trying to dispatch AMS monitor message: " + e);
            System.exit(1);
        }
        finally
        {
            if (getMonitorPublisher() != null)
            {
                getMonitorPublisher().cleanup();
            }
        }

        System.exit(1);
    }

    /**
     * Publish heartbeat message
     *
     * @throws JMSException
     * @throws UndeliveredMessageException
     */
    public static void publish() throws JMSException, UndeliveredMessageException
    {
        //Send the message generated from the payload using the _publisher
//        getMonitorPublisher().sendImmediateMessage
//          (FileMessageFactory.createSimpleEventMessage(getMonitorPublisher().getSession(),"monitor:" +System.currentTimeMillis()));

        getMonitorPublisher().sendMessage
                (getMonitorPublisher()._session,
                 FileMessageFactory.createSimpleEventMessage(getMonitorPublisher().getSession(), "monitor:" + System.currentTimeMillis()),
                 DeliveryMode.PERSISTENT, false, true);

    }

    /** Cleanup publishers */
    public static void cleanup()
    {
        if (getMonitorPublisher() != null)
        {
            getMonitorPublisher().cleanup();
        }

        if (getMonitorPublisher() != null)
        {
            getMonitorPublisher().cleanup();
        }
    }

    //Returns a _publisher for the monitor queue
    private static MonitorPublisher getMonitorPublisher()
    {
        if (_monitorPublisher != null)
        {
            return _monitorPublisher;
        }

        //Create a _publisher using failover details and constant for monitor queue
        _monitorPublisher = new MonitorPublisher();

        _monitorPublisher.setName(MonitorMessageDispatcher.DEFAULT_MONITOR_PUB_NAME);
        return _monitorPublisher;
    }

}
