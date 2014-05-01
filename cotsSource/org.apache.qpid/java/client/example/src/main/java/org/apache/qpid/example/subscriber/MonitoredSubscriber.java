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
package org.apache.qpid.example.subscriber;

import org.apache.qpid.example.shared.Statics;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import javax.jms.*;
/**
 * Subclass of Subscriber which consumes a heartbeat message
 */

public class MonitoredSubscriber extends Subscriber
{
    protected String _monitorDestinationName;

    private static final Logger _logger = LoggerFactory.getLogger(MonitoredSubscriber.class);

    private static MessageConsumer _monitorConsumer;

    public MonitoredSubscriber()
    {
        super();
        //lookup queue name and append suffix
        _monitorDestinationName = _destination.toString() + Statics.MONITOR_QUEUE_SUFFIX;
    }

    /**
     * MessageListener implementation for this subscriber
     */
    public static class MonitorMessageListener implements MessageListener
    {
        private String _name;

        public MonitorMessageListener(String name)
        {
            _name = name;

        }

        /**
         * Listens for heartbeat messages and acknowledges them
         * @param message
         */
        public void onMessage(javax.jms.Message message)
        {
            _logger.info(_name + " monitor got message '" + message + "'");

            try
            {
               _logger.debug("Monitor acknowledging recieved message");

                //Now acknowledge the message to clear it from our queue
                message.acknowledge();
            }
            catch(JMSException j)
            {
                _logger.error("Monitor caught JMSException trying to acknowledge message receipt");
                j.printStackTrace();
            }
            catch(Exception e)
            {
                _logger.error("Monitor caught unexpected exception trying to handle message");
                e.printStackTrace();
            }
        }
    }

    /**
     * Subscribes to Queue and attaches additional monitor listener
     */
    public void subscribeAndMonitor()
    {
        try
        {
            _connection = _connectionFactory.createConnection();

             //create a transactional session
            Session session =  _connection.createSession(true, Session.AUTO_ACKNOWLEDGE);

            //Queue is non-exclusive and not deleted when last consumer detaches
            Destination destination = session.createQueue(_monitorDestinationName);

            //Create a consumer with a destination of our queue which will use defaults for prefetch etc
            _monitorConsumer = session.createConsumer(destination);

            //give the monitor message listener a name of it's own
            _monitorConsumer.setMessageListener(new MonitoredSubscriber.MonitorMessageListener
                ("MonitorListener " + System.currentTimeMillis()));

            MonitoredSubscriber._logger.info("Starting monitored subscription ...");

            MonitoredSubscriber._connection.start();

            //and now start ordinary consumption too
            subscribe();
        }
        catch (Throwable t)
        {
            _logger.error("Fatal error: " + t);
            t.printStackTrace();
        }
    }

    /**
     * Stop consuming
     */
    public void stopMonitor()
    {
        try
        {
            _monitorConsumer.close();
            _monitorConsumer = null;
            stop();
        }
        catch(JMSException j)
        {
            _logger.error("JMSException trying to Subscriber.stop: " + j.getStackTrace());
        }
    }

}
