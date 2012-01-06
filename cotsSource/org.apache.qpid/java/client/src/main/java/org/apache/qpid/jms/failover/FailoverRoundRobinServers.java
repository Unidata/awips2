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
package org.apache.qpid.jms.failover;

import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.jms.ConnectionURL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FailoverRoundRobinServers implements FailoverMethod
{
    private static final Logger _logger = LoggerFactory.getLogger(FailoverRoundRobinServers.class);

    /** The default number of times to cycle through all servers */
    public static final int DEFAULT_CYCLE_RETRIES = 1;
    /** The default number of times to retry each server */
    public static final int DEFAULT_SERVER_RETRIES = 0;

    /** The index into the hostDetails array of the broker to which we are connected */
    private int _currentBrokerIndex = 0;

    /** The number of times to retry connecting for each server */
    private int _serverRetries;

    /** The current number of retry attempts made */
    private int _currentServerRetry = 0;

    /** The number of times to cycle through the servers */
    private int _cycleRetries;

    /** The current number of cycles performed. */
    private int _currentCycleRetries = 0;

    /** Array of BrokerDetail used to make connections. */
    protected ConnectionURL _connectionDetails;

    public FailoverRoundRobinServers(ConnectionURL connectionDetails)
    {
        if (!(connectionDetails.getBrokerCount() > 0))
        {
            throw new IllegalArgumentException("At least one broker details must be specified.");
        }

        _connectionDetails = connectionDetails;

        // There is no current broker at startup so set it to -1.
        _currentBrokerIndex = 0;

        String cycleRetries = _connectionDetails.getFailoverOption(ConnectionURL.OPTIONS_FAILOVER_CYCLE);

        _cycleRetries = DEFAULT_CYCLE_RETRIES;
        
        if (cycleRetries != null)
        {
            try
            {
                _cycleRetries = Integer.parseInt(cycleRetries);
            }
            catch (NumberFormatException nfe)
            {
                _logger.warn("Cannot set cycle Retries, " + cycleRetries + " is not a number. Using default: " + DEFAULT_CYCLE_RETRIES); 
            }
        }

        _currentCycleRetries = 0;

        _serverRetries = 0;
        _currentServerRetry = 0;
    }

    public void reset()
    {
        _currentBrokerIndex = 0;
        _currentCycleRetries = 0;
        _currentServerRetry = 0;
    }

    public boolean failoverAllowed()
    {
        _logger.info("==== Checking failoverAllowed() ====");
        _logger.info(toString());
        _logger.info("====================================");
        return ((_currentCycleRetries < _cycleRetries) || (_currentServerRetry < _serverRetries));
    }

    public void attainedConnection()
    {
        _currentCycleRetries = 0;
        _currentServerRetry = 0;
    }

    public BrokerDetails getCurrentBrokerDetails()
    {
        return _connectionDetails.getBrokerDetails(_currentBrokerIndex);
    }

    public BrokerDetails getNextBrokerDetails()
    {
        boolean doDelay = false;

        if (_currentBrokerIndex == (_connectionDetails.getBrokerCount() - 1))
        {
            if (_currentServerRetry < _serverRetries)
            {
                _logger.info("Trying " + _connectionDetails.getBrokerDetails(_currentBrokerIndex));
                doDelay= _currentBrokerIndex != 0;
                _currentServerRetry++;
            }
            else
            {
                _currentCycleRetries++;
                // failed to connect to first broker
                _currentBrokerIndex = 0;

                setBroker(_connectionDetails.getBrokerDetails(_currentBrokerIndex));

                // This is zero rather than -1 as we are already retrieving the details.
                _currentServerRetry = 0;
            }
            // else - should force client to stop as max retries has been reached.
        }
        else
        {
            if (_currentServerRetry < _serverRetries)
            {
                _logger.info("Trying " + _connectionDetails.getBrokerDetails(_currentBrokerIndex));
                doDelay= _currentBrokerIndex != 0;

                _currentServerRetry++;
            }
            else
            {
                _currentBrokerIndex++;

                setBroker(_connectionDetails.getBrokerDetails(_currentBrokerIndex));
                // This is zero rather than -1 as we are already retrieving the details.
                _currentServerRetry = 0;
            }
        }

        BrokerDetails broker = _connectionDetails.getBrokerDetails(_currentBrokerIndex);

        String delayStr = broker.getProperty(BrokerDetails.OPTIONS_CONNECT_DELAY);
        if (delayStr != null && doDelay)
        {
            Long delay = Long.parseLong(delayStr);
            _logger.info("Delay between connect retries:" + delay);
            try
            {
                Thread.sleep(delay);
            }
            catch (InterruptedException ie)
            {
                return null;
            }
        }
        else
        {
            // Only display if option not set. Not if deDelay==false.
            if (delayStr == null)
            {
                _logger.info("No delay between connect retries, use tcp://host:port?connectdelay='value' to enable.");
            }
        }

        return broker;
    }

    public void setBroker(BrokerDetails broker)
    {

        _connectionDetails.addBrokerDetails(broker);

        int index = _connectionDetails.getAllBrokerDetails().indexOf(broker);

        String serverRetries = broker.getProperty(BrokerDetails.OPTIONS_RETRY);

        if (serverRetries != null)
        {
            try
            {
                _serverRetries = Integer.parseInt(serverRetries);
            }
            catch (NumberFormatException nfe)
            {
                _serverRetries = DEFAULT_SERVER_RETRIES;
            }
        }

        _currentServerRetry = 0;
        _currentBrokerIndex = index;
    }

    public void setRetries(int maxRetries)
    {
        _cycleRetries = maxRetries;
    }

    public String methodName()
    {
        return "Cycle Servers";
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer();

        sb.append("Cycle Servers:\n");

        sb.append("Cycle Retries:");
        sb.append(_cycleRetries);
        sb.append("\nCurrent Cycle:");
        sb.append(_currentCycleRetries);
        sb.append("\nServer Retries:");
        sb.append(_serverRetries);
        sb.append("\nCurrent Retry:");
        sb.append(_currentServerRetry);
        sb.append("\nCurrent Broker:");
        sb.append(_currentBrokerIndex);
        sb.append("\n");

        for (int i = 0; i < _connectionDetails.getBrokerCount(); i++)
        {
            if (i == _currentBrokerIndex)
            {
                sb.append(">");
            }

            sb.append(_connectionDetails.getBrokerDetails(i));
            sb.append("\n");
        }

        return sb.toString();
    }
}
