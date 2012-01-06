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

public class FailoverSingleServer implements FailoverMethod
{
    private static final Logger _logger = LoggerFactory.getLogger(FailoverSingleServer.class);

    /** The default number of times to rety a conection to this server */
    public static final int DEFAULT_SERVER_RETRIES = 0;

    /** The details of the Single Server */
    private BrokerDetails _brokerDetail;

    /** The number of times to retry connecting to the sever */
    protected int _retries;

    /** The current number of attempts made to the server */
    protected int _currentRetries = 0;


    public FailoverSingleServer(ConnectionURL connectionDetails)
    {
        if (connectionDetails.getBrokerCount() > 0)
        {
            setBroker(connectionDetails.getBrokerDetails(0));
        }
        else
        {
            throw new IllegalArgumentException("BrokerDetails details required for connection.");
        }
    }

    public FailoverSingleServer(BrokerDetails brokerDetail)
    {
        setBroker(brokerDetail);
    }

    public void reset()
    {
        _currentRetries = 0;
    }

    public boolean failoverAllowed()
    {
        return _currentRetries < _retries;
    }

    public void attainedConnection()
    {
        reset();
    }

    public BrokerDetails getCurrentBrokerDetails()
    {
        return _brokerDetail;
    }

    public BrokerDetails getNextBrokerDetails()
    {
        if (_currentRetries == _retries)
        {
            return null;
        }
        else
        {
            if (_currentRetries < _retries)
            {
                _currentRetries++;
            }
        }


        String delayStr = _brokerDetail.getProperty(BrokerDetails.OPTIONS_CONNECT_DELAY);
        if (delayStr != null && _currentRetries > 0)
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
            _logger.info("No delay between connect retries, use tcp://host:port?connectdelay='value' to enable.");
        }

        return _brokerDetail;
    }

    public void setBroker(BrokerDetails broker)
    {
        if (broker == null)
        {
            throw new IllegalArgumentException("BrokerDetails details cannot be null");
        }
        _brokerDetail = broker;

        String retries = broker.getProperty(BrokerDetails.OPTIONS_RETRY);
        if (retries != null)
        {
            try
            {
                _retries = Integer.parseInt(retries);
            }
            catch (NumberFormatException nfe)
            {
                _retries = DEFAULT_SERVER_RETRIES;
            }
        }
        else
        {
            _retries = DEFAULT_SERVER_RETRIES;
        }

        reset();
    }

    public void setRetries(int retries)
    {
        _retries = retries;
    }

    public String methodName()
    {
        return "Single Server";
    }

    public String toString()
    {
        return methodName()+":\n" +
               "Max Retries:" + _retries +
               "\nCurrent Retry:" + _currentRetries +
               "\n" + _brokerDetail + "\n";
    }

}
