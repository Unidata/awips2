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

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.jms.failover.FailoverExchangeMethod;
import org.apache.qpid.jms.failover.FailoverMethod;
import org.apache.qpid.jms.failover.FailoverRoundRobinServers;
import org.apache.qpid.jms.failover.FailoverSingleServer;
import org.apache.qpid.jms.failover.NoFailover;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FailoverPolicy
{
    private static final Logger _logger = LoggerFactory.getLogger(FailoverPolicy.class);

    private static final long MINUTE = 60000L;

    private static final long DEFAULT_METHOD_TIMEOUT = 1 * MINUTE;

    private FailoverMethod[] _methods = new FailoverMethod[1];

    private int _currentMethod;

    private int _methodsRetries;

    private int _currentRetry;

    private boolean _timing;

    private long _lastMethodTime;
    private long _lastFailTime;

    public FailoverPolicy(ConnectionURL connectionDetails, AMQConnection conn)
    {
        FailoverMethod method;

        // todo This should be integrated in to the connection url when it supports
        // multiple strategies.

        _methodsRetries = 0;

        if (connectionDetails.getFailoverMethod() == null)
        {
            if (connectionDetails.getBrokerCount() > 1)
            {
                method = new FailoverRoundRobinServers(connectionDetails);
            }
            else
            {
                method = new FailoverSingleServer(connectionDetails);
            }
        }
        else
        {
            String failoverMethod = connectionDetails.getFailoverMethod();

            /*
                        if (failoverMethod.equals(FailoverMethod.RANDOM))
                        {
                            //todo write a random connection Failover
                        }
             */
            if (failoverMethod.equals(FailoverMethod.SINGLE_BROKER))
            {
                method = new FailoverRoundRobinServers(connectionDetails);
            }
            else
            {
                if (failoverMethod.equals(FailoverMethod.ROUND_ROBIN))
                {
                    method = new FailoverRoundRobinServers(connectionDetails);
                }
                else if (failoverMethod.equals(FailoverMethod.FAILOVER_EXCHANGE))
                {
                    method = new FailoverExchangeMethod(connectionDetails, conn);
                }
                else if (failoverMethod.equals(FailoverMethod.NO_FAILOVER))
                {
                    method = new NoFailover(connectionDetails);
                }
                else
                {
                    try
                    {
                        Class[] constructorSpec = { ConnectionURL.class };
                        Object[] params = { connectionDetails };

                        method =
                            (FailoverMethod) ClassLoader.getSystemClassLoader().loadClass(failoverMethod)
                                                        .getConstructor(constructorSpec).newInstance(params);
                    }
                    catch (Exception cnfe)
                    {
                        throw new IllegalArgumentException("Unknown failover method:" + failoverMethod, cnfe);
                    }
                }
            }
        }

        if (method == null)
        {
            throw new IllegalArgumentException("Unknown failover method specified.");
        }

        reset();

        _methods[_currentMethod] = method;
    }

    public FailoverPolicy(FailoverMethod method)
    {
        this(method, 0);
    }

    public FailoverPolicy(FailoverMethod method, int retries)
    {
        _methodsRetries = retries;

        reset();

        _methods[_currentMethod] = method;
    }

    private void reset()
    {
        _currentMethod = 0;
        _currentRetry = 0;
        _timing = false;

    }

    public boolean failoverAllowed()
    {
        boolean failoverAllowed;

        if (_timing)
        {
            long now = System.currentTimeMillis();

            if ((now - _lastMethodTime) >= DEFAULT_METHOD_TIMEOUT)
            {
                _logger.info("Failover method timeout");
                _lastMethodTime = now;

                if (!nextMethod())
                {
                    return false;
                }

            }
            else
            {
                _lastMethodTime = now;
            }
        }
        else
        {
            _timing = true;
            _lastMethodTime = System.currentTimeMillis();
            _lastFailTime = _lastMethodTime;
        }

        if (_methods[_currentMethod].failoverAllowed())
        {
            failoverAllowed = true;
        }
        else
        {
            if (_currentMethod < (_methods.length - 1))
            {
                nextMethod();
                _logger.info("Changing method to " + _methods[_currentMethod].methodName());

                return failoverAllowed();
            }
            else
            {
                return cycleMethods();
            }
        }

        return failoverAllowed;
    }

    private boolean nextMethod()
    {
        if (_currentMethod < (_methods.length - 1))
        {
            _currentMethod++;
            _methods[_currentMethod].reset();

            return true;
        }
        else
        {
            return cycleMethods();
        }
    }

    private boolean cycleMethods()
    {
        if (_currentRetry < _methodsRetries)
        {
            _currentRetry++;

            _currentMethod = 0;

            _logger.info("Retrying methods starting with " + _methods[_currentMethod].methodName());
            _methods[_currentMethod].reset();

            return failoverAllowed();
        }
        else
        {
            _logger.debug("All failover methods exhausted");

            return false;
        }
    }

    /**
     * Notification that connection was successful.
     */
    public void attainedConnection()
    {
        _currentRetry = 0;

        _methods[_currentMethod].attainedConnection();

        _timing = false;
    }

    public BrokerDetails getCurrentBrokerDetails()
    {
        return _methods[_currentMethod].getCurrentBrokerDetails();
    }

    public BrokerDetails getNextBrokerDetails()
    {
        return _methods[_currentMethod].getNextBrokerDetails();
    }

    public void setBroker(BrokerDetails broker)
    {
        _methods[_currentMethod].setBroker(broker);
    }

    public void addMethod(FailoverMethod method)
    {
        int len = _methods.length + 1;
        FailoverMethod[] newMethods = new FailoverMethod[len];
        System.arraycopy(_methods, 0, newMethods, 0, _methods.length);
        int index = len - 1;
        newMethods[index] = method;
        _methods = newMethods;
    }

    public void setMethodRetries(int retries)
    {
        _methodsRetries = retries;
    }

    public FailoverMethod getCurrentMethod()
    {
        if ((_currentMethod >= 0) && (_currentMethod < (_methods.length)))
        {
            return _methods[_currentMethod];
        }
        else
        {
            return null;
        }
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer();

        sb.append("Failover Policy:\n");

        if (failoverAllowed())
        {
            sb.append("Failover allowed\n");
        }
        else
        {
            sb.append("Failover not allowed\n");
        }

        sb.append("Failover policy methods\n");
        for (int i = 0; i < _methods.length; i++)
        {

            if (i == _currentMethod)
            {
                sb.append(">");
            }

            sb.append(_methods[i].toString());
        }

        return sb.toString();
    }
}
