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

/**
 * Extend the Single Server Model to gain retry functionality but once connected do not attempt to failover.
 */
public class NoFailover extends FailoverSingleServer
{
    private boolean _connected = false;

    public NoFailover(BrokerDetails brokerDetail)
    {
        super(brokerDetail);
    }

    public NoFailover(ConnectionURL connectionDetails)
    {
        super(connectionDetails);
    }

    @Override
    public void attainedConnection()
    {
        _connected=true;
        _currentRetries = _retries;
    }

    @Override
    public String methodName()
    {
        return "NoFailover";
    }

    @Override
    public String toString()
    {
        return super.toString() + (_connected ? "Connection attained." : "Never connected.") + "\n";
    }

}
