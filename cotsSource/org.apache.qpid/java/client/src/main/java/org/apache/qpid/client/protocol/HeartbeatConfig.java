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
package org.apache.qpid.client.protocol;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class HeartbeatConfig
{
    private static final Logger _logger = LoggerFactory.getLogger(HeartbeatConfig.class);
    static final HeartbeatConfig CONFIG = new HeartbeatConfig();

    /**
     * The factor used to get the timeout from the delay between heartbeats.
     */
    private float timeoutFactor = 2;

    HeartbeatConfig()
    {
        String property = System.getProperty("amqj.heartbeat.timeoutFactor");
        if (property != null)
        {
            try
            {
                timeoutFactor = Float.parseFloat(property);
            }
            catch (NumberFormatException e)
            {
                _logger.warn("Invalid timeout factor (amqj.heartbeat.timeoutFactor): " + property);
            }
        }
    }

    float getTimeoutFactor()
    {
        return timeoutFactor;
    }

    int getTimeout(int writeDelay)
    {
        return (int) (timeoutFactor * writeDelay);
    }
}
