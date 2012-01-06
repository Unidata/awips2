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

import javax.jms.ResourceAllocationException;

/**
 * Indicates that the maximum number of sessions per connection limit has been reached.
 */
public class ChannelLimitReachedException extends ResourceAllocationException
{
    private static final String ERROR_CODE = "1";

    private long _limit;

    public ChannelLimitReachedException(long limit)
    {
        super("Unable to create session since maximum number of sessions per connection is " +
              limit + ". Either close one or more sessions or increase the " +
              "maximum number of sessions per connection (or contact your AMQP administrator.", ERROR_CODE);
        _limit = limit;
    }

    public long getLimit()
    {
        return _limit;
    }
}
