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
package org.apache.qpid.server.exchange;

import org.apache.qpid.AMQException;
import org.apache.qpid.server.queue.IncomingMessage;

/**
 * Separated out from the ExchangeRegistry interface to allow components
 * that use only this part to have a dependency with a reduced footprint.
 *
 */
public interface MessageRouter
{
    /**
     * Routes content through exchanges, delivering it to 1 or more queues.
     * @param message the message to be routed
     *
     * @throws org.apache.qpid.AMQException if something goes wrong delivering data
     */
    void routeContent(IncomingMessage message) throws AMQException;
}
