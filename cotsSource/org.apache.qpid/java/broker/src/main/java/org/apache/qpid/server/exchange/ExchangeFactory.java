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

import java.util.Collection;

import org.apache.commons.configuration.Configuration;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.configuration.VirtualHostConfiguration;


public interface ExchangeFactory
{
    Exchange createExchange(AMQShortString exchange, AMQShortString type, boolean durable, boolean autoDelete,
                            int ticket)
            throws AMQException;

    void initialise(VirtualHostConfiguration hostConfig);

    Collection<ExchangeType<? extends Exchange>> getRegisteredTypes();

    Exchange createExchange(String exchange, String type, boolean durable, boolean autoDelete) throws AMQException;
}
