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
import org.apache.qpid.framing.AMQShortString;

import java.util.Collection;


public interface ExchangeRegistry extends MessageRouter
{
    void registerExchange(Exchange exchange) throws AMQException;

    /**
     * Unregister an exchange
     * @param name name of the exchange to delete
     * @param inUse if true, do NOT delete the exchange if it is in use (has queues bound to it)
     * @throws ExchangeInUseException when the exchange cannot be deleted because it is in use
     * @throws AMQException
     */
    void unregisterExchange(AMQShortString name, boolean inUse) throws ExchangeInUseException, AMQException;

    Exchange getExchange(AMQShortString name);

    void setDefaultExchange(Exchange exchange);

    Exchange getDefaultExchange();

    Collection<AMQShortString> getExchangeNames();

    void initialise() throws AMQException;

    Exchange getExchange(String exchangeName);

    void unregisterExchange(String exchange, boolean ifUnused)  throws ExchangeInUseException, AMQException;;
}
