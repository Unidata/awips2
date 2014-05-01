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

/**
 * ExchangeInUseRegistry indicates that an exchange cannot be unregistered because it is currently being used.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represents failure to unregister exchange that is in use.
 * </table>
 *
 * @todo Not an AMQP exception as no status code.
 *
 * @todo This exception is not used. However, it is part of the ExchangeRegistry interface, and looks like code is
 *       going to need to be added to throw/deal with this. Alternatively ExchangeResitries may be able to handle the
 *       issue internally.
 */
public class ExchangeInUseException extends AMQException
{
    public ExchangeInUseException(String exchangeName)
    {
        super("Exchange " + exchangeName + " is currently in use");
    }
}
