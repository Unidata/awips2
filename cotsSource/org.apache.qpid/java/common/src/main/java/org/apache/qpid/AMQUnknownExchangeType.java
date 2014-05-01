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

package org.apache.qpid;

/**
 * AMQUnknownExchangeType represents coding error where unknown exchange type requested from exchange factory.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represents unknown exchange type request.
 * <tr><td>
 *
 * @todo Not an AMQP exception as no status code.
 *
 * @todo Represent coding error, where unknown exchange type is requested by passing a string parameter. Use a type safe
 *       enum for the exchange type, or replace with IllegalArgumentException. Should be runtime.
 */
public class AMQUnknownExchangeType extends AMQException
{
    public AMQUnknownExchangeType(String message, Throwable cause)
    {
        super(null, message, cause);
    }
}
