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
 * AMQUnresolvedAddressException indicates failure to resolve a socket address.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represents failre to resolve a socket address.
 * </table>
 *
 * @todo Not an AMQP exception as no status code.
 *
 * @todo Why replace java.nio.UnresolvedAddressException with this? This is checked, which may explain why, but it
 *       doesn't wrap the underlying exception.
 */
public class AMQUnresolvedAddressException extends AMQException
{
    String _broker;

    public AMQUnresolvedAddressException(String message, String broker, Throwable cause)
    {
        super(null, message, cause);
        _broker = broker;
    }

    public String toString()
    {
        return super.toString() + " Broker, \"" + _broker + "\"";
    }
}
