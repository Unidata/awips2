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
package org.apache.qpid.client.transport;

import org.apache.qpid.jms.BrokerDetails;

/**
 * AMQNoTransportForProtocolException represents a connection failure where there is no transport medium to connect
 * to the broker available.  This may be the case if their is a error in the connection url, or an unsupported transport
 * type is specified.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represent absence of a transport medium.
 * </table>
 *
 * @todo Error code never used. This is not an AMQException.
 */
public class AMQNoTransportForProtocolException extends AMQTransportConnectionException
{
    BrokerDetails _details;

    public AMQNoTransportForProtocolException(BrokerDetails details, String message, Throwable cause)
    {
        super(null, message, cause);

        _details = details;
    }

    public String toString()
    {
        if (_details != null)
        {
            return super.toString() + _details.toString();
        }
        else
        {
            return super.toString();
        }
    }
}
