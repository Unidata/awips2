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

import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;

/**
 * AMQTransportConnectionException indicates a failure to establish a connection through the transporting medium, to
 * an AMQP broker.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represent failure to connect through the transport medium.
 * </table>
 *
 * @todo Error code never used. This is not an AMQException.
 */
public class AMQTransportConnectionException extends AMQException
{
    public AMQTransportConnectionException(AMQConstant errorCode, String message, Throwable cause)
    {
        super(errorCode, message, cause);
    }
}
