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
package org.apache.qpid.client;

import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;

/**
 * AMQAuthenticationException represents all failures to authenticate access to a broker.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represent failure to authenticate the client.
 * </table>
 *
 * @todo Will this alwyas have the same status code, NOT_ALLOWED 530? Might set this up to always use that code.
 */
public class AMQAuthenticationException extends AMQException
{
    public AMQAuthenticationException(AMQConstant error, String msg, Throwable cause)
    {
        super(error, msg, cause);
    }
}
