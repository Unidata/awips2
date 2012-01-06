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
package org.apache.qpid.framing;

import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;

/**
 * AMQFrameDecodingException indicates that an AMQP frame cannot be decoded because it does not have the correct
 * format as defined by the protocol.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represents a format error in a protocol frame.
 * </table>
 */
public class AMQFrameDecodingException extends AMQException
{
    public AMQFrameDecodingException(AMQConstant errorCode, String message, Throwable cause)
    {
        super(errorCode, message, cause);
    }

    public AMQFrameDecodingException(AMQConstant errorCode, String message)
    {
        super(errorCode, message, null);
    }

}
