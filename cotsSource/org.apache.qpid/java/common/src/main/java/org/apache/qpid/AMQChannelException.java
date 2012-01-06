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

import org.apache.qpid.framing.*;
import org.apache.qpid.protocol.AMQConstant;

/**
 * AMQChannelException indicates that an error that requires the channel to be closed has occurred.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represents an error that rquires the channel to be closed.
 * </table>
 *
 * @todo Does this duplicate AMQChannelClosedException?
 */
public class AMQChannelException extends AMQException
{
    private final int _classId;
    private final int _methodId;
    /* AMQP version for which exception ocurred */
    private final byte major;
    private final byte minor;

    public AMQChannelException(AMQConstant errorCode, String msg, int classId, int methodId, byte major, byte minor,
        Throwable cause)
    {
        super(errorCode, msg, cause);
        _classId = classId;
        _methodId = methodId;
        this.major = major;
        this.minor = minor;
    }

    public AMQFrame getCloseFrame(int channel)
    {
        MethodRegistry reg = MethodRegistry.getMethodRegistry(new ProtocolVersion(major,minor));
        return new AMQFrame(channel, reg.createChannelCloseBody(getErrorCode() == null ? AMQConstant.INTERNAL_ERROR.getCode() : getErrorCode().getCode(), new AMQShortString(getMessage()),_classId,_methodId));
    }
}
