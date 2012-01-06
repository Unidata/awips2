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

import org.apache.qpid.protocol.AMQConstant;

/**
 * AMQUndeliveredException indicates that a message, marked immediate or mandatory, could not be delivered.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represents failure to delivery a message that must be delivered.
 * </table>
 */
public class AMQUndeliveredException extends AMQException
{
    private Object _bounced;

    public AMQUndeliveredException(AMQConstant errorCode, String msg, Object bounced, Throwable cause)
    {
        super(errorCode, msg, cause);

        _bounced = bounced;
    }

    public Object getUndeliveredMessage()
    {
        return _bounced;
    }

    public boolean isHardError()
    {
        return false;
    }
    
}
