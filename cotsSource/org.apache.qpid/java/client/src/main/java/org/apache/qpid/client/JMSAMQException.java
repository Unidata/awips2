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

import javax.jms.JMSException;

/**
 * JMSException does not accept wrapped exceptions in its constructor. Presumably this is because it is a relatively old
 * Java exception class, before this was added as a default to Throwable. This exception class accepts wrapped exceptions
 * as well as error messages, through its constructor, but is a JMSException.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Accept wrapped exceptions as a JMSException.
 * </table>
 */
public class JMSAMQException extends JMSException
{
    /**
     * Creates a JMSException, wrapping another exception class.
     *
     * @param message The error message.
     * @param cause   The underlying exception that caused this one. May be null if none is to be set.
     */
    public JMSAMQException(String message, Exception cause)
    {
        super(message);

        if (cause != null)
        {
            setLinkedException(cause);
        }
    }

    /**
     * @param s The underlying exception.
     *
     * @deprecated Use the other constructor and write a helpfull message. This one will be deleted.
     */
    public JMSAMQException(AMQException s)
    {
        super(s.getMessage(), String.valueOf(s.getErrorCode()));
        setLinkedException(s);
    }
}
