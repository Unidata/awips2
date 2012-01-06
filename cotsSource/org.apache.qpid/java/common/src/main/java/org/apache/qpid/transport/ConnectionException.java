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
package org.apache.qpid.transport;


/**
 * ConnectionException
 *
 */

public class ConnectionException extends TransportException
{

    private ConnectionClose close;

    public ConnectionException(String message, ConnectionClose close, Throwable cause)
    {
        super(message, cause);
        this.close = close;
    }

    public ConnectionException(String message)
    {
        this(message, null, null);
    }

    public ConnectionException(String message, Throwable cause)
    {
        this(message, null, cause);
    }

    public ConnectionException(Throwable cause)
    {
        this(cause.getMessage(), null, cause);
    }

    public ConnectionException(ConnectionClose close)
    {
        this(close.getReplyText(), close, null);
    }

    public ConnectionClose getClose()
    {
        return close;
    }

    @Override public void rethrow()
    {
        throw new ConnectionException(getMessage(), close, this);
    }

}
