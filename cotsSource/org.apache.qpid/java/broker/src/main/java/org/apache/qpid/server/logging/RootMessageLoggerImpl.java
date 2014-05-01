/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.    
 *
 * 
 */
package org.apache.qpid.server.logging;

import org.apache.qpid.server.configuration.ServerConfiguration;

public class RootMessageLoggerImpl implements RootMessageLogger
{
    private boolean _enabled;

    RawMessageLogger _rawLogger;
    private static final String MESSAGE = "MESSAGE ";

    public RootMessageLoggerImpl(ServerConfiguration configuration, RawMessageLogger rawLogger)
    {
        _enabled = configuration.getStatusUpdatesEnabled();
        _rawLogger = rawLogger;
    }

    public boolean isMessageEnabled(LogActor actor, LogSubject subject)
    {
        return _enabled;
    }

    public boolean isMessageEnabled(LogActor actor)
    {
        return _enabled;
    }

    public void rawMessage(String message)
    {
        _rawLogger.rawMessage(MESSAGE + message);
    }

    public void rawMessage(String message, Throwable throwable)
    {
        _rawLogger.rawMessage(MESSAGE + message, throwable);
    }
}
