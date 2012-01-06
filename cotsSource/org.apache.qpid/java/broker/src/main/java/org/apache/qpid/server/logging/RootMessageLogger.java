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

/**
 * The RootMessageLogger is used by the LogActors to query if
 * logging is enabled for the requested message and to provide the actual
 * message that should be logged.
 */
public interface RootMessageLogger
{
    /**
     * Determine if the LogSubject and the LogActor should be
     * generating log messages.
     *
     * @param subject The subject of this log request
     * @param actor   The actor requesting the logging
     * @return boolean true if the message should be logged.
     */
    boolean isMessageEnabled(LogActor actor, LogSubject subject);

    /**
     * Determine if  the LogActor should be generating log messages.
     *
     * @param actor   The actor requesting the logging
     *
     * @return boolean true if the message should be logged.
     */
    boolean isMessageEnabled(LogActor actor);

    /**
     * Log the raw message to the configured logger.
     *
     * @param message   The message to log
     */
    public void rawMessage(String message);

    /**
     * Log the raw message to the configured logger.
     * Along with a formated stack trace from the Throwable.
     *
     * @param message   The message to log
     * @param throwable Optional Throwable that should provide stact trace
     */
    void rawMessage(String message, Throwable throwable);
}