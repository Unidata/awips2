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
 * LogActor the entity that is stored as in a ThreadLocal and used to perform logging.
 *
 * The actor is responsible for formatting its display name for the log entry.
 *
 * The actor performs the requested logging.
 */
public interface LogActor
{
    /**
     * Logs the specified LogMessage about the LogSubject
     *
     * Currently logging has a global setting however this will later be revised and
     * as such the LogActor will need to take into consideration any new configuration
     * as a means of enabling the logging of LogActors and LogSubjects.
     *
     * @param subject The subject that is being logged
     * @param message The message to log
     */
    public void message(LogSubject subject, LogMessage message);

    /**
     * Logs the specified LogMessage against this actor
     *
     * Currently logging has a global setting however this will later be revised and
     * as such the LogActor will need to take into consideration any new configuration
     * as a means of enabling the logging of LogActors and LogSubjects.
     *
     * @param message The message to log
     */
    public void message(LogMessage message);

    /**
     *
     * @return the RootMessageLogger that is currently in use by this LogActor.
     */
    RootMessageLogger getRootMessageLogger();

    /**
     * 
     * @return the String representing this LogActor
     */
    public String getLogMessage();
}