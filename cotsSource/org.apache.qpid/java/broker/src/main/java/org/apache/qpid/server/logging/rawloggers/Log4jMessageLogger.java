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
package org.apache.qpid.server.logging.rawloggers;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.qpid.server.logging.RawMessageLogger;

public class Log4jMessageLogger implements RawMessageLogger
{
    public static final String DEFAULT_LEVEL = "INFO";
    public static final String DEFAULT_LOGGER = "qpid.message";
    private Level _level;
    private Logger _rawMessageLogger;

    public Log4jMessageLogger()
    {
        this(DEFAULT_LEVEL, DEFAULT_LOGGER);
    }

    public Log4jMessageLogger(String level, String logger)
    {
        _level = Level.toLevel(level);

        _rawMessageLogger = Logger.getLogger(logger);
        _rawMessageLogger.setLevel(_level);
    }

    public void rawMessage(String message)
    {
        rawMessage(message, null);
    }

    public void rawMessage(String message, Throwable throwable)
    {
        _rawMessageLogger.log(_level, message, throwable);
    }
}
