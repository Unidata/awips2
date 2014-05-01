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
package org.apache.qpid.transport.util;

import org.slf4j.LoggerFactory;

/**
 * Logger
 *
 */

public final class Logger
{

    public static final Logger get(Class<?> klass)
    {
        return new Logger(LoggerFactory.getLogger(klass));
    }

    private final org.slf4j.Logger log;

    private Logger(org.slf4j.Logger log)
    {
        this.log = log;
    }

    public boolean isDebugEnabled()
    {
        return log.isDebugEnabled();
    }

    public void debug(String message, Object ... args)
    {
        if (log.isDebugEnabled())
        {
            log.debug(String.format(message, args));
        }
    }

    public void debug(Throwable t, String message, Object ... args)
    {
        if (log.isDebugEnabled())
        {
            log.debug(String.format(message, args), t);
        }
    }

    public void error(String message, Object ... args)
    {
        if (log.isErrorEnabled())
        {
            log.error(String.format(message, args));
        }
    }

    public void error(Throwable t, String message, Object ... args)
    {
        if (log.isErrorEnabled())
        {
            log.error(String.format(message, args), t);
        }
    }

    public void warn(String message, Object ... args)
    {
        if (log.isWarnEnabled())
        {
            log.warn(String.format(message, args));
        }
    }

    public void warn(Throwable t, String message, Object ... args)
    {
        if (log.isWarnEnabled())
        {
            log.warn(String.format(message, args), t);
        }
    }

    public void info(String message, Object ... args)
    {
        if (log.isInfoEnabled())
        {
            log.info(String.format(message, args));
        }
    }

    public void info(Throwable t, String message, Object ... args)
    {
        if (log.isInfoEnabled())
        {
            log.info(String.format(message, args), t);
        }
    }

    public void trace(String message, Object ... args)
    {
        if (log.isTraceEnabled())
        {
            log.trace(String.format(message, args));
        }
    }

    public void trace(Throwable t, String message, Object ... args)
    {
        if (log.isTraceEnabled())
        {
            log.trace(String.format(message, args), t);
        }
    }

}
