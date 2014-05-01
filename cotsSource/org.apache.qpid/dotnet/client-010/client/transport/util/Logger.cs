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
using System;
using log4net;

namespace org.apache.qpid.transport.util
{

    /// <summary> Logger
    /// 
    /// </summary>

    public sealed class Logger
    {
        private readonly ILog log;

        public static Logger Get(Type type)
        {
            return new Logger(LogManager.GetLogger(type));
        }

        private Logger(ILog log)
        {
            this.log = log;
        }

        public bool IsDebugEnabled()
        {
            return log.IsDebugEnabled;
        }

        public void Debug(String message, params Object[] args)
        {
            if (log.IsDebugEnabled)
            {
                log.Debug(String.Format(message, args));
            }
        }

        public void Debug(Exception t, String message, params Object[] args)
        {
            if (log.IsDebugEnabled)
            {
                log.Debug(String.Format(message, args), t);
            }
        }

        public void Error(String message, params Object[] args)
        {
            if (log.IsErrorEnabled)
            {
                log.Error(String.Format(message, args));
            }
        }

        public void Error(Exception t, String message, params Object[] args)
        {
            if (log.IsErrorEnabled)
            {
                log.Error(String.Format(message, args), t);
            }
        }

        public void Warn(String message, params Object[] args)
        {
            if (log.IsWarnEnabled)
            {
                log.Warn(String.Format(message, args));
            }
        }

        public void Warn(Exception t, String message, params Object[] args)
        {
            if (log.IsWarnEnabled)
            {
                log.Warn(String.Format(message, args), t);
            }
        }

        public void Info(String message, params Object[] args)
        {
            if (log.IsInfoEnabled)
            {
                log.Info(String.Format(message, args));
            }
        }

        public void Info(Exception t, String message, params Object[] args)
        {
            if (log.IsInfoEnabled)
            {
                log.Info(String.Format(message, args), t);
            }
        }
    }
}
