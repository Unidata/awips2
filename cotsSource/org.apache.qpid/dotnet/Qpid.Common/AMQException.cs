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
using System.Runtime.Serialization;
using log4net;

namespace Apache.Qpid
{
    /// <summary>
    /// The generic AMQ exception.
    /// </summary>
    [Serializable]
    public class AMQException : Exception
    {
        private int _errorCode;

        public AMQException(string message)
            : base(message)
        {
        }

        public AMQException(string message, Exception innerException)
            : base(message, innerException)
        {
        }

        public AMQException(int errorCode, string message)
            : base(message)
        {
            _errorCode = errorCode;
        }

        public AMQException(int errorCode, string message, Exception innerException)
            : base(message, innerException)
        {
            _errorCode = errorCode;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="T:AMQException"/> class, with a logger that will
        /// be used to output log information upon construction. This saves having to log separately.
        /// </summary>
        /// <param name="logger">The logger.</param>
        /// <param name="message">The message.</param>
        public AMQException(ILog logger, string message)
            : base(message)
        {
            logger.Error(message);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="T:AMQException"/> class, with a logger that will
        /// be used to output log information upon construction. This saves having to log separately.
        /// </summary>
        /// <param name="logger">The logger.</param>
        /// <param name="message">The message.</param>
        /// <param name="innerException">The root cause</param>
        public AMQException(ILog logger, string message, Exception innerException)
            : base(message, innerException)
        {
            logger.Error(message, innerException);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="T:AMQException"/> class, with a logger that will
        /// be used to output log information upon construction. This saves having to log separately.
        /// </summary>
        /// <param name="logger">The logger.</param>
        /// <param name="message">The message.</param>
        /// <param name="errorCode">The AMQ error code. See RFC 006 for details of error codes</param>
        public AMQException(ILog logger, int errorCode, string message)
            : this(errorCode, message)
        {
            logger.Error(message);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="T:AMQException"/> class, with a logger that will
        /// be used to output log information upon construction. This saves having to log separately.
        /// </summary>
        /// <param name="logger">The logger.</param>
        /// <param name="message">The message.</param>
        /// <param name="errorCode">The AMQ error code. See RFC 006 for details of error codes</param>
        /// <param name="innerException">The root cause</param>
        public AMQException(ILog logger, int errorCode, string message, Exception innerException)
            : this(errorCode, message, innerException)
        {
            logger.Error(message, innerException);
        }

        /// <summary>
        /// Serialization Constructor
        /// </summary>
        /// <param name="info">SerializationInfo object</param>
        /// <param name="ctxt">StreamingContext object</param>
        protected AMQException(SerializationInfo info, StreamingContext ctxt)
           : base(info, ctxt)
        {
           _errorCode = info.GetInt32("ErrorCode");
        }

        /// <summary>
        /// ISerializable implementation of GetObjectData()
        /// </summary>
        /// <param name="info">SerializationInfo object</param>
        /// <param name="ctxt">StreamingContext object</param>
        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            base.GetObjectData(info, context);
            info.AddValue("ErrorCode", _errorCode);
        }


        /// <summary>
        /// Gets or sets the error code. See RFC 006 for details of error codes.
        /// </summary>
        /// <value>The error code.</value>
        public int ErrorCode
        {
            get
            {
                return _errorCode;
            }
            set
            {
                _errorCode = value;
            }
        }

    }
}
