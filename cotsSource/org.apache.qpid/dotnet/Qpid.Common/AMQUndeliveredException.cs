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

namespace Apache.Qpid
{
    /// <summary>
    /// Thrown when a message has been bounced by the broker, indicating it could not be delivered.
    /// </summary>
    [Serializable]
    public class AMQUndeliveredException : AMQException
    {
        // TODO: Warning, no guarantee that the value stored here is serializable!
        private object _bounced;
        
        public AMQUndeliveredException(int errorCode, string message, object bounced)
            : base(errorCode, message)
        {
            _bounced = bounced;
        }

        protected AMQUndeliveredException(SerializationInfo info, StreamingContext ctxt)
           : base(info, ctxt)
        {
           _bounced = info.GetValue("bounced", typeof(object));
        }

        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
           base.GetObjectData(info, context);
           info.AddValue("bounced", _bounced);
        }

        public object GetUndeliveredMessage()
        {
            return _bounced;
        }
    }
}
