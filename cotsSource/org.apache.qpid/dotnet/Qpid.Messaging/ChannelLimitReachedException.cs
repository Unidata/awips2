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

namespace Apache.Qpid.Messaging
{
    [Serializable]
    public class ChannelLimitReachedException : ResourceAllocationException
    {
        private long _limit;

        public ChannelLimitReachedException(long limit)
            : base("Unable to create session since maximum number of sessions per connection is " +
                   limit + ". Either close one or more sessions or increase the " +
                   "maximum number of sessions per connection (or contact your OpenAMQ administrator.")
        {            
            _limit = limit;
        }

        protected ChannelLimitReachedException(SerializationInfo info, StreamingContext ctxt)
           : base(info, ctxt)
        {
           _limit = info.GetInt64("Limit");
        }

        public long Limit
        {
            get
            {
                return _limit;
            }
        }

        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
           base.GetObjectData(info, context);
           info.AddValue("Limit", _limit);
        }
    }
}
