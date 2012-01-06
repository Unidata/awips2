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

namespace Apache.Qpid.Client.State
{
    [Serializable]
    public class IllegalStateTransitionException : AMQException
    {
        private AMQState _originalState;

        private Type _frame;

        public IllegalStateTransitionException(AMQState originalState, Type frame)
            : base("No valid state transition defined for receiving frame " + frame +
                   " from state " + originalState)
        {            
            _originalState = originalState;
            _frame = frame;
        }

        protected IllegalStateTransitionException(SerializationInfo info, StreamingContext ctxt)
           : base(info, ctxt)
        {
           _originalState = (AMQState)info.GetValue("OriginalState", typeof(AMQState));
           _frame = (Type)info.GetValue("FrameType", typeof(Type));
        }

        public AMQState OriginalState            
        {
            get
            {
                return _originalState;
            }
        }

        public Type FrameType
        {
            get
            {
                return _frame;
            }
        }

        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
           base.GetObjectData(info, context);
           info.AddValue("OriginalState", OriginalState);
           info.AddValue("FrameType", FrameType);
        }
    }
}



