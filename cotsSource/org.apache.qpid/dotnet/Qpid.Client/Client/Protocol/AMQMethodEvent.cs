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
using System.Text;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Protocol
{
    public class AMQMethodEvent
    {
        private AMQMethodBody _method;

        private ushort _channelId;

        private AMQProtocolSession _protocolSession;

        public AMQMethodEvent(ushort channelId, AMQMethodBody method, AMQProtocolSession protocolSession)
        {
            _channelId = channelId;
            _method = method;
            _protocolSession = protocolSession;
        }

        public AMQMethodBody Method
        {
            get
            {
                return _method;
            }
        }

        public ushort ChannelId
        {
            get
            {
                return _channelId;
            }
        }

        public AMQProtocolSession ProtocolSession
        {
            get
            {
                return _protocolSession;
            }            
        }

        public override String ToString()
        {
            StringBuilder buf = new StringBuilder("Method event: ");
            buf.Append("\nChannel id: ").Append(_channelId);
            buf.Append("\nMethod: ").Append(_method);
            return buf.ToString();
        }
    }
}


