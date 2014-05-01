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
namespace Apache.Qpid.Client
{
    public class ConnectionTuneParameters
    {
        private uint _frameMax;

        private ushort _channelMax;

        private uint _hearbeat;

        private uint _txnLimit;

        public uint FrameMax
        {
            get
            {
                return _frameMax;
            }
            set
            {
                _frameMax = value;
            }
        }

        public ushort ChannelMax
        {
            get
            {
                return _channelMax;
            }
            set
            {
                _channelMax = value;
            }
        }

        public uint Heartbeat
        {
            get
            {
                return _hearbeat;
            }
            set
            {
                _hearbeat = value;
            }
        }

        public uint TxnLimit
        {
            get
            {
                return _txnLimit;
            }
            set
            {
                _txnLimit = value;
            }
        }
    }
}


