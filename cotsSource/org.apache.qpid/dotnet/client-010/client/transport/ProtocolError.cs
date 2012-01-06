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
using org.apache.qpid.transport.network;

namespace org.apache.qpid.transport
{
    /// <summary> 
    /// ProtocolError
    /// </summary>
    public sealed class ProtocolError : INetworkEvent, IProtocolEvent
    {
        private int channel;
        private byte track;
        private String format;
        private Object[] args;

        public ProtocolError(byte track, String format, params Object[] args)
        {
            this.track = track;
            this.format = format;
            this.args = args;
        }

        #region INetworkEvent Methods

        public void ProcessNetworkEvent(INetworkDelegate ndelegate)
        {
            ndelegate.Error(this);
        }

        #endregion

        #region IProtocolEvent Methods

        public int Channel
        {
            get { return channel; }
            set { channel = value; }
        }

        public byte EncodedTrack
        {
            get { return track; }
            set { throw new NotImplementedException(); }
        }

        public void ProcessProtocolEvent<C>(C context, IProtocolDelegate<C> protocoldelegate)
        {
            protocoldelegate.Error(context, this);
        }

        #endregion

        public String Message
        {
            get { return String.Format(format, args); }
        }


        public override String ToString()
        {
            return String.Format("protocol error: {0}", Message);
        }

    }
}
