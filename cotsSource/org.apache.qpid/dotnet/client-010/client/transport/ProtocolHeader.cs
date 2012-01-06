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
using System.IO;
using System.Text;
using org.apache.qpid.transport.network;
using Frame = org.apache.qpid.transport.network.Frame;

namespace org.apache.qpid.transport
{
    /// <summary> ProtocolHeader
    /// 
    /// </summary>
    public sealed class ProtocolHeader : INetworkEvent, IProtocolEvent
    {
        private readonly char[] AMQP = new char[] {'A', 'M', 'Q', 'P'};
        private const byte CLASS = 1;

        private readonly byte instance;
        private readonly byte major;
        private readonly byte minor;
        private int channel;

        public ProtocolHeader(byte instance, byte major, byte minor)
        {
            this.instance = instance;
            this.major = major;
            this.minor = minor;
        }

        public ProtocolHeader(int instance, int major, int minor) : this((byte)instance, (byte)major, (byte)minor)
        {
        }

        #region INetworkEvent Methods

        public void ProcessNetworkEvent(INetworkDelegate ndelegate)
        {
            ndelegate.Init(this);
        }

        #endregion

        #region IProtocolEvent Methods

        public int Channel
        {
            get
            {
                return channel;
            }
            set
            {
                channel = value;
            }           
        }

        public byte EncodedTrack
        {
            get
            {
                return Frame.L1;
            }
            set { throw new NotImplementedException(); }
        }

        public void ProcessProtocolEvent<C>(C context, IProtocolDelegate<C> protocoldelegate)
        {
            protocoldelegate.Init(context, this);
        }

        #endregion

        public byte Instance
        {
            get { return instance; }
        }

        public byte Major
        {
            get { return major; }
        }

        public byte Minor
        {
            get { return minor; }
        }

        public MemoryStream ToMemoryStream()
        {
            MemoryStream buf = new MemoryStream(8);
            BinaryWriter writer = new BinaryWriter(buf);
            writer.Write(AMQP);
            writer.Write(CLASS);
            writer.Write(instance);
            writer.Write((sbyte) major);
            writer.Write((sbyte) minor);
            return buf;
        }

        public override String ToString()
        {
            return String.Format("AMQP.{0:d} {1:d}-{2:d}", instance, major, minor);
        }
    }
}
