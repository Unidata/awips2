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
using Frame = org.apache.qpid.transport.network.Frame;

namespace org.apache.qpid.transport
{
    /// <summary> 
    /// Method
    /// </summary>
    public abstract class Method : Struct, IProtocolEvent
    {
        public new static Method Create(int type)
        {
            return (Method) StructFactory.createInstruction(type);
        }

        // XXX: command subclass?
        private int id;
        private int channel;
        private bool idSet;
        private bool sync;
        private bool batch;

        public int Id
        {
            get { return id; }
            set
            {
                id = value;
                idSet = true;
            }
        }


        public bool Sync
        {
            get { return sync; }
            set { sync = value; }
        }

        public bool Batch
        {
            get { return batch; }
            set { batch = value; }
        }

        public abstract bool HasPayload();

        public virtual Header Header
        {
            get { return null; }
            set { throw new Exception(); }
        }

        public virtual MemoryStream Body
        {
            get { return null; }
            set { throw new Exception(); }
        }


        public abstract void Dispatch<C>(C context, MethodDelegate<C> mdelegate );

        #region IProtocolEvent

        public int Channel
        {
            get { return channel; }
            set { channel = value; }
        }

        public abstract byte EncodedTrack { get; set; }

        public void ProcessProtocolEvent<C>(C context, IProtocolDelegate<C> protocoldelegate)
        {
            if (EncodedTrack == Frame.L4)
            {
                protocoldelegate.Command(context, this);
            }
            else
            {
                protocoldelegate.Control(context, this);
            }
        }

        #endregion

        public override String ToString()
        {
            StringBuilder str = new StringBuilder();

            str.Append("ch=");
            str.Append(channel);

            if (EncodedTrack == Frame.L4 && idSet)
            {
                str.Append(" id=");
                str.Append(id);
            }

            if (sync || batch)
            {
                str.Append(" ");
                str.Append("[");
                if (Sync)
                {
                    str.Append("S");
                }
                if (Batch)
                {
                    str.Append("B");
                }
                str.Append("]");
            }
            str.Append(" ");
            str.Append(base.ToString());
            if (Header != null)
            {
                str.Append(Header.ToString());
            }
            if (Body != null)
            {
                str.Append("\n  body=");
                str.Append(Body.ToString());
            }
            return str.ToString();
        }
    }
}
