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
using System.Text;
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Framing
{
    public class CompositeAMQDataBlock : IDataBlock, IEncodableAMQDataBlock
    {
        private IDataBlock[] _blocks;

        public CompositeAMQDataBlock(IDataBlock[] blocks)
        {
            _blocks = blocks;
        }

        public IDataBlock[] Blocks
        {
            get
            {
                return _blocks;
            }
        }

        public uint Size
        {
            get
            {
                uint frameSize = 0;
                foreach (IDataBlock block in _blocks)
                {
                    frameSize += block.Size;
                }
                return frameSize;
            }
        }

        public void WritePayload(ByteBuffer buffer)
        {
            foreach (IDataBlock block in _blocks)
            {
                block.WritePayload(buffer);
            }
        }

        public override string ToString()
        {
            if (_blocks == null)
            {
                return "No blocks contained in composite frame";
            }
            else
            {
                StringBuilder buf = new StringBuilder(GetType().Name);
                buf.Append("{");
                //buf.Append("encodedBlock=").Append(_encodedBlock);
                for (int i = 0; i < _blocks.Length; i++)
                {
                    buf.Append(" ").Append(i).Append("=[").Append(_blocks[i].ToString()).Append("]");
                }
                buf.Append("}");
                return buf.ToString();
            }
        }

    }
}
