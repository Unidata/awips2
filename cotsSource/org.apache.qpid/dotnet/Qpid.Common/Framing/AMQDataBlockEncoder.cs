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
using System.Collections;
using log4net;
using Apache.Qpid.Buffer;
using Apache.Qpid.Codec;
using Apache.Qpid.Codec.Demux;

namespace Apache.Qpid.Framing
{
    public class AMQDataBlockEncoder : IMessageEncoder
    {
        private static ILog _logger = LogManager.GetLogger(typeof(AMQDataBlockEncoder));

        private Hashtable _messageTypes;
        
        public AMQDataBlockEncoder()
        {
            _messageTypes = new Hashtable();
            _messageTypes[typeof (IEncodableAMQDataBlock)] = 1;
        }


        public Hashtable MessageTypes
        {
            get
            {
                return _messageTypes;
            }
        }

        public void Encode(object message, IProtocolEncoderOutput output)
        {
            IDataBlock frame = (IDataBlock) message;
            int frameSize = (int)frame.Size; // TODO: sort out signed/unsigned
            ByteBuffer buffer = ByteBuffer.Allocate(frameSize);
            frame.WritePayload(buffer);
            
            if (_logger.IsDebugEnabled)
            {                
                _logger.Debug("Encoded frame byte-buffer is '" + ByteBufferHexDumper.GetHexDump(buffer) + "'");
            }
            buffer.Flip();
            output.Write(buffer);
        }
    }
}
