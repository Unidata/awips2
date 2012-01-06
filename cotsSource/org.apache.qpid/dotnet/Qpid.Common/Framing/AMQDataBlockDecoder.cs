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
using System.Collections;
using log4net;
using Apache.Qpid.Buffer;
using Apache.Qpid.Codec;
using Apache.Qpid.Codec.Demux;

namespace Apache.Qpid.Framing
{
    public class AMQDataBlockDecoder : IMessageDecoder
    {
        private static ILog _logger = LogManager.GetLogger(typeof(AMQDataBlockDecoder));

        private Hashtable _supportedBodies = new Hashtable();

        private bool _disabled = false;

        public AMQDataBlockDecoder()
        {
            _supportedBodies[AMQMethodBody.TYPE] = AMQMethodBodyFactory.GetInstance();
            _supportedBodies[ContentHeaderBody.TYPE] = ContentHeaderBodyFactory.GetInstance();
            _supportedBodies[ContentBody.TYPE] = ContentBodyFactory.GetInstance();
            _supportedBodies[HeartbeatBody.TYPE] = new HeartbeatBodyFactory();
        }
        
        public MessageDecoderResult Decodable(ByteBuffer input)
        {
            if (_disabled)
            {
                return MessageDecoderResult.NOT_OK;
            }
            // final +1 represents the command end which we know we must require even
            // if there is an empty body
            if (input.Remaining < 1)
            {
                return MessageDecoderResult.NEED_DATA;
            }
            byte type = input.GetByte();

            // we have to check this isn't a protocol initiation frame here - we can't tell later on and we end up
            // waiting for more data. This could be improved if MINA supported some kind of state awareness when decoding
            if ((char)type == 'A')
            {
                _logger.Error("Received what appears to be a protocol initiation frame");
                return MessageDecoderResult.NOT_OK;
            }
            // zero, channel, body size and end byte
            if (input.Remaining < (1 + 2 + 4 + 1))
            {
                return MessageDecoderResult.NEED_DATA;
            }

            int channel = input.GetUInt16();
            long bodySize = input.GetUInt32();            

            // bodySize can be zero
            if (type <= 0 || channel < 0 || bodySize < 0)
            {
                _logger.Error(String.Format("Error decoding frame: Type={0}, Channel={1}, BodySize={2}", type, channel, bodySize));                
                return MessageDecoderResult.NOT_OK;
            }

            if (input.Remaining < (bodySize + 1))
            {
                return MessageDecoderResult.NEED_DATA;
            }

            if (IsSupportedFrameType(type))
            {
                if (_logger.IsDebugEnabled)
                {
                    // we have read 7 bytes so far, so output 7 + bodysize + 1 (for end byte) to get complete data block size
                    // this logging statement is useful when looking at exactly what size of data is coming in/out
                    // the broker
                    _logger.Debug("Able to decode data block of size " + (bodySize + 8));
                }
                return MessageDecoderResult.OK;
            }
            else
            {
                return MessageDecoderResult.NOT_OK;
            }
        }

        private bool IsSupportedFrameType(byte frameType)
        {
            bool result = _supportedBodies.ContainsKey(frameType);

            if (!result)
            {
                _logger.Warn("AMQDataBlockDecoder does not handle frame type " + frameType);
            }

            return result;
        }

        protected Object CreateAndPopulateFrame(ByteBuffer input)
        {
            byte type = input.GetByte();
            ushort channel = input.GetUInt16();
            uint bodySize = input.GetUInt32();

            IBodyFactory bodyFactory = (IBodyFactory)_supportedBodies[type];
            if (bodyFactory == null)
            {
                throw new AMQFrameDecodingException("Unsupported body type: " + type);
            }
            AMQFrame frame = new AMQFrame();

            frame.PopulateFromBuffer(input, channel, bodySize, bodyFactory);

            byte marker = input.GetByte();
            if (marker != 0xCE) {
           		throw new FormatException("marker is not 0xCE"); 	
            }
            return frame;
        }

        public MessageDecoderResult Decode(ByteBuffer input, IProtocolDecoderOutput output)
        {

            output.Write(CreateAndPopulateFrame(input));

            return MessageDecoderResult.OK;
        }

        public bool Disabled
        {
            set
            {
                _disabled = value;
            }
        }
    }
}
