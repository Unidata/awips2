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
using System.Configuration;
using System.Reflection;
using System.Xml;
using log4net;
using Apache.Qpid.Buffer;
using Apache.Qpid.Codec;
using Apache.Qpid.Codec.Demux;
using Apache.Qpid.Common;

namespace Apache.Qpid.Framing
{
    public class ProtocolInitiation : IDataBlock, IEncodableAMQDataBlock
    {
        private static readonly ILog _log = LogManager.GetLogger(typeof(ProtocolInitiation));

        public char[] Header = new char[]{'A','M','Q','P'};

        private const byte CURRENT_PROTOCOL_CLASS = 1;
        private const int CURRENT_PROTOCOL_INSTANCE = 1;
        // FIXME: Needs to be tweakable from GRM.dll.config file. i.e. Major version 7 or 8 + 
        // FIXME: a configuration item for avoiding Basic.Qos (for OpenAMQ compatibility)
        public static int CURRENT_PROTOCOL_VERSION_MAJOR = 8; // FIXME: put back to 7 for OpenAMQ!
        private const int CURRENT_PROTOCOL_VERSION_MINOR = 0;

        public byte ProtocolClass = CURRENT_PROTOCOL_CLASS;
        public byte ProtocolInstance = CURRENT_PROTOCOL_INSTANCE;
        public byte ProtocolMajor = (byte)CURRENT_PROTOCOL_VERSION_MAJOR;
        public byte ProtocolMinor = CURRENT_PROTOCOL_VERSION_MINOR;        

        static ProtocolInitiation()
        {
            AssemblySettings settings = new AssemblySettings();

            /*
            string openAMQ = settings["OpenAMQ1d4Compatibility"];
            if (openAMQ.Equals("true"))
            {
                _log.Warn("Starting in OpenAMQ-1.0d4 compatibility mode. ProtocolMajorVersion is 7 and Basic.Qos will not be sent.");                
                CURRENT_PROTOCOL_VERSION_MAJOR = 7;                
            }
            */
        }

        public uint Size
        {
            get
            {
                return 4 + 1 + 1 + 1 + 1;
            }
        }

        public void WritePayload(ByteBuffer buffer)
        {
            foreach (char c in Header)
            {
                buffer.Put((byte) c);
            }
            buffer.Put(ProtocolClass);
            buffer.Put(ProtocolInstance);
            buffer.Put(ProtocolMajor);
            buffer.Put(ProtocolMinor);
        }

        /// <summary>
        /// Populates from buffer.
        /// </summary>
        /// <param name="buffer">The buffer.</param>
        public void PopulateFromBuffer(ByteBuffer buffer)
        {
            throw new AMQException("Method not implemented");
        }

        public class Decoder : IMessageDecoder
        {
            private bool _disabled = false;

            public MessageDecoderResult Decodable(ByteBuffer inbuf)
            {
                if (_disabled)
                {
                    return MessageDecoderResult.NOT_OK;
                }
                if (inbuf.Remaining < 8)
                {
                    return MessageDecoderResult.NEED_DATA;
                }
                else
                {
                    char[] expected = new char[]{'A', 'M', 'Q', 'P'};
                    for (int i = 0; i < 4; i++)
                    {
                        if (((char) inbuf.GetByte()) != expected[i])
                        {
                            return MessageDecoderResult.NOT_OK;
                        }
                    }
                    return MessageDecoderResult.OK;
                }
            }

            /// <summary>
            /// Decodes the specified session.
            /// </summary>
            /// <param name="inbuf">The inbuf.</param>
            /// <param name="output">The protocol output.</param>
            /// <returns></returns>
            public MessageDecoderResult Decode(ByteBuffer inbuf, IProtocolDecoderOutput output)
            {
                byte[] header = new byte[4];
                inbuf.GetBytes(header);
                ProtocolInitiation pi = new ProtocolInitiation();
                pi.Header = new char[]{'A','M','Q','P'};
                pi.ProtocolClass = inbuf.GetByte();
                pi.ProtocolInstance = inbuf.GetByte();
                pi.ProtocolMajor = inbuf.GetByte();
                pi.ProtocolMinor = inbuf.GetByte();
                output.Write(pi);
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

        public override string ToString()
        {
            return String.Format("{0}{{Class={1} Instance={2} Major={3} Minor={4}}}",
                                 GetType().Name, ProtocolClass, ProtocolInstance, ProtocolMajor, ProtocolMinor);
        }
    }
}
