/*
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
*/

namespace Apache.Qpid.Test.Channel.WcfPerftest
{
    using System;
    using System.Collections;
    using System.IO;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.ServiceModel.Description;
    using System.Threading;
    using System.Text;
    using System.Xml;
    using Apache.Qpid.Channel;


    /// <summary>
    /// A sample interface for populating and extracting message body content.
    /// Just enough methods to handle basic Interop text and raw byte messages.
    /// </summary>
 
    
    public interface IRawBodyUtility
    {
        Message CreateMessage(byte[] body, int offset, int len);
        Message CreateMessage(byte[] body);
        byte[] GetBytes(Message m, byte[] recyclableBuffer);

        Message CreateMessage(string body);
        string GetText(Message m);
    }

    // an implementation of IRawBodyUtility that expects a RawMessageEncoder based channel

    public class RawEncoderUtility : IRawBodyUtility
    {
        public Message CreateMessage(byte[] body, int offset, int count)
        {
            return Message.CreateMessage(MessageVersion.None, "", new RawEncoderBodyWriter(body, offset, count));
        }

        public Message CreateMessage(byte[] body)
        {
            return CreateMessage(body, 0, body.Length);
        }

        public byte[] GetBytes(Message message, byte[] recyclableBuffer)
        {
            XmlDictionaryReader reader = message.GetReaderAtBodyContents();
            int length;

            while (!reader.HasValue)
            {
                reader.Read();
                if (reader.EOF)
                {
                    throw new InvalidDataException("empty XmlDictionaryReader");
                }
            }

            if (reader.TryGetBase64ContentLength(out length))
            {
                byte[] bytes = null;
                if (recyclableBuffer != null)
                {
                    if (recyclableBuffer.Length == length)
                    {
                        // reuse
                        bytes = recyclableBuffer;
                    }
                }

                if (bytes == null)
                {
                    bytes = new byte[length];
                }

                // this is the single copy mechanism from native to managed space with no intervening
                // buffers.  One could also write a method GetBytes(msg, myBuf, offset)...
                reader.ReadContentAsBase64(bytes, 0, length);
                reader.Close();
                return bytes;
            }
            else
            {
                // uses whatever default buffering mechanism is used by the base XmlDictionaryReader class
                return reader.ReadContentAsBase64();
            }
        }

        public Message CreateMessage(string body)
        {
            return Message.CreateMessage(MessageVersion.None, "", new RawEncoderBodyWriter(body));
        }

        public string GetText(Message message)
        {
            byte[] rawBuffer = GetBytes(message, null);
            return Encoding.UTF8.GetString(rawBuffer, 0, rawBuffer.Length);
        }

        internal class RawEncoderBodyWriter : BodyWriter
        {
            // works only with the Raw Encoder; the "body" is either a single string or byte[] segment
            String bodyAsString;
            byte[] bodyAsBytes;
            int offset;
            int count;

            public RawEncoderBodyWriter(string body)
                : base(false)               // isBuffered
            {
                this.bodyAsString = body;
            }

            public RawEncoderBodyWriter(byte[] body, int offset, int count)
                : base(false)               // isBuffered
            {
                this.bodyAsBytes = body;
                this.offset = offset;
                this.count = count;
            }

            protected override void OnWriteBodyContents(System.Xml.XmlDictionaryWriter writer)
            {
                // TODO:  RawMessageEncoder.StreamElementName should be public.
                writer.WriteStartElement("Binary");  // the expected Raw encoder "<Binary>" virtual xml tag

                if (bodyAsString != null)
                {
                    byte[] buf = Encoding.UTF8.GetBytes(bodyAsString);
                    writer.WriteBase64(buf, 0, buf.Length);
                }
                else
                {
                    writer.WriteBase64(this.bodyAsBytes, this.offset, this.count);
                }

                writer.WriteEndElement();
            }
        }
    }

}
