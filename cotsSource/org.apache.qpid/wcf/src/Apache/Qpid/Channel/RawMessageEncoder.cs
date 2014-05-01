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

namespace Apache.Qpid.Channel
{
    using System;
    using System.IO;
    using System.ServiceModel.Channels;
    using System.ServiceModel;
    using System.Xml;


    class RawMessageEncoder : MessageEncoder
    {
        public const string StreamElementName = "Binary";
	
        XmlDictionaryReaderQuotas readerQuotas;
        
        public RawMessageEncoder(XmlDictionaryReaderQuotas quotas)
        {
            this.readerQuotas = new XmlDictionaryReaderQuotas();
            if (quotas != null)
            {
                quotas.CopyTo(this.readerQuotas);
            }
        }

        public override string ContentType
        {
            get { return null; }
        }

        public override bool IsContentTypeSupported(string contentType)
        {
            return true;
        }

        public override string MediaType
        {
            get { return null; }
        }

        public override MessageVersion MessageVersion
        {
            get { return MessageVersion.None; }
        }

        public override Message ReadMessage(ArraySegment<byte> buffer, BufferManager bufferManager, string contentType)
        {
            RawMessage message = new RawMessage(buffer.Array, buffer.Offset, buffer.Count, bufferManager, readerQuotas);
            message.Properties.Encoder = this;
            return message;
        }

        public override Message ReadMessage(Stream stream, int maxSizeOfHeaders, string contentType)
        {
            RawMessage message = new RawMessage(stream, readerQuotas);
            message.Properties.Encoder = this;
            return message;
        }

        private void CheckType(XmlDictionaryReader reader, XmlNodeType type)
        {
            if (reader.NodeType != type)
            {
                throw new System.IO.InvalidDataException(String.Format("RawMessageEncoder xml check {0} type should be {1}", type, reader.NodeType));
            }
        }

        public override ArraySegment<byte> WriteMessage(Message message, int maxMessageSize, BufferManager bufferManager, int messageOffset)
        {
            MemoryStream tempStream = new MemoryStream();
            this.WriteMessage(message, tempStream);
            int len = messageOffset + (int)tempStream.Length;
            byte[] buf = bufferManager.TakeBuffer(len);
            MemoryStream targetStream = new MemoryStream(buf);
            if (messageOffset > 0)
            {
                targetStream.Seek(messageOffset, SeekOrigin.Begin);
            }

            tempStream.WriteTo(targetStream);
            targetStream.Close();

            return new ArraySegment<byte>(buf, messageOffset, len - messageOffset);
        }

        public override void WriteMessage(Message message, Stream stream)
        {
            using (XmlWriter writer = new RawXmlWriter(stream))
            {
                message.WriteMessage(writer);
                writer.Flush();
            }
        }
    }
}
