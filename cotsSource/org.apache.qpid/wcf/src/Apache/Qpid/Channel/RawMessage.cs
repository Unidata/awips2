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
    using System.Xml;

    // This incoming Message is backed either by a Stream (bodyStream) or a byte array (bodyBytes).
    // If bodyBytes belongs to a BufferManager, we must return it when done.
    // The pay-off is OnGetReaderAtBodyContents().
    // Most of the complexity is dealing with the OnCreateBufferedCopy() machinery.
    internal class RawMessage : Message
    {
        private MessageHeaders headers;
        private MessageProperties properties;
        private XmlDictionaryReaderQuotas readerQuotas;
        private Stream bodyStream;
        private byte[] bodyBytes;
        private int index;
        private int count;
        private BufferManager bufferManager;

        public RawMessage(byte[] buffer, int index, int count, BufferManager bufferManager, XmlDictionaryReaderQuotas quotas)
        {
            // this constructor supports MessageEncoder.ReadMessage(ArraySegment<byte> b, BufferManager mgr, string contentType)
            if (quotas == null)
            {
                quotas = new XmlDictionaryReaderQuotas();
            }

            this.headers = new MessageHeaders(MessageVersion.None);
            this.properties = new MessageProperties();
            this.readerQuotas = quotas;
            this.bodyBytes = buffer;
            this.index = index;
            this.count = count;
            this.bufferManager = bufferManager;
        }

        public RawMessage(Stream stream, XmlDictionaryReaderQuotas quotas)
        {
            // this constructor supports MessageEncoder.ReadMessage(System.IO.Stream s, int max, string contentType)
            if (quotas == null)
            {
                quotas = new XmlDictionaryReaderQuotas();
            }

            this.headers = new MessageHeaders(MessageVersion.None);
            this.properties = new MessageProperties();
            this.bodyStream = stream;
        }

        public RawMessage(MessageHeaders headers, MessageProperties properties, byte[] bytes, int index, int count, XmlDictionaryReaderQuotas quotas)
        {
            // this constructor supports internal needs for CreateBufferedCopy().CreateMessage()
            this.headers = new MessageHeaders(headers);
            this.properties = new MessageProperties(properties);
            this.bodyBytes = bytes;
            this.index = index;
            this.count = count;
            this.readerQuotas = quotas;
        }

        public override MessageHeaders Headers
        {
            get
            {
                if (this.IsDisposed)
                {
                    throw new ObjectDisposedException("message");
                }

                return this.headers;
            }
        }

        public override bool IsEmpty
        {
            get
            {
                if (this.IsDisposed)
                {
                    throw new ObjectDisposedException("message");
                }

                return false;
            }
        }

        public override bool IsFault
        {
            get
            {
                if (this.IsDisposed)
                {
                    throw new ObjectDisposedException("message");
                }

                return false;
            }
        }

        public override MessageProperties Properties
        {
            get
            {
                if (this.IsDisposed)
                {
                    throw new ObjectDisposedException("message");
                }

                return this.properties;
            }
        }

        public override MessageVersion Version
        {
            get
            {
                if (this.IsDisposed)
                {
                    throw new ObjectDisposedException("message");
                }

                return MessageVersion.None;
            }
        }

        protected override void OnBodyToString(XmlDictionaryWriter writer)
        {
            if (this.bodyStream != null)
            {
                writer.WriteString("Stream");
            }
            else
            {
                writer.WriteStartElement(RawMessageEncoder.StreamElementName, string.Empty);
                writer.WriteBase64(this.bodyBytes, this.index, this.count);
                writer.WriteEndElement();
            }
        }

        protected override void OnClose()
        {
            Exception deferEx = null;
            try
            {
                base.OnClose();
            }
            catch (Exception e)
            {
                deferEx = e;
            }

            try
            {
                if (this.properties != null)
                {
                    this.properties.Dispose();
                }
            }
            catch (Exception e)
            {
                if (deferEx == null)
                {
                    deferEx = e;
                }
            }

            try
            {
                if (this.bufferManager != null)
                {
                    this.bufferManager.ReturnBuffer(this.bodyBytes);
                    this.bufferManager = null;
                }
            }
            catch (Exception e)
            {
                if (deferEx == null)
                {
                    deferEx = e;
                }
            }

            if (deferEx != null)
            {
                throw deferEx;
            }
        }

        protected override MessageBuffer OnCreateBufferedCopy(int maxBufferSize)
        {
            if (this.bodyStream != null)
            {
                int len = (int)this.bodyStream.Length;
                byte[] buf = new byte[len];
                this.bodyStream.Read(buf, 0, len);
                this.bodyStream = null;
                this.bodyBytes = buf;
                this.count = len;
                this.index = 0;
            }
            else
            {
                if (this.bufferManager != null)
                {
                    // we could take steps to share the buffer among copies and release the memory
                    // after the last user finishes by a reference count or such, but we are already
                    // far from the intended optimized use.  Make one GC managed memory copy that is
                    // shared by all.
                    byte[] buf = new byte[this.count];

                    Buffer.BlockCopy(this.bodyBytes, this.index, buf, 0, this.count);
                    this.bufferManager.ReturnBuffer(this.bodyBytes);
                    this.bufferManager = null;
                    this.bodyBytes = buf;
                    this.index = 0;
                }
            }
 
            return new RawMessageBuffer(this.headers, this.properties, this.bodyBytes, this.index, this.count, this.readerQuotas);
        }

        protected override XmlDictionaryReader OnGetReaderAtBodyContents()
        {
            Stream readerStream = null;
            bool ownsStream;

            if (this.bodyStream != null)
            {
                readerStream = this.bodyStream;
                ownsStream = false;
            }
            else
            {
                // create stream for duration of XmlReader.
                ownsStream = true;
                if (this.bufferManager != null)
                {
                    readerStream = new RawMemoryStream(this.bodyBytes, this.index, this.count, this.bufferManager);
                    this.bufferManager = null;
                }
                else
                {
                    readerStream = new MemoryStream(this.bodyBytes, this.index, this.count, false);
                }
            }
            
            return new RawXmlReader(readerStream, this.readerQuotas, ownsStream);
        }

        protected override void OnWriteBodyContents(XmlDictionaryWriter writer)
        {
            writer.WriteStartElement(RawMessageEncoder.StreamElementName, string.Empty);
            if (this.bodyStream != null)
            {
                int len = (int)this.bodyStream.Length;
                byte[] buf = new byte[len];
                this.bodyStream.Read(buf, 0, len);
                writer.WriteBase64(buf, 0, len);
            }
            else
            {
                writer.WriteBase64(this.bodyBytes, this.index, this.count);
            }

            writer.WriteEndElement();
        }

        private class RawMemoryStream : MemoryStream
        {
            private BufferManager bufferManager;
            private byte[] buffer;

            public RawMemoryStream(byte[] bytes, int index, int count, BufferManager mgr)
                : base(bytes, index, count, false)
            {
                this.bufferManager = mgr;
                this.buffer = bytes;
            }

            protected override void Dispose(bool disposing)
            {
                if (this.bufferManager != null)
                {
                    try
                    {
                        this.bufferManager.ReturnBuffer(this.buffer);
                    }
                    finally
                    {
                        this.bufferManager = null;
                        base.Dispose(disposing);
                    }
                }
            }
        }

        private class RawMessageBuffer : MessageBuffer
        {
            private bool closed;
            private MessageHeaders headers;
            private MessageProperties properties;
            private byte[] bodyBytes;
            private int index;
            private int count;
            private XmlDictionaryReaderQuotas readerQuotas;

            public RawMessageBuffer(MessageHeaders headers, MessageProperties properties, byte[] bytes, int index, int count, XmlDictionaryReaderQuotas quotas)
                : base()
            {
                this.headers = new MessageHeaders(headers);
                this.properties = new MessageProperties(properties);
                this.bodyBytes = bytes;
                this.index = index;
                this.count = count;
                this.readerQuotas = new XmlDictionaryReaderQuotas();
                quotas.CopyTo(this.readerQuotas);
            }

            public override int BufferSize
            {
                get { return this.count; }
            }

            public override void Close()
            {
                if (!this.closed)
                {
                    this.closed = true;
                    this.headers = null;
                    if (this.properties != null)
                    {
                        this.properties.Dispose();
                        this.properties = null;
                    }

                    this.bodyBytes = null;
                    this.readerQuotas = null;
                }
            }

            public override Message CreateMessage()
            {
                if (this.closed)
                {
                    throw new ObjectDisposedException("message");
                }

                return new RawMessage(this.headers, this.properties, this.bodyBytes, this.index, this.count, this.readerQuotas);
            }
        }
    }
}
