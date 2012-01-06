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
    using System.Xml;

    internal sealed class RawXmlWriter : XmlDictionaryWriter
    {

        WriteState state;
        Stream stream;
        bool closed;
        bool rawWritingEnabled;

        public RawXmlWriter(Stream stream)
        {
            if (stream == null)
            {
                throw new ArgumentNullException("Stream");
            }

            this.stream = stream;
            this.state = WriteState.Start;
        }

        public override WriteState WriteState
        {
            get
            {
                return this.state;
            }
        }

        public override void Close()
        {
            if (!this.closed)
            {
                this.closed = true;
                this.state = WriteState.Closed;
                this.rawWritingEnabled = false;
            }
        }

        public override void Flush()
        {
            this.ThrowIfClosed();
            this.stream.Flush();
        }

        public override string LookupPrefix(string ns)
        {
            return null;
        }

        public override void WriteBase64(byte[] buffer, int index, int count)
        {
            if (buffer == null)
            {
                throw new ArgumentNullException("buffer");
            }

            ThrowIfClosed();

            if (!this.rawWritingEnabled)
            {
                throw new InvalidOperationException("XmlWriter not in Element");
            }

            this.stream.Write(buffer, index, count);
            this.state = WriteState.Content;
        }

        public override void WriteStartElement(string prefix, string localName, string ns)
        {
            ThrowIfClosed();
            if (this.state != WriteState.Start)
            {
                throw new InvalidOperationException("Start Element Already Called");
            }

            if (!string.IsNullOrEmpty(prefix) || !string.IsNullOrEmpty(ns) || localName != RawMessageEncoder.StreamElementName)
            {
                throw new XmlException("Wrong XML Start Element Name");
            }
            this.state = WriteState.Element;
            this.rawWritingEnabled = true;
        }

        public override void WriteEndElement()
        {
            ThrowIfClosed();
            if (!this.rawWritingEnabled)
            {
                throw new InvalidOperationException("Unexpected End Element");
            }
            this.rawWritingEnabled = false;
        }

        public override void WriteFullEndElement()
        {
            this.WriteEndElement();
        }

        public override void WriteEndDocument()
        {
            this.rawWritingEnabled = false;
            this.ThrowIfClosed();
        }

        public override void WriteStartDocument()
        {
            this.rawWritingEnabled = false;
            this.ThrowIfClosed();
        }

        public override void WriteStartDocument(bool standalone)
        {
            this.rawWritingEnabled = false;
            this.ThrowIfClosed();
        }

        private void ThrowIfClosed()
        {
            if (this.closed)
            {
                throw new InvalidOperationException("XML Writer closed");
            }
        }


        public override void WriteString(string text)
        {
            throw new NotSupportedException();
        }

        public override void WriteCData(string text)
        {
            throw new NotSupportedException();
        }

        public override void WriteCharEntity(char ch)
        {
            throw new NotSupportedException();
        }

        public override void WriteChars(char[] buffer, int index, int count)
        {
            throw new NotSupportedException();
        }

        public override void WriteComment(string text)
        {
            throw new NotSupportedException();
        }

        public override void WriteDocType(string name, string pubid, string sysid, string subset)
        {
            throw new NotSupportedException();
        }

        public override void WriteEndAttribute()
        {
            throw new NotSupportedException();
        }

        public override void WriteEntityRef(string name)
        {
            throw new NotSupportedException();
        }


        public override void WriteProcessingInstruction(string name, string text)
        {
            throw new NotSupportedException();
        }

        public override void WriteRaw(string data)
        {
            throw new NotSupportedException();
        }

        public override void WriteRaw(char[] buffer, int index, int count)
        {
            throw new NotSupportedException();
        }

        public override void WriteStartAttribute(string prefix, string localName, string ns)
        {
            throw new NotSupportedException();
        }

        public override void WriteSurrogateCharEntity(char lowChar, char highChar)
        {
            throw new NotSupportedException();
        }

        public override void WriteWhitespace(string ws)
        {
            throw new NotSupportedException();
        }
    }
}
