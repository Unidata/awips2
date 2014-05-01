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

    internal class RawXmlReader : XmlDictionaryReader
    {        
        ////this class presents a hardcoded XML InfoSet: "<rawtag>X</rawtag>" where X is the entire stream content

        private Stream stream;
        private bool closed;
        private bool streamOwner;
        private ReaderPosition position;
        private string contentAsBase64;
        private XmlNameTable xmlNameTable;
        private XmlDictionaryReaderQuotas readerQuotas;

        public RawXmlReader(Stream stream, XmlDictionaryReaderQuotas quotas, bool streamOwner)
        {
            this.stream = stream;
            this.streamOwner = streamOwner;
            if (quotas == null)
            {
                this.readerQuotas = new XmlDictionaryReaderQuotas();
            }
            else
            {
                this.readerQuotas = quotas;
            }
        }

        private enum ReaderPosition
        {
            None,
            StartElement,
            Content,
            EndElement,
            EOF
        }

        public override int AttributeCount
        {
            get { return 0; }
        }

        public override string BaseURI
        {
            get { return string.Empty; }
        }

        public override int Depth
        {
            get { return (this.position == ReaderPosition.Content) ? 1 : 0; }
        }

        public override bool EOF
        {
            get { return this.position == ReaderPosition.EOF; }
        }

        public override bool HasAttributes
        {
            get { return false; }
        }

        public override bool HasValue
        {
            get { return this.position == ReaderPosition.Content; }
        }

        public override bool IsEmptyElement
        {
            get { return false; }
        }

        public override string LocalName
        {
            get 
            {
                if (this.position == ReaderPosition.StartElement)
                {
                    return RawMessageEncoder.StreamElementName;
                }

                return null;
            }
        }

        public override string NamespaceURI
        {
            get { return string.Empty; }
        }

        public override XmlNameTable NameTable
        {
            get
            {
                if (this.xmlNameTable == null)
                {
                    this.xmlNameTable = new NameTable();
                    this.xmlNameTable.Add(RawMessageEncoder.StreamElementName);
                }

                return this.xmlNameTable;
            }
        }

        public override XmlNodeType NodeType
        {
            get
            {
                switch (this.position)
                {
                    case ReaderPosition.StartElement:
                        return XmlNodeType.Element;
                    case ReaderPosition.Content:
                        return XmlNodeType.Text;
                    case ReaderPosition.EndElement:
                        return XmlNodeType.EndElement;
                    default:
                        // and StreamPosition.EOF
                        return XmlNodeType.None;
                }
            }
        }

        public override string Prefix
        {
            get { return string.Empty; }
        }

        public override ReadState ReadState
        {
            get
            {
                switch (this.position)
                {
                    case ReaderPosition.None:
                        return ReadState.Initial;
                    case ReaderPosition.StartElement:
                    case ReaderPosition.Content:
                    case ReaderPosition.EndElement:
                        return ReadState.Interactive;
                    case ReaderPosition.EOF:
                        return ReadState.Closed;
                    default:
                        return ReadState.Error;
                }
            }
        }

        public override string Value
        {
            get
            {
                switch (this.position)
                {
                    case ReaderPosition.Content:
                        if (this.contentAsBase64 == null)
                        {
                            this.contentAsBase64 = Convert.ToBase64String(this.ReadContentAsBase64());
                        }

                        return this.contentAsBase64;

                    default:
                        return string.Empty;
                }
            }
        }

        public override void Close()
        {
            if (!this.closed)
            {
                this.closed = true;
                this.position = ReaderPosition.EOF;
                this.readerQuotas = null;
                if (this.streamOwner)
                {
                    this.stream.Close();
                }
            }
        }

        public override string GetAttribute(int i)
        {
            throw new ArgumentOutOfRangeException("i", i, "Argument not in set of valid values");
        }

        public override string GetAttribute(string name, string namespaceURI)
        {
            return null;
        }

        public override string GetAttribute(string name)
        {
            return null;
        }

        public override string LookupNamespace(string prefix)
        {
            if (prefix == string.Empty)
            {
                return string.Empty;
            }
            else if (prefix == "xml")
            {
                return "http://www.w3.org/XML/1998/namespace";
            }
            else if (prefix == "xmlns")
            {
                return "http://www.w3.org/2000/xmlns/";
            }
            else
            {
                return null;
            }
        }

        public override bool MoveToAttribute(string name, string ns)
        {
            return false;
        }

        public override bool MoveToAttribute(string name)
        {
            return false;
        }

        public override bool MoveToElement()
        {
            if (this.position == ReaderPosition.None)
            {
                this.position = ReaderPosition.StartElement;
                return true;
            }

            return false;
        }

        public override bool MoveToFirstAttribute()
        {
            return false;
        }

        public override bool MoveToNextAttribute()
        {
            return false;
        }

        public override bool Read()
        {
            switch (this.position)
            {
                case ReaderPosition.None:
                    this.position = ReaderPosition.StartElement;
                    return true;
                case ReaderPosition.StartElement:
                    this.position = ReaderPosition.Content;
                    return true;
                case ReaderPosition.Content:
                    this.position = ReaderPosition.EndElement;
                    return true;
                case ReaderPosition.EndElement:
                    this.position = ReaderPosition.EOF;
                    return false;
                case ReaderPosition.EOF:
                    return false;
                default:
                    return false;
            }
        }

        public override bool ReadAttributeValue()
        {
            return false;
        }

        public override int ReadContentAsBase64(byte[] buffer, int index, int count)
        {
            if (buffer == null)
            {
                throw new ArgumentNullException("buffer");
            }

            if (this.position != ReaderPosition.Content)
            {
                throw new InvalidOperationException("XML reader not in Element");
            }

            if (count == 0)
            {
                return 0;
            }

            int readCount = this.stream.Read(buffer, index, count);
            if (readCount == 0)
            {
                this.position = ReaderPosition.EndElement;
            }

            return readCount;
        }

        public override int ReadContentAsBinHex(byte[] buffer, int index, int count)
        {
            throw new NotSupportedException();
        }

        public override void ResolveEntity()
        {
            throw new NotSupportedException();
        }

        public override bool TryGetBase64ContentLength(out int length)
        {
            // The whole stream is this one element
            if (!this.closed && this.stream.CanSeek)
            {
                long streamLength = this.stream.Length;
                if (streamLength <= int.MaxValue)
                {
                    length = (int)streamLength;
                    return true;
                }
            }

            length = -1;
            return false;
        }
    }
}
