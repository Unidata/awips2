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
using System.Runtime.Serialization;
using System.Text;
using Apache.Qpid.Framing;
using Apache.Qpid.Messaging;
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Client.Message
{
    [Serializable]
    class MessageEOFException : QpidException
    {
        public MessageEOFException(string message) : base(message)
        {
        }

        protected MessageEOFException(SerializationInfo info, StreamingContext ctxt)
           : base(info, ctxt)
        {
        }
    }

    public class QpidBytesMessage : AbstractQmsMessage, IBytesMessage
    {
        private const int DEFAULT_BUFFER_INITIAL_SIZE = 1024;

        public QpidBytesMessage() : this(null)
        {            
        }

        /// <summary>
        /// Construct a bytes message with existing data.
        /// </summary>
        /// <param name="data">if data is not null, the message is immediately in read only mode. if data is null, it is in
        /// write-only mode</param>        
        QpidBytesMessage(ByteBuffer data) : base(data)
        {
            // superclass constructor has instantiated a content header at this point
            if (data == null)
            {
                _data = ByteBuffer.Allocate(DEFAULT_BUFFER_INITIAL_SIZE);
                _data.IsAutoExpand = true;
            }
        }

        internal QpidBytesMessage(long messageNbr, ContentHeaderBody contentHeader, ByteBuffer data)
            // TODO: this casting is ugly. Need to review whole ContentHeaderBody idea
            : base(messageNbr, (BasicContentHeaderProperties)contentHeader.Properties, data)
        {
        }

        public override void ClearBodyImpl()
        {
            _data.Clear();
        }

        public override string ToBodyString()
        {
            CheckReadable();
            try
            {
                return GetText();
            }
            catch (IOException e)
            {
                throw new QpidException(e.ToString());
            }
        }

        private String GetText()
        {
            // this will use the default platform encoding
            if (_data == null)
            {
                return null;
            }
            int pos = _data.Position;
            _data.Rewind();
            // one byte left is for the end of frame marker
            if (_data.Remaining == 0)
            {
                // this is really redundant since pos must be zero
                _data.Position = pos;
                return null;
            }
            else
            {
                byte[] data = new byte[_data.Remaining];
                _data.GetBytes(data);
                return Encoding.UTF8.GetString(data);
            }
        }

        public long BodyLength
        {
            get
            {
                CheckReadable();
                return _data.Limit;
            }
        }

        private void CheckWritable()
        {
            if (_readableMessage)
            {
                throw new MessageNotWriteableException("You need to call clearBody() to make the message writable");
            }
        }

        public bool ReadBoolean()
        {
            CheckReadable();
            CheckAvailable(1);
            return _data.GetByte() != 0;
        }

        public byte ReadByte()
        {
            CheckReadable();
            CheckAvailable(1);
            return _data.GetByte();
        }

        public short ReadSignedByte()
        {
            CheckReadable();
            CheckAvailable(1);
            return _data.GetSByte();
        }

        public short ReadShort()
        {
            CheckReadable();
            CheckAvailable(2);
            return _data.GetInt16();
        }

        public char ReadChar()
        {
            CheckReadable();
            CheckAvailable(2);
            return _data.GetChar();
        }

        public int ReadInt()
        {
            CheckReadable();
            CheckAvailable(4);
            return _data.GetInt32();
        }

        public long ReadLong()
        {
            CheckReadable();
            CheckAvailable(8);
            return _data.GetInt64();
        }

        public float ReadFloat()
        {
            CheckReadable();
            CheckAvailable(4);
            return _data.GetFloat();
        }

        public double ReadDouble()
        {
            CheckReadable();
            CheckAvailable(8);
            return _data.GetDouble();
        }

        public string ReadUTF()
        {
            CheckReadable();
            // we check only for one byte since theoretically the string could be only a
            // single byte when using UTF-8 encoding
            CheckAvailable(1);
            try
            {
                byte[] data = new byte[_data.Remaining];
                _data.GetBytes(data);
                return Encoding.UTF8.GetString(data);                
            }
            catch (IOException e)
            {
                throw new QpidException(e.ToString(), e);
            }
        }

        public int ReadBytes(byte[] bytes)
        {
            if (bytes == null)
            {
                throw new ArgumentNullException("bytes");
            }
            CheckReadable();
            int count = (_data.Remaining >= bytes.Length ? bytes.Length : _data.Remaining);
            if (count == 0)
            {
                return -1;
            }
            else
            {
                _data.GetBytes(bytes, 0, count);
                return count;
            }
        }

        public int ReadBytes(byte[] bytes, int maxLength)
        {
            if (bytes == null)
            {
                throw new ArgumentNullException("bytes");
            }
            if (maxLength > bytes.Length)
            {
                throw new ArgumentOutOfRangeException("maxLength must be >= 0");
            }
            CheckReadable();
            int count = (_data.Remaining >= maxLength ? maxLength : _data.Remaining);
            if (count == 0)
            {
                return -1;
            }
            else
            {
                _data.GetBytes(bytes, 0, count);
                return count;
            }
        }

        public void WriteBoolean(bool b)
        {
            CheckWritable();
            _data.Put(b ? (byte)1 : (byte)0);
        }

        public void WriteByte(byte b)
        {
            CheckWritable();
            _data.Put(b);
        }

        public void WriteShort(short i)
        {
            CheckWritable();
            _data.Put(i);
        }

        public void WriteChar(char c)
        {
            CheckWritable();
            _data.Put(c);
        }

        public void WriteSignedByte(short value)
        {
            CheckWritable();
            _data.Put(value);
        }

        public void WriteDouble(double value)
        {
            CheckWritable();
            _data.Put(value);
        }

        public void WriteFloat(float value)
        {
            CheckWritable();
            _data.Put(value);
        }

        public void WriteInt(int value)
        {
            CheckWritable();
            _data.Put(value);
        }

        public void WriteLong(long value)
        {
            CheckWritable();
            _data.Put(value);
        }        

        public void WriteUTF(string value)
        {
            CheckWritable();
            byte[] encodedData = Encoding.UTF8.GetBytes(value);
            _data.Put(encodedData);
        }

        public void WriteBytes(byte[] bytes)
        {
            CheckWritable();
            _data.Put(bytes);
        }

        public void WriteBytes(byte[] bytes, int offset, int length)
        {
            CheckWritable();
            _data.Put(bytes, offset, length);
        }

        protected override void Reset()
        {
            base.Reset();
            _data.Flip();
        }

        void IBytesMessage.Reset()
        {
            Reset();
        }

        /**
         * Check that there is at least a certain number of bytes available to read
         *
         * @param len the number of bytes
         * @throws MessageEOFException if there are less than len bytes available to read
         */
        private void CheckAvailable(int len)
        {
            if (_data.Remaining < len)
            {
                throw new MessageEOFException("Unable to read " + len + " bytes");
            }
        }
    }
}
