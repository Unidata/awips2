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
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport.codec
{
    /// <summary> 
    /// AbstractDecoder
    /// </summary>
    public abstract class AbstractDecoder : IDecoder
    {
        private readonly Dictionary<Binary, String> str8cache = new Dictionary<Binary, String>();

        protected abstract byte DoGet();

        protected abstract void DoGet(byte[] bytes);
        public abstract bool HasRemaining();

        protected byte Get()
        {
            return DoGet();
        }

        protected void Get(byte[] bytes)
        {
            DoGet(bytes);
        }

        protected Binary Get(int size)
        {
            byte[] bytes = new byte[size];
            Get(bytes);
            return new Binary(bytes);
        }

        protected short Uget()
        {
            return (short) (0xFF & Get());
        }

        public virtual short ReadUint8()
        {
            return Uget();
        }

        public abstract int ReadUint16();
       

        public abstract long ReadUint32();
      

        public int ReadSequenceNo()
        {
            return (int) ReadUint32();
        }

        public virtual long ReadUint64()
        {
            long l = 0;
            for (int i = 0; i < 8; i++)
            {
                l |= ((long) (0xFF & Get())) << (56 - i*8);
            }
            return l;
        }

        public abstract short ReadInt8();
        public abstract int ReadInt16();       
        public abstract long ReadInt32() ;
        public abstract long ReadInt64();     
        public abstract float ReadFloat() ;  
        public abstract double ReadDouble() ;          

        public long ReadDatetime()
        {
            return ReadUint64();
        }

        private static String Decode(byte[] bytes, int offset, int length, Encoding encoding)
        {
            return encoding.GetString(bytes, offset, length);
        }

        private static String Decode(byte[] bytes, Encoding encoding)
        {
            return Decode(bytes, 0, bytes.Length, encoding);
        }

        public String ReadStr8()
        {
            short size = ReadUint8();
            Binary bin = Get(size);
            String str;
            if (! str8cache.TryGetValue(bin, out str))
            {
                str = Decode(bin.Array(), bin.Offset(), bin.Size(), Encoding.UTF8);
                str8cache.Add(bin, str);
            }
            return str;
        }

        public String ReadStr16()
        {
            int size = ReadUint16();
            byte[] bytes = new byte[size];
            Get(bytes);
            return Decode(bytes, Encoding.UTF8);
        }

        public byte[] ReadVbin8()
        {
            int size = ReadUint8();
            byte[] bytes = new byte[size];
            Get(bytes);
            return bytes;
        }

        public byte[] ReadVbin16()
        {
            int size = ReadUint16();
            byte[] bytes = new byte[size];
            Get(bytes);
            return bytes;
        }

        public byte[] ReadVbin32()
        {
            int size = (int) ReadUint32();
            byte[] bytes = new byte[size];
            Get(bytes);
            return bytes;
        }

        public RangeSet ReadSequenceSet()
        {
            int count = ReadUint16()/8;
            if (count == 0)
            {
                return null;
            }
            RangeSet ranges = new RangeSet();
            for (int i = 0; i < count; i++)
            {
                ranges.Add(ReadSequenceNo(), ReadSequenceNo());
            }
            return ranges;
        }

        public RangeSet ReadByteRanges()
        {
            throw new Exception("not implemented");
        }

        public UUID ReadUuid()
        {
            long msb = ReadUint64();
            long lsb = ReadUint64();
            return new UUID(msb, lsb);
        }

        public String ReadContent()
        {
            throw new Exception("Deprecated");
        }

        public Struct ReadStruct(int type)
        {
            Struct st = Struct.Create(type);
            int width = st.GetSizeWidth();
            if (width > 0)
            {
                long size = ReadSize(width);
                if (size == 0)
                {
                    return null;
                }
            }
            if (type > 0)
            {
                int code = ReadUint16();
                Debug.Assert(code == type);
            }
            st.Read(this);
            return st;
        }

        public Struct ReadStruct32()
        {
            long size = ReadUint32();
            if (size == 0)
            {
                return null;
            }
            int type = ReadUint16();
            Struct result = Struct.Create(type);
            result.Read(this);
            return result;
        }

        public Dictionary<String, Object> ReadMap()
        {
            long size = ReadUint32();

            if (size == 0)
            {
                return null;
            }

            long count = ReadUint32();

            Dictionary<String, Object> result = new Dictionary<String, Object>();
            for (int i = 0; i < count; i++)
            {
                String key = ReadStr8();
                byte code = Get();
                QpidType t = GetType(code);
                Object value = Read(t);
                result.Add(key, value);
            }

            return result;
        }

        public List<Object> ReadList()
        {
            long size = ReadUint32();

            if (size == 0)
            {
                return null;
            }

            long count = ReadUint32();

            List<Object> result = new List<Object>();
            for (int i = 0; i < count; i++)
            {
                byte code = Get();
                QpidType t = GetType(code);
                Object value = Read(t);
                result.Add(value);
            }
            return result;
        }

        public List<Object> ReadArray()
        {
            long size = ReadUint32();

            if (size == 0)
            {
                return null;
            }

            byte code = Get();
            QpidType t = GetType(code);
            long count = ReadUint32();

            List<Object> result = new List<Object>();
            for (int i = 0; i < count; i++)
            {
                Object value = Read(t);
                result.Add(value);
            }
            return result;
        }

        private QpidType GetType(byte code)
        {
            return QpidType.get(code);
        }

        private long ReadSize(QpidType t)
        {
            return t.Fixed ? t.Width : ReadSize(t.Width);
        }

        private long ReadSize(int width)
        {
            switch (width)
            {
                case 1:
                    return ReadUint8();
                case 2:
                    return ReadUint16();
                case 4:
                    return ReadUint32();
                default:
                    throw new Exception("illegal width: " + width);
            }
        }

        private byte[] ReadBytes(QpidType t)
        {
            long size = ReadSize(t);
            byte[] result = new byte[(int) size];
            Get(result);
            return result;
        }

        private Object Read(QpidType t)
        {
            switch (t.Code)
            {
                case Code.BIN8:
                case Code.UINT8:
                    return ReadUint8();
                case Code.INT8:
                    return Get();
                case Code.CHAR:
                    return (char) Get();
                case Code.BOOLEAN:
                    return Get() > 0;

                case Code.BIN16:
                case Code.UINT16:
                    return ReadUint16();
                case Code.INT16:
                    return (short) ReadUint16();

                case Code.BIN32:
                case Code.UINT32:
                    return ReadUint32();

                case Code.CHAR_UTF32:
                case Code.INT32:
                    return (int) ReadUint32();

                case Code.FLOAT:                    
                    return  (float)BitConverter.Int64BitsToDouble(ReadUint32() << 32);
                           
                case Code.BIN64:
                case Code.UINT64:
                case Code.INT64:
                case Code.DATETIME:
                    return ReadUint64();

                case Code.DOUBLE:                   
                    return BitConverter.Int64BitsToDouble(ReadUint64());
                case Code.UUID:
                    return ReadUuid();
                case Code.STR8:
                    return ReadStr8();
                case Code.STR16:
                    return ReadStr16();
                case Code.STR8_LATIN:
                case Code.STR8_UTF16:
                case Code.STR16_LATIN:
                case Code.STR16_UTF16:
                    // XXX: need to do character conversion
                    return Encoding.UTF8.GetString(ReadBytes(t));

                case Code.MAP:
                    return ReadMap();
                case Code.LIST:
                    return ReadList();
                case Code.ARRAY:
                    return ReadArray();
                case Code.STRUCT32:
                    return ReadStruct32();

                case Code.BIN40:
                case Code.DEC32:
                case Code.BIN72:
                case Code.DEC64:
                    // XXX: what types are we supposed to use here?
                    return ReadBytes(t);

                case Code.VOID:
                    return null;

                default:
                    return ReadBytes(t);
            }
        }
    }
}
