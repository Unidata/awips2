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
using System.IO;
using System.Text;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport.codec
{
    /// <summary> 
    /// AbstractEncoder
    /// </summary>
    public abstract class AbstractEncoder : IEncoder
    {
        private static readonly Dictionary<Type, Code> ENCODINGS = new Dictionary<Type, Code>();
        private readonly Dictionary<String, byte[]> str8cache = new Dictionary<String, byte[]>();

        static AbstractEncoder()
        {
            ENCODINGS.Add(typeof (Boolean), Code.BOOLEAN);
            ENCODINGS.Add(typeof (String), Code.STR16);
            ENCODINGS.Add(typeof (long), Code.INT64);
            ENCODINGS.Add(typeof (int), Code.INT32);
            ENCODINGS.Add(typeof (short), Code.INT16);
            ENCODINGS.Add(typeof (Byte), Code.INT8);
            ENCODINGS.Add(typeof (Dictionary<String, Object>), Code.MAP);
            ENCODINGS.Add(typeof (List<Object>), Code.LIST);
            ENCODINGS.Add(typeof (float), Code.FLOAT);
            ENCODINGS.Add(typeof (Double), Code.DOUBLE);
            ENCODINGS.Add(typeof (char), Code.CHAR);
            ENCODINGS.Add(typeof (byte[]), Code.VBIN32);
            ENCODINGS.Add(typeof (UUID), Code.UUID);
        }

        protected abstract void DoPut(byte b);

        protected abstract void DoPut(MemoryStream src);


        protected void Put(byte b)
        {
            DoPut(b);
        }

        protected void Put(MemoryStream src)
        {
            DoPut(src);
        }

        protected virtual void Put(byte[] bytes)
        {
            Put(new MemoryStream(bytes));
        }

        protected abstract int BeginSize8();
        protected abstract void EndSize8(int pos);

        protected abstract int BeginSize16();
        protected abstract void EndSize16(int pos);

        protected abstract int BeginSize32();
        protected abstract void EndSize32(int pos);

        public virtual void WriteUint8(short b)
        {
            Debug.Assert(b < 0x100);
            Put((byte) b);
        }

        public virtual void WriteUint16(int s)
        {
            Debug.Assert(s < 0x10000);
            Put((byte) Functions.Lsb(s >> 8));
            Put((byte) Functions.Lsb(s));
        }

        public virtual void WriteUint32(long i)
        {
            Debug.Assert(i < 0x100000000L);
            Put((byte) Functions.Lsb(i >> 24));
            Put((byte) Functions.Lsb(i >> 16));
            Put((byte) Functions.Lsb(i >> 8));
            Put((byte) Functions.Lsb(i));
        }

        public void WriteSequenceNo(int i)
        {
            WriteUint32(i);
        }

        public virtual void WriteUint64(long l)
        {
            for (int i = 0; i < 8; i++)
            {
                Put((byte) Functions.Lsb(l >> (56 - i*8)));
            }
        }

        public abstract void WriteInt8(short b) ;
        public abstract void WriteInt16(int s) ;
        public abstract void WriteInt32(long i) ;
        public abstract void WriteInt64(long l) ;
        public abstract void WriteFloat(float f) ;
        public abstract void WriteDouble(double d) ;        

        public void WriteDatetime(long l)
        {
            WriteUint64(l);
        }

        private static byte[] Encode(String s, Encoding encoding)
        {
            return encoding.GetBytes(s);
        }

        public void WriteStr8(String s)
        {
            if (s == null)
            {
                s = "";
            }

            byte[] bytes;
            if (! str8cache.ContainsKey(s))
            {
                bytes = Encode(s, System.Text.Encoding.UTF8);
                str8cache.Add(s, bytes);
            }
            else
            {
                bytes = str8cache[s];
            }
            WriteUint8((short) bytes.Length);
            Put(bytes);
        }

        public void WriteStr16(String s)
        {
            if (s == null)
            {
                s = "";
            }

            byte[] bytes = Encode(s, System.Text.Encoding.UTF8);
            WriteUint16(bytes.Length);
            Put(bytes);
        }

        public void WriteVbin8(byte[] bytes)
        {
            if (bytes == null)
            {
                bytes = new byte[0];
            }
            if (bytes.Length > 255)
            {
                throw new Exception("array too long: " + bytes.Length);
            }
            WriteUint8((short) bytes.Length);
            Put(bytes);
        }

        public void WriteVbin16(byte[] bytes)
        {
            if (bytes == null)
            {
                bytes = new byte[0];
            }
            WriteUint16(bytes.Length);
            Put(bytes);
        }

        public void WriteVbin32(byte[] bytes)
        {
            if (bytes == null)
            {
                bytes = new byte[0];
            }
            WriteUint32(bytes.Length);
            Put(bytes);
        }

        public void WriteSequenceSet(RangeSet ranges)
        {
            if (ranges == null)
            {
                WriteUint16(0);
            }
            else
            {
                WriteUint16(ranges.Size()*8);
                foreach (Range range in ranges)
                {
                    WriteSequenceNo(range.Lower);
                    WriteSequenceNo(range.Upper);
                }
            }
        }

        public void WriteByteRanges(RangeSet ranges)
        {
            throw new Exception("not implemented");
        }

        public void WriteUuid(UUID uuid)
        {
            long msb = 0;
            long lsb = 0;
            if (uuid != null)
            {
                msb = uuid.MostSignificantBits;
                lsb = uuid.LeastSignificantBits;
            }
            WriteUint64(msb);
            WriteUint64(lsb);
        }

        public void WriteStruct(int type, Struct s)
        {
            if (s == null)
            {
                s = Struct.Create(type);
            }

            int width = s.GetSizeWidth();
            int pos = -1;
            if (width > 0)
            {
                pos = BeginSize(width);
            }

            if (type > 0)
            {
                WriteUint16(type);
            }

            s.Write(this);

            if (width > 0)
            {
                EndSize(width, pos);
            }
        }

        public void WriteStruct32(Struct s)
        {
            if (s == null)
            {
                WriteUint32(0);
            }
            else
            {
                int pos = BeginSize32();
                WriteUint16(s.GetEncodedType());
                s.Write(this);
                EndSize32(pos);
            }
        }

        private Code Encoding(Object value)
        {
            if (value == null)
            {
                return Code.VOID;
            }

            Type klass = value.GetType();
            Code type = Resolve(klass);

            if (type == Code.VOID)
            {
                throw new Exception
                    ("unable to resolve type: " + klass + ", " + value);
            }
            else
            {
                return type;
            }
        }

        private static Code Resolve(Type klass)
        {
            Code type;
            if(ENCODINGS.ContainsKey(klass))
            {
                return ENCODINGS[klass];
            }
            
            Type sup = klass.BaseType;
            if (sup != null)
            {
                type = Resolve(sup);

                if (type != Code.VOID)
                {
                    return type;
                }
            }
            foreach (Type iface in klass.GetInterfaces())
            {
                type = Resolve(iface);
                if (type != Code.VOID)
                {
                    return type;
                }
            }
            return Code.VOID;
        }

        public void WriteMap(Dictionary<String, Object> map)
        {
            int pos = BeginSize32();
            if (map != null)
            {
                WriteUint32(map.Count);
                WriteMapEntries(map);
            }
            EndSize32(pos);
        }

        protected void WriteMapEntries(Dictionary<String, Object> map)
        {
            foreach (KeyValuePair<String, Object> entry in map)
            {
                String key = entry.Key;
                Object value = entry.Value;
                Code type = Encoding(value);
                WriteStr8(key);
                Put((byte) type);
                Write(type, value);
            }
        }

        public void WriteList(List<Object> list)
        {
            int pos = BeginSize32();
            if (list != null)
            {
                WriteUint32(list.Count);
                WriteListEntries(list);
            }
            EndSize32(pos);
        }

        protected void WriteListEntries(List<Object> list)
        {
            foreach (Object value in list)
            {
                Code type = Encoding(value);
                Put((byte) type);
                Write(type, value);
            }
        }

        public void WriteArray(List<Object> array)
        {
            int pos = BeginSize32();
            if (array != null)
            {
                WriteArrayEntries(array);
            }
            EndSize32(pos);
        }

        protected void WriteArrayEntries(List<Object> array)
        {
            Code type;

            if (array.Count == 0)
            {
                return;
            }
            else
            {
                type = Encoding(array[0]);
            }
            Put((byte) type);
            WriteUint32(array.Count);

            foreach (Object value in array)
            {
                Write(type, value);
            }
        }

        private void WriteSize(QpidType t, int size)
        {
            if (t.Fixed)
            {
                if (size != t.width)
                {
                    throw new Exception("size does not match fixed width " + t.width + ": " + size);
                }
            }
            else
            {
                WriteSize(t.width, size);
            }
        }

        private void WriteSize(int width, int size)
        {
            // XXX: should check lengths
            switch (width)
            {
                case 1:
                    WriteUint8((short) size);
                    break;
                case 2:
                    WriteUint16(size);
                    break;
                case 4:
                    WriteUint32(size);
                    break;
                default:
                    throw new Exception("illegal width: " + width);
            }
        }

        private int BeginSize(int width)
        {
            switch (width)
            {
                case 1:
                    return BeginSize8();
                case 2:
                    return BeginSize16();
                case 4:
                    return BeginSize32();
                default:
                    throw new Exception("illegal width: " + width);
            }
        }

        private void EndSize(int width, int pos)
        {
            switch (width)
            {
                case 1:
                    EndSize8(pos);
                    break;
                case 2:
                    EndSize16(pos);
                    break;
                case 4:
                    EndSize32(pos);
                    break;
                default:
                    throw new Exception("illegal width: " + width);
            }
        }

        private void WriteBytes(QpidType t, byte[] bytes)
        {
            WriteSize(t, bytes.Length);
            Put(bytes);
        }

        private void Write(Code t, Object value)
        {
            switch (t)
            {
                case Code.BIN8:
                case Code.UINT8:
                    WriteUint8((short) value);
                    break;
                case Code.INT8:
                    Put((Byte) value);
                    break;
                case Code.CHAR:
                    byte[] b = BitConverter.GetBytes((char) value);
                    Put(b[0]);
                    break;
                case Code.BOOLEAN:
                    if ((bool) value)
                    {
                        Put(1);
                    }
                    else
                    {
                        Put(0);
                    }

                    break;

                case Code.BIN16:
                case Code.UINT16:
                    WriteUint16((int) value);
                    break;

                case Code.INT16:
                    WriteUint16((short) value);
                    break;

                case Code.BIN32:
                case Code.UINT32:
                    WriteUint32((long) value);
                    break;

                case Code.CHAR_UTF32:
                case Code.INT32:
                    WriteUint32((int) value);
                    break;

                case Code.FLOAT:
                    WriteUint32(BitConverter.DoubleToInt64Bits((float) value) >> 32);
                    break;

                case Code.BIN64:
                case Code.UINT64:
                case Code.INT64:                   
                case Code.DATETIME:
                    WriteUint64((long) value);
                    break;

                case Code.DOUBLE:
                    WriteUint64( BitConverter.DoubleToInt64Bits((double) value));                    
                    break;

                case Code.UUID:
                    WriteUuid((UUID) value);
                    break;

                case Code.STR8:
                    WriteStr8((string) value);
                    break;

                case Code.STR16:
                    WriteStr16((string) value);
                    break;

                case Code.STR8_LATIN:
                case Code.STR8_UTF16:
                case Code.STR16_LATIN:
                case Code.STR16_UTF16:
                    // XXX: need to do character conversion
                    WriteBytes(QpidType.get((byte) t), Encode((string) value, System.Text.Encoding.Unicode));
                    break;

                case Code.MAP:
                    WriteMap((Dictionary<String, Object>) value);
                    break;
                case Code.LIST:
                    WriteList((List<Object>) value);
                    break;
                case Code.ARRAY:
                    WriteList((List<Object>) value);
                    break;
                case Code.STRUCT32:
                    WriteStruct32((Struct) value);
                    break;

                case Code.BIN40:
                case Code.DEC32:
                case Code.BIN72:
                case Code.DEC64:
                    // XXX: what types are we supposed to use here?
                    WriteBytes(QpidType.get((byte) t), (byte[]) value);
                    break;

                case Code.VOID:
                    break;

                default:
                    WriteBytes(QpidType.get((byte) t), (byte[]) value);
                    break;
            }
        }
    }
}
