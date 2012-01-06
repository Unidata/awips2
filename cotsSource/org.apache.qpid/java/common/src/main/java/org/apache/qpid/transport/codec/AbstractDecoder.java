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
package org.apache.qpid.transport.codec;

import java.io.UnsupportedEncodingException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.qpid.transport.Binary;
import org.apache.qpid.transport.RangeSet;
import org.apache.qpid.transport.Struct;
import org.apache.qpid.transport.Type;

import static org.apache.qpid.transport.util.Functions.*;


/**
 * AbstractDecoder
 *
 * @author Rafael H. Schloming
 */

abstract class AbstractDecoder implements Decoder
{

    private final Map<Binary,String> str8cache = new LinkedHashMap<Binary,String>()
    {
        @Override protected boolean removeEldestEntry(Map.Entry<Binary,String> me)
        {
            return size() > 4*1024;
        }
    };

    protected abstract byte doGet();

    protected abstract void doGet(byte[] bytes);

    protected byte get()
    {
        return doGet();
    }

    protected void get(byte[] bytes)
    {
        doGet(bytes);
    }

    protected Binary get(int size)
    {
        byte[] bytes = new byte[size];
        get(bytes);
        return new Binary(bytes);
    }

    protected short uget()
    {
        return (short) (0xFF & get());
    }

    public short readUint8()
    {
        return uget();
    }

    public int readUint16()
    {
        int i = uget() << 8;
        i |= uget();
        return i;
    }

    public long readUint32()
    {
        long l = uget() << 24;
        l |= uget() << 16;
        l |= uget() << 8;
        l |= uget();
        return l;
    }

    public int readSequenceNo()
    {
        return (int) readUint32();
    }

    public long readUint64()
    {
        long l = 0;
        for (int i = 0; i < 8; i++)
        {
            l |= ((long) (0xFF & get())) << (56 - i*8);
        }
        return l;
    }

    public long readDatetime()
    {
        return readUint64();
    }

    private static final String decode(byte[] bytes, int offset, int length, String charset)
    {
        try
        {
            return new String(bytes, offset, length, charset);
        }
        catch (UnsupportedEncodingException e)
        {
            throw new RuntimeException(e);
        }
    }

    private static final String decode(byte[] bytes, String charset)
    {
        return decode(bytes, 0, bytes.length, charset);
    }

    public String readStr8()
    {
        short size = readUint8();
        Binary bin = get(size);
        String str = str8cache.get(bin);
        if (str == null)
        {
            str = decode(bin.array(), bin.offset(), bin.size(), "UTF-8");
            str8cache.put(bin, str);
        }
        return str;
    }

    public String readStr16()
    {
        int size = readUint16();
        byte[] bytes = new byte[size];
        get(bytes);
        return decode(bytes, "UTF-8");
    }

    public byte[] readVbin8()
    {
        int size = readUint8();
        byte[] bytes = new byte[size];
        get(bytes);
        return bytes;
    }

    public byte[] readVbin16()
    {
        int size = readUint16();
        byte[] bytes = new byte[size];
        get(bytes);
        return bytes;
    }

    public byte[] readVbin32()
    {
        int size = (int) readUint32();
        byte[] bytes = new byte[size];
        get(bytes);
        return bytes;
    }

    public RangeSet readSequenceSet()
    {
        int count = readUint16()/8;
        if (count == 0)
        {
            return null;
        }
        else
        {
            RangeSet ranges = new RangeSet();
            for (int i = 0; i < count; i++)
            {
                ranges.add(readSequenceNo(), readSequenceNo());
            }
            return ranges;
        }
    }

    public RangeSet readByteRanges()
    {
        throw new Error("not implemented");
    }

    public UUID readUuid()
    {
        long msb = readUint64();
        long lsb = readUint64();
        return new UUID(msb, lsb);
    }

    public String readContent()
    {
        throw new Error("Deprecated");
    }

    public Struct readStruct(int type)
    {
        Struct st = Struct.create(type);
        int width = st.getSizeWidth();
        if (width > 0)
        {
            long size = readSize(width);
            if (size == 0)
            {
                return null;
            }
        }
        if (type > 0)
        {
            int code = readUint16();
            assert code == type;
        }
        st.read(this);
        return st;
    }

    public Struct readStruct32()
    {
        long size = readUint32();
        if (size == 0)
        {
            return null;
        }
        else
        {
            int type = readUint16();
            Struct result = Struct.create(type);
            result.read(this);
            return result;
        }
    }

    public Map<String,Object> readMap()
    {
        long size = readUint32();

        if (size == 0)
        {
            return null;
        }

        long count = readUint32();

        if (count == 0)
        {
            return Collections.EMPTY_MAP;
        }

        Map<String,Object> result = new LinkedHashMap();
        for (int i = 0; i < count; i++)
        {
            String key = readStr8();
            byte code = get();
            Type t = getType(code);
            Object value = read(t);
            result.put(key, value);
        }

        return result;
    }

    public List<Object> readList()
    {
        long size = readUint32();

        if (size == 0)
        {
            return null;
        }

        long count = readUint32();

        if (count == 0)
        {
            return Collections.EMPTY_LIST;
        }

        List<Object> result = new ArrayList();
        for (int i = 0; i < count; i++)
        {
            byte code = get();
            Type t = getType(code);
            Object value = read(t);
            result.add(value);
        }
        return result;
    }

    public List<Object> readArray()
    {
        long size = readUint32();

        if (size == 0)
        {
            return null;
        }

        byte code = get();
        Type t = getType(code);
        long count = readUint32();

        if (count == 0)
        {
            return Collections.EMPTY_LIST;
        }

        List<Object> result = new ArrayList<Object>();
        for (int i = 0; i < count; i++)
        {
            Object value = read(t);
            result.add(value);
        }
        return result;
    }

    private Type getType(byte code)
    {
        Type type = Type.get(code);
        if (type == null)
        {
            throw new IllegalArgumentException("unknown code: " + code);
        }
        else
        {
            return type;
        }
    }

    private long readSize(Type t)
    {
        if (t.fixed)
        {
            return t.width;
        }
        else
        {
            return readSize(t.width);
        }
    }

    private long readSize(int width)
    {
        switch (width)
        {
        case 1:
            return readUint8();
        case 2:
            return readUint16();
        case 4:
            return readUint32();
        default:
            throw new IllegalStateException("illegal width: " + width);
        }
    }

    private byte[] readBytes(Type t)
    {
        long size = readSize(t);
        byte[] result = new byte[(int) size];
        get(result);
        return result;
    }

    private Object read(Type t)
    {
        switch (t)
        {
        case BIN8:
        case UINT8:
            return readUint8();
        case INT8:
            return get();
        case CHAR:
            return (char) get();
        case BOOLEAN:
            return get() > 0;

        case BIN16:
        case UINT16:
            return readUint16();

        case INT16:
            return (short) readUint16();

        case BIN32:
        case UINT32:
            return readUint32();

        case CHAR_UTF32:
        case INT32:
            return (int) readUint32();

        case FLOAT:
            return Float.intBitsToFloat((int) readUint32());

        case BIN64:
        case UINT64:
        case INT64:
        case DATETIME:
            return readUint64();

        case DOUBLE:
            return Double.longBitsToDouble(readUint64());

        case UUID:
            return readUuid();

        case STR8:
            return readStr8();

        case STR16:
            return readStr16();

        case STR8_LATIN:
        case STR8_UTF16:
        case STR16_LATIN:
        case STR16_UTF16:
            // XXX: need to do character conversion
            return new String(readBytes(t));

        case MAP:
            return readMap();
        case LIST:
            return readList();
        case ARRAY:
            return readArray();
        case STRUCT32:
            return readStruct32();

        case BIN40:
        case DEC32:
        case BIN72:
        case DEC64:
            // XXX: what types are we supposed to use here?
            return readBytes(t);

        case VOID:
            return null;

        default:
            return readBytes(t);
        }
    }

}
