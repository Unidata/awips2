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
package org.apache.qpid.agent.binding;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;

/**
 * Basic type mappings for QMF schema
 */
public abstract class QMFTypeBinding implements TypeBinding
{
    private static final Map<Class<?>, QMFTypeBinding> TYPES = new HashMap<Class<?>, QMFTypeBinding>();
    private static final Map<Short, QMFTypeBinding> TYPES_BY_CODE = new HashMap<Short, QMFTypeBinding>();
    static
    {
        new QMFTypeBinding(null, (short) 1)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Short.valueOf(dec.readUint8());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeUint8(((Number) value).shortValue());
            }
        };
        new QMFTypeBinding(null, (short) 2)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Integer.valueOf(dec.readUint16());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeUint16(((Number) value).intValue());
            }
        };
        new QMFTypeBinding(null, (short) 3)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Long.valueOf(dec.readUint32());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeUint32(((Number) value).longValue());
            }
        };
        new QMFTypeBinding(null, (short) 4)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Long.valueOf(dec.readUint64());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeUint64(((Number) value).longValue());
            }
        };
        new QMFTypeBinding(null, (short) 6) // short string
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readStr8();
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                if (null == value)
                    value = "";
                enc.writeStr8((String) value);
            }
        };
        new QMFTypeBinding(String.class, (short) 7) // long string
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readStr16();
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                if (null == value)
                    value = "";
                enc.writeStr16((String) value);
            }
        };
        new QMFTypeBinding(Date.class, (short) 8)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return new Date(dec.readDatetime());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeDatetime(((Date) value).getTime());
            }
        };
        new QMFTypeBinding(Boolean.class, (short) 11)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Boolean.valueOf(dec.readUint8() != 0);
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                if (((Boolean) value).booleanValue())
                {
                    enc.writeUint8((short) 1);
                } else
                {
                    enc.writeUint8((short) 0);
                }
            }

            @Override
            public short[] alternateTypes()
            {
                short[] types =
                { 5 };
                return types;
            }
        };
        new QMFTypeBinding(boolean.class, (short) 11)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readUint8() != 0;
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                if (((Boolean) value).booleanValue())
                {
                    enc.writeUint8((short) 1);
                } else
                {
                    enc.writeUint8((short) 0);
                }
            }
        };
        new QMFTypeBinding(Float.class, (short) 12)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Float.valueOf(dec.readFloat());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeFloat(((Number) value).floatValue());
            }
        };
        new QMFTypeBinding(float.class, (short) 12)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readFloat();
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeFloat(((Number) value).floatValue());
            }
        };
        new QMFTypeBinding(Double.class, (short) 13)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Double.valueOf(dec.readDouble());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeDouble(((Number) value).doubleValue());
            }
        };
        new QMFTypeBinding(double.class, (short) 13)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readDouble();
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeDouble(((Number) value).doubleValue());
            }
        };
        new QMFTypeBinding(UUID.class, (short) 14)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readUuid();
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeUuid((UUID) value);
            }
        };
        new QMFTypeBinding(byte.class, (short) 16)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readInt8();
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeInt8(((Number) value).byteValue());
            }
        };
        new QMFTypeBinding(Short.class, (short) 17)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Short.valueOf(dec.readInt16());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeInt16(((Number) value).shortValue());
            }
        };
        new QMFTypeBinding(short.class, (short) 17)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readInt16();
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeInt16(((Number) value).shortValue());
            }
        };
        new QMFTypeBinding(Integer.class, (short) 18)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Integer.valueOf(dec.readInt32());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeInt32(((Number) value).intValue());
            }
        };
        new QMFTypeBinding(int.class, (short) 18)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readInt32();
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeInt32(((Number) value).intValue());
            }
        };
        new QMFTypeBinding(Long.class, (short) 19)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return Long.valueOf(dec.readInt64());
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeInt64(((Number) value).longValue());
            }
        };
        new QMFTypeBinding(long.class, (short) 19)
        {
            @Override
            public Object decode(Decoder dec)
            {
                return dec.readInt64();
            }

            @Override
            public void encode(Encoder enc, Object value)
            {
                enc.writeInt64(((Number) value).longValue());
            }
        };
    }

    public static final QMFTypeBinding forClass(Class<?> cls)
    {
        QMFTypeBinding t = TYPES.get(cls);
        return t;
    }

    public static final boolean isBound(Class<?> cls)
    {
        return TYPES.containsKey(cls);
    }

    public static QMFTypeBinding getType(short code)
    {
        return TYPES_BY_CODE.get(code);
    }

    private final Class<?> cls;
    private final short code;

    private QMFTypeBinding(Class<?> cls, short code)
    {
        this.cls = cls;
        this.code = code;
        if (cls != null)
        {
            TYPES.put(cls, this);
        }
        TYPES_BY_CODE.put(code, this);
        for (short type : this.alternateTypes())
        {
            TYPES_BY_CODE.put(type, this);
        }
    }

    public Class<?> getJavaClass()
    {
        return cls;
    }

    public short getCode()
    {
        return code;
    }

    public boolean isNative()
    {
        return true;
    }

    public boolean optionalDefault()
    {
        return false;
    }

    public String getRefClass()
    {
        return null;
    }

    public String getRefPackage()
    {
        return null;
    }

    public abstract Object decode(Decoder dec);

    public abstract void encode(Encoder enc, Object value);

    public short[] alternateTypes()
    {
        short[] types =
        {};
        return types;
    }

    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((cls == null) ? 0 : cls.hashCode());
        result = prime * result + code;
        return result;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        QMFTypeBinding other = (QMFTypeBinding) obj;
        if (cls == null)
        {
            if (other.cls != null)
                return false;
        } else if (!cls.equals(other.cls))
            return false;
        if (code != other.code)
            return false;
        return true;
    }
}
