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
package org.apache.qpid.framing;

import org.apache.mina.common.ByteBuffer;

import java.util.Date;
import java.util.Map;
import java.math.BigDecimal;

/**
 * AMQTypedValue combines together a native Java Object value, and an {@link AMQType}, as a fully typed AMQP parameter
 * value. It provides the ability to read and write fully typed parameters to and from byte buffers. It also provides
 * the ability to create such parameters from Java native value and a type tag or to extract the native value and type
 * from one.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Create a fully typed AMQP value from a native type and a type tag. <td> {@link AMQType}
 * <tr><td> Create a fully typed AMQP value from a binary representation in a byte buffer. <td> {@link AMQType}
 * <tr><td> Write a fully typed AMQP value to a binary representation in a byte buffer. <td> {@link AMQType}
 * <tr><td> Extract the type from a fully typed AMQP value.
 * <tr><td> Extract the value from a fully typed AMQP value.
 * </table>
 */
public class AMQTypedValue
{
    /** The type of the value. */
    private final AMQType _type;

    /** The Java native representation of the AMQP typed value. */
    private final Object _value;

    public AMQTypedValue(AMQType type, Object value)
    {
        if (type == null)
        {
            throw new NullPointerException("Cannot create a typed value with null type");
        }

        _type = type;
        _value = type.toNativeValue(value);
    }

    private AMQTypedValue(AMQType type, ByteBuffer buffer)
    {
        _type = type;
        _value = type.readValueFromBuffer(buffer);
    }

    public AMQType getType()
    {
        return _type;
    }

    public Object getValue()
    {
        return _value;
    }

    public void writeToBuffer(ByteBuffer buffer)
    {
        _type.writeToBuffer(_value, buffer);
    }

    public int getEncodingSize()
    {
        return _type.getEncodingSize(_value);
    }

    public static AMQTypedValue readFromBuffer(ByteBuffer buffer)
    {
        AMQType type = AMQTypeMap.getType(buffer.get());

        return new AMQTypedValue(type, buffer);
    }

    public String toString()
    {
        return "[" + getType() + ": " + getValue() + "]";
    }


    public boolean equals(Object o)
    {
        if(o instanceof AMQTypedValue)
        {
            AMQTypedValue other = (AMQTypedValue) o;
            return _type == other._type && (_value == null ? other._value == null : _value.equals(other._value));
        }
        else
        {
            return false;
        }
    }

    public int hashCode()
    {
        return _type.hashCode() ^ (_value == null ? 0 : _value.hashCode());
    }


    public static AMQTypedValue toTypedValue(Object val)
    {
        if(val == null)
        {
            return AMQType.VOID.asTypedValue(null);
        }

        Class klass = val.getClass();
        if(klass == String.class)
        {
            return AMQType.ASCII_STRING.asTypedValue(val);
        }
        else if(klass == Character.class)
        {
            return AMQType.ASCII_CHARACTER.asTypedValue(val);
        }
        else if(klass == Integer.class)
        {
            return AMQType.INT.asTypedValue(val);
        }
        else if(klass == Long.class)
        {
            return AMQType.LONG.asTypedValue(val);
        }
        else if(klass == Float.class)
        {
            return AMQType.FLOAT.asTypedValue(val);
        }
        else if(klass == Double.class)
        {
            return AMQType.DOUBLE.asTypedValue(val);
        }
        else if(klass == Date.class)
        {
            return AMQType.TIMESTAMP.asTypedValue(val);
        }
        else if(klass == Byte.class)
        {
            return AMQType.BYTE.asTypedValue(val);
        }
        else if(klass == Boolean.class)
        {
            return AMQType.BOOLEAN.asTypedValue(val);
        }
        else if(klass == byte[].class)
        {
            return AMQType.BINARY.asTypedValue(val);
        }
        else if(klass == BigDecimal.class)
        {
            return AMQType.DECIMAL.asTypedValue(val);
        }
        else if(val instanceof Map)
        {
            return AMQType.FIELD_TABLE.asTypedValue(FieldTable.convertToFieldTable((Map)val));
        }
        return null;
    }
}
