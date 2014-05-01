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

import java.math.BigDecimal;

/**
 * AMQType is a type that represents the different possible AMQP field table types. It provides operations for each
 * of the types to perform tasks such as calculating the size of an instance of the type, converting types between AMQP
 * and Java native types, and reading and writing instances of AMQP types in binary formats to and from byte buffers.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Get the equivalent one byte identifier for a type.
 * <tr><td> Calculate the size of an instance of an AMQP parameter type. <td> {@link EncodingUtils}
 * <tr><td> Convert an instance of an AMQP parameter into a compatable Java object tagged with its AMQP type.
 *     <td> {@link AMQTypedValue}
 * <tr><td> Write an instance of an AMQP parameter type to a byte buffer. <td> {@link EncodingUtils}
 * <tr><td> Read an instance of an AMQP parameter from a byte buffer. <td> {@link EncodingUtils}
 * </table>
 */
public enum AMQType
{
    LONG_STRING('S')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedLongStringLength((String) value);
        }

        public String toNativeValue(Object value)
        {
            if (value != null)
            {
                return value.toString();
            }
            else
            {
                throw new NullPointerException("Cannot convert: null to String.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeLongStringBytes(buffer, (String) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readLongString(buffer);
        }
    },

    INTEGER('i')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.unsignedIntegerLength();
        }

        public Long toNativeValue(Object value)
        {
            if (value instanceof Long)
            {
                return (Long) value;
            }
            else if (value instanceof Integer)
            {
                return ((Integer) value).longValue();
            }
            else if (value instanceof Short)
            {
                return ((Short) value).longValue();
            }
            else if (value instanceof Byte)
            {
                return ((Byte) value).longValue();
            }
            else if ((value instanceof String) || (value == null))
            {
                return Long.valueOf((String) value);
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName() + ") to int.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeUnsignedInteger(buffer, (Long) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readUnsignedInteger(buffer);
        }
    },

    DECIMAL('D')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedByteLength() + EncodingUtils.encodedIntegerLength();
        }

        public Object toNativeValue(Object value)
        {
            if (value instanceof BigDecimal)
            {
                return (BigDecimal) value;
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to BigDecimal.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            BigDecimal bd = (BigDecimal) value;

            byte places = new Integer(bd.scale()).byteValue();

            int unscaled = bd.intValue();

            EncodingUtils.writeByte(buffer, places);

            EncodingUtils.writeInteger(buffer, unscaled);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            byte places = EncodingUtils.readByte(buffer);

            int unscaled = EncodingUtils.readInteger(buffer);

            BigDecimal bd = new BigDecimal(unscaled);

            return bd.setScale(places);
        }
    },

    TIMESTAMP('T')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedLongLength();
        }

        public Object toNativeValue(Object value)
        {
            if (value instanceof Long)
            {
                return (Long) value;
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to timestamp.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeLong(buffer, (Long) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readLong(buffer);
        }
    },

    /**
     * Implements the field table type. The native value of a field table type will be an instance of
     * {@link FieldTable}, which itself may contain name/value pairs encoded as {@link AMQTypedValue}s.
     */
    FIELD_TABLE('F')
    {
        /**
         * Calculates the size of an instance of the type in bytes.
         *
         * @param value An instance of the type.
         *
         * @return The size of the instance of the type in bytes.
         */
        public int getEncodingSize(Object value)
        {
            // Ensure that the value is a FieldTable.
            if (!(value instanceof FieldTable))
            {
                throw new IllegalArgumentException("Value is not a FieldTable.");
            }

            FieldTable ftValue = (FieldTable) value;

            // Loop over all name/value pairs adding up size of each. FieldTable itself keeps track of its encoded
            // size as entries are added, so no need to loop over all explicitly.
            // EncodingUtils calculation of the encoded field table lenth, will include 4 bytes for its 'size' field.
            return EncodingUtils.encodedFieldTableLength(ftValue);
        }

        /**
         * Converts an instance of the type to an equivalent Java native representation.
         *
         * @param value An instance of the type.
         *
         * @return An equivalent Java native representation.
         */
        public Object toNativeValue(Object value)
        {
            // Ensure that the value is a FieldTable.
            if (!(value instanceof FieldTable))
            {
                throw new IllegalArgumentException("Value is not a FieldTable.");
            }

            return (FieldTable) value;
        }

        /**
         * Writes an instance of the type to a specified byte buffer.
         *
         * @param value  An instance of the type.
         * @param buffer The byte buffer to write it to.
         */
        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            // Ensure that the value is a FieldTable.
            if (!(value instanceof FieldTable))
            {
                throw new IllegalArgumentException("Value is not a FieldTable.");
            }

            FieldTable ftValue = (FieldTable) value;

            // Loop over all name/values writing out into buffer.
            ftValue.writeToBuffer(buffer);
        }

        /**
         * Reads an instance of the type from a specified byte buffer.
         *
         * @param buffer The byte buffer to write it to.
         *
         * @return An instance of the type.
         */
        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            try
            {
                // Read size of field table then all name/value pairs.
                return EncodingUtils.readFieldTable(buffer);
            }
            catch (AMQFrameDecodingException e)
            {
                throw new IllegalArgumentException("Unable to read field table from buffer.", e);
            }
        }
    },

    VOID('V')
    {
        public int getEncodingSize(Object value)
        {
            return 0;
        }

        public Object toNativeValue(Object value)
        {
            if (value == null)
            {
                return null;
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to null String.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        { }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return null;
        }
    },

    BINARY('x')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedLongstrLength((byte[]) value);
        }

        public Object toNativeValue(Object value)
        {
            if ((value instanceof byte[]) || (value == null))
            {
                return value;
            }
            else
            {
                throw new IllegalArgumentException("Value: " + value + " (" + value.getClass().getName()
                    + ") cannot be converted to byte[]");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeLongstr(buffer, (byte[]) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readLongstr(buffer);
        }
    },

    ASCII_STRING('c')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedLongStringLength((String) value);
        }

        public String toNativeValue(Object value)
        {
            if (value != null)
            {
                return value.toString();
            }
            else
            {
                throw new NullPointerException("Cannot convert: null to String.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeLongStringBytes(buffer, (String) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readLongString(buffer);
        }
    },

    WIDE_STRING('C')
    {
        public int getEncodingSize(Object value)
        {
            // FIXME: use proper charset encoder
            return EncodingUtils.encodedLongStringLength((String) value);
        }

        public String toNativeValue(Object value)
        {
            if (value != null)
            {
                return value.toString();
            }
            else
            {
                throw new NullPointerException("Cannot convert: null to String.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeLongStringBytes(buffer, (String) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readLongString(buffer);
        }
    },

    BOOLEAN('t')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedBooleanLength();
        }

        public Object toNativeValue(Object value)
        {
            if (value instanceof Boolean)
            {
                return (Boolean) value;
            }
            else if ((value instanceof String) || (value == null))
            {
                return Boolean.valueOf((String) value);
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to boolean.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeBoolean(buffer, (Boolean) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readBoolean(buffer);
        }
    },

    ASCII_CHARACTER('k')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedCharLength();
        }

        public Character toNativeValue(Object value)
        {
            if (value instanceof Character)
            {
                return (Character) value;
            }
            else if (value == null)
            {
                throw new NullPointerException("Cannot convert null into char");
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to char.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeChar(buffer, (Character) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readChar(buffer);
        }
    },

    BYTE('b')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedByteLength();
        }

        public Byte toNativeValue(Object value)
        {
            if (value instanceof Byte)
            {
                return (Byte) value;
            }
            else if ((value instanceof String) || (value == null))
            {
                return Byte.valueOf((String) value);
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to byte.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeByte(buffer, (Byte) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readByte(buffer);
        }
    },

    SHORT('s')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedShortLength();
        }

        public Short toNativeValue(Object value)
        {
            if (value instanceof Short)
            {
                return (Short) value;
            }
            else if (value instanceof Byte)
            {
                return ((Byte) value).shortValue();
            }
            else if ((value instanceof String) || (value == null))
            {
                return Short.valueOf((String) value);
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to short.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeShort(buffer, (Short) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readShort(buffer);
        }
    },

    INT('I')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedIntegerLength();
        }

        public Integer toNativeValue(Object value)
        {
            if (value instanceof Integer)
            {
                return (Integer) value;
            }
            else if (value instanceof Short)
            {
                return ((Short) value).intValue();
            }
            else if (value instanceof Byte)
            {
                return ((Byte) value).intValue();
            }
            else if ((value instanceof String) || (value == null))
            {
                return Integer.valueOf((String) value);
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName() + ") to int.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeInteger(buffer, (Integer) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readInteger(buffer);
        }
    },

    LONG('l')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedLongLength();
        }

        public Object toNativeValue(Object value)
        {
            if (value instanceof Long)
            {
                return (Long) value;
            }
            else if (value instanceof Integer)
            {
                return ((Integer) value).longValue();
            }
            else if (value instanceof Short)
            {
                return ((Short) value).longValue();
            }
            else if (value instanceof Byte)
            {
                return ((Byte) value).longValue();
            }
            else if ((value instanceof String) || (value == null))
            {
                return Long.valueOf((String) value);
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to long.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeLong(buffer, (Long) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readLong(buffer);
        }
    },

    FLOAT('f')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedFloatLength();
        }

        public Float toNativeValue(Object value)
        {
            if (value instanceof Float)
            {
                return (Float) value;
            }
            else if ((value instanceof String) || (value == null))
            {
                return Float.valueOf((String) value);
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to float.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeFloat(buffer, (Float) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readFloat(buffer);
        }
    },

    DOUBLE('d')
    {
        public int getEncodingSize(Object value)
        {
            return EncodingUtils.encodedDoubleLength();
        }

        public Double toNativeValue(Object value)
        {
            if (value instanceof Double)
            {
                return (Double) value;
            }
            else if (value instanceof Float)
            {
                return ((Float) value).doubleValue();
            }
            else if ((value instanceof String) || (value == null))
            {
                return Double.valueOf((String) value);
            }
            else
            {
                throw new NumberFormatException("Cannot convert: " + value + "(" + value.getClass().getName()
                    + ") to double.");
            }
        }

        public void writeValueImpl(Object value, ByteBuffer buffer)
        {
            EncodingUtils.writeDouble(buffer, (Double) value);
        }

        public Object readValueFromBuffer(ByteBuffer buffer)
        {
            return EncodingUtils.readDouble(buffer);
        }
    };

    /** Holds the defined one byte identifier for the type. */
    private final byte _identifier;

    /**
     * Creates an instance of an AMQP type from its defined one byte identifier.
     *
     * @param identifier The one byte identifier for the type.
     */
    AMQType(char identifier)
    {
        _identifier = (byte) identifier;
    }

    /**
     * Extracts the byte identifier for the typ.
     *
     * @return The byte identifier for the typ.
     */
    public final byte identifier()
    {
        return _identifier;
    }

    /**
     * Calculates the size of an instance of the type in bytes.
     *
     * @param value An instance of the type.
     *
     * @return The size of the instance of the type in bytes.
     */
    public abstract int getEncodingSize(Object value);

    /**
     * Converts an instance of the type to an equivalent Java native representation.
     *
     * @param value An instance of the type.
     *
     * @return An equivalent Java native representation.
     */
    public abstract Object toNativeValue(Object value);

    /**
     * Converts an instance of the type to an equivalent Java native representation, packaged as an
     * {@link AMQTypedValue} tagged with its AMQP type.
     *
     * @param value An instance of the type.
     *
     * @return An equivalent Java native representation, tagged with its AMQP type.
     */
    public AMQTypedValue asTypedValue(Object value)
    {
        return new AMQTypedValue(this, toNativeValue(value));
    }

    /**
     * Writes an instance of the type to a specified byte buffer, preceded by its one byte identifier. As the type and
     * value are both written, this provides a fully encoded description of a parameters type and value.
     *
     * @param value  An instance of the type.
     * @param buffer The byte buffer to write it to.
     */
    public void writeToBuffer(Object value, ByteBuffer buffer)
    {
        buffer.put(identifier());
        writeValueImpl(value, buffer);
    }

    /**
     * Writes an instance of the type to a specified byte buffer.
     *
     * @param value  An instance of the type.
     * @param buffer The byte buffer to write it to.
     */
    abstract void writeValueImpl(Object value, ByteBuffer buffer);

    /**
     * Reads an instance of the type from a specified byte buffer.
     *
     * @param buffer The byte buffer to write it to.
     *
     * @return An instance of the type.
     */
    abstract Object readValueFromBuffer(ByteBuffer buffer);
}
