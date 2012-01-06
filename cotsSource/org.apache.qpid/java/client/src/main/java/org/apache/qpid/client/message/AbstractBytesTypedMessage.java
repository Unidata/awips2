/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */

package org.apache.qpid.client.message;

import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;

import javax.jms.JMSException;
import javax.jms.MessageEOFException;
import javax.jms.MessageFormatException;
import javax.jms.MessageNotReadableException;
import javax.jms.MessageNotWriteableException;

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicContentHeaderProperties;

/**
 * @author Apache Software Foundation
 */
public abstract class AbstractBytesTypedMessage extends AbstractBytesMessage
{

    protected static final byte BOOLEAN_TYPE = (byte) 1;

    protected static final byte BYTE_TYPE = (byte) 2;

    protected static final byte BYTEARRAY_TYPE = (byte) 3;

    protected static final byte SHORT_TYPE = (byte) 4;

    protected static final byte CHAR_TYPE = (byte) 5;

    protected static final byte INT_TYPE = (byte) 6;

    protected static final byte LONG_TYPE = (byte) 7;

    protected static final byte FLOAT_TYPE = (byte) 8;

    protected static final byte DOUBLE_TYPE = (byte) 9;

    protected static final byte STRING_TYPE = (byte) 10;

    protected static final byte NULL_STRING_TYPE = (byte) 11;

    /**
     * This is set when reading a byte array. The readBytes(byte[]) method supports multiple calls to read
     * a byte array in multiple chunks, hence this is used to track how much is left to be read
     */
    private int _byteArrayRemaining = -1;

    AbstractBytesTypedMessage(AMQMessageDelegateFactory delegateFactory)
    {

        this(delegateFactory, null);
    }

    /**
     * Construct a stream message with existing data.
     *
     * @param delegateFactory
     * @param data the data that comprises this message. If data is null, you get a 1024 byte buffer that is
     */
    AbstractBytesTypedMessage(AMQMessageDelegateFactory delegateFactory, ByteBuffer data)
    {

        super(delegateFactory, data); // this instanties a content header
    }

    AbstractBytesTypedMessage(AMQMessageDelegate delegate, ByteBuffer data) throws AMQException
    {

        super(delegate, data);
    }


    protected byte readWireType() throws MessageFormatException, MessageEOFException,
            MessageNotReadableException
    {
        checkReadable();
        checkAvailable(1);
        return _data.get();
    }

    protected void writeTypeDiscriminator(byte type) throws MessageNotWriteableException
    {
        checkWritable();
        _data.put(type);
        _changedData = true;
    }

    protected boolean readBoolean() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        boolean result;
        try
        {
            switch (wireType)
            {
                case BOOLEAN_TYPE:
                    checkAvailable(1);
                    result = readBooleanImpl();
                    break;
                case STRING_TYPE:
                    checkAvailable(1);
                    result = Boolean.parseBoolean(readStringImpl());
                    break;
                default:
                    _data.position(position);
                    throw new MessageFormatException("Unable to convert " + wireType + " to a boolean");
            }
            return result;
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
    }

    private boolean readBooleanImpl()
    {
        return _data.get() != 0;
    }

    protected byte readByte() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        byte result;
        try
        {
            switch (wireType)
            {
                case BYTE_TYPE:
                    checkAvailable(1);
                    result = readByteImpl();
                    break;
                case STRING_TYPE:
                    checkAvailable(1);
                    result = Byte.parseByte(readStringImpl());
                    break;
                default:
                    _data.position(position);
                    throw new MessageFormatException("Unable to convert " + wireType + " to a byte");
            }
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
        return result;
    }

    private byte readByteImpl()
    {
        return _data.get();
    }

    protected short readShort() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        short result;
        try
        {
            switch (wireType)
            {
                case SHORT_TYPE:
                    checkAvailable(2);
                    result = readShortImpl();
                    break;
                case STRING_TYPE:
                    checkAvailable(1);
                    result = Short.parseShort(readStringImpl());
                    break;
                case BYTE_TYPE:
                    checkAvailable(1);
                    result = readByteImpl();
                    break;
                default:
                    _data.position(position);
                    throw new MessageFormatException("Unable to convert " + wireType + " to a short");
            }
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
        return result;
    }

    private short readShortImpl()
    {
        return _data.getShort();
    }

    /**
     * Note that this method reads a unicode character as two bytes from the stream
     *
     * @return the character read from the stream
     * @throws javax.jms.JMSException
     */
    protected char readChar() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        try
        {
        	if(wireType == NULL_STRING_TYPE){
        		throw new NullPointerException();
        	}

            if (wireType != CHAR_TYPE)
            {
                _data.position(position);
                throw new MessageFormatException("Unable to convert " + wireType + " to a char");
            }
            else
            {
                checkAvailable(2);
                return readCharImpl();
            }
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
    }

    private char readCharImpl()
    {
        return _data.getChar();
    }

    protected int readInt() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        int result;
        try
        {
            switch (wireType)
            {
                case INT_TYPE:
                    checkAvailable(4);
                    result = readIntImpl();
                    break;
                case SHORT_TYPE:
                    checkAvailable(2);
                    result = readShortImpl();
                    break;
                case STRING_TYPE:
                    checkAvailable(1);
                    result = Integer.parseInt(readStringImpl());
                    break;
                case BYTE_TYPE:
                    checkAvailable(1);
                    result = readByteImpl();
                    break;
                default:
                    _data.position(position);
                    throw new MessageFormatException("Unable to convert " + wireType + " to an int");
            }
            return result;
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
    }

    protected int readIntImpl()
    {
        return _data.getInt();
    }

    protected long readLong() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        long result;
        try
        {
            switch (wireType)
            {
                case LONG_TYPE:
                    checkAvailable(8);
                    result = readLongImpl();
                    break;
                case INT_TYPE:
                    checkAvailable(4);
                    result = readIntImpl();
                    break;
                case SHORT_TYPE:
                    checkAvailable(2);
                    result = readShortImpl();
                    break;
                case STRING_TYPE:
                    checkAvailable(1);
                    result = Long.parseLong(readStringImpl());
                    break;
                case BYTE_TYPE:
                    checkAvailable(1);
                    result = readByteImpl();
                    break;
                default:
                    _data.position(position);
                    throw new MessageFormatException("Unable to convert " + wireType + " to a long");
            }
            return result;
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
    }

    private long readLongImpl()
    {
        return _data.getLong();
    }

    protected float readFloat() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        float result;
        try
        {
            switch (wireType)
            {
                case FLOAT_TYPE:
                    checkAvailable(4);
                    result = readFloatImpl();
                    break;
                case STRING_TYPE:
                    checkAvailable(1);
                    result = Float.parseFloat(readStringImpl());
                    break;
                default:
                    _data.position(position);
                    throw new MessageFormatException("Unable to convert " + wireType + " to a float");
            }
            return result;
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
    }

    private float readFloatImpl()
    {
        return _data.getFloat();
    }

    protected double readDouble() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        double result;
        try
        {
            switch (wireType)
            {
                case DOUBLE_TYPE:
                    checkAvailable(8);
                    result = readDoubleImpl();
                    break;
                case FLOAT_TYPE:
                    checkAvailable(4);
                    result = readFloatImpl();
                    break;
                case STRING_TYPE:
                    checkAvailable(1);
                    result = Double.parseDouble(readStringImpl());
                    break;
                default:
                    _data.position(position);
                    throw new MessageFormatException("Unable to convert " + wireType + " to a double");
            }
            return result;
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
    }

    private double readDoubleImpl()
    {
        return _data.getDouble();
    }

    protected String readString() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        String result;
        try
        {
            switch (wireType)
            {
                case STRING_TYPE:
                    checkAvailable(1);
                    result = readStringImpl();
                    break;
                case NULL_STRING_TYPE:
                    result = null;
                    throw new NullPointerException("data is null");
                case BOOLEAN_TYPE:
                    checkAvailable(1);
                    result = String.valueOf(readBooleanImpl());
                    break;
                case LONG_TYPE:
                    checkAvailable(8);
                    result = String.valueOf(readLongImpl());
                    break;
                case INT_TYPE:
                    checkAvailable(4);
                    result = String.valueOf(readIntImpl());
                    break;
                case SHORT_TYPE:
                    checkAvailable(2);
                    result = String.valueOf(readShortImpl());
                    break;
                case BYTE_TYPE:
                    checkAvailable(1);
                    result = String.valueOf(readByteImpl());
                    break;
                case FLOAT_TYPE:
                    checkAvailable(4);
                    result = String.valueOf(readFloatImpl());
                    break;
                case DOUBLE_TYPE:
                    checkAvailable(8);
                    result = String.valueOf(readDoubleImpl());
                    break;
                case CHAR_TYPE:
                    checkAvailable(2);
                    result = String.valueOf(readCharImpl());
                    break;
                default:
                    _data.position(position);
                    throw new MessageFormatException("Unable to convert " + wireType + " to a String");
            }
            return result;
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
    }

    protected String readStringImpl() throws JMSException
    {
        try
        {
            return _data.getString(Charset.forName("UTF-8").newDecoder());
        }
        catch (CharacterCodingException e)
        {
            JMSException je = new JMSException("Error decoding byte stream as a UTF8 string: " + e);
            je.setLinkedException(e);
            throw je;
        }
    }

    protected int readBytes(byte[] bytes) throws JMSException
    {
        if (bytes == null)
        {
            throw new IllegalArgumentException("byte array must not be null");
        }
        checkReadable();
        // first call
        if (_byteArrayRemaining == -1)
        {
            // type discriminator checked separately so you get a MessageFormatException rather than
            // an EOF even in the case where both would be applicable
            checkAvailable(1);
            byte wireType = readWireType();
            if (wireType != BYTEARRAY_TYPE)
            {
                throw new MessageFormatException("Unable to convert " + wireType + " to a byte array");
            }
            checkAvailable(4);
            int size = _data.getInt();
            // length of -1 indicates null
            if (size == -1)
            {
                return -1;
            }
            else
            {
                if (size > _data.remaining())
                {
                    throw new MessageEOFException("Byte array has stated length " + size + " but message only contains " +
                                                  _data.remaining() + " bytes");
                }
                else
                {
                    _byteArrayRemaining = size;
                }
            }
        }
        else if (_byteArrayRemaining == 0)
        {
            _byteArrayRemaining = -1;
            return -1;
        }

        int returnedSize = readBytesImpl(bytes);
        if (returnedSize < bytes.length)
        {
            _byteArrayRemaining = -1;
        }
        return returnedSize;
    }

    private int readBytesImpl(byte[] bytes)
    {
        int count = (_byteArrayRemaining >= bytes.length ? bytes.length : _byteArrayRemaining);
        _byteArrayRemaining -= count;

        if (count == 0)
        {
            return 0;
        }
        else
        {
            _data.get(bytes, 0, count);
            return count;
        }
    }

    protected Object readObject() throws JMSException
    {
        int position = _data.position();
        byte wireType = readWireType();
        Object result = null;
        try
        {
            switch (wireType)
            {
                case BOOLEAN_TYPE:
                    checkAvailable(1);
                    result = readBooleanImpl();
                    break;
                case BYTE_TYPE:
                    checkAvailable(1);
                    result = readByteImpl();
                    break;
                case BYTEARRAY_TYPE:
                    checkAvailable(4);
                    int size = _data.getInt();
                    if (size == -1)
                    {
                        result = null;
                    }
                    else
                    {
                        _byteArrayRemaining = size;
                        byte[] bytesResult = new byte[size];
                        readBytesImpl(bytesResult);
                        result = bytesResult;
                    }
                    break;
                case SHORT_TYPE:
                    checkAvailable(2);
                    result = readShortImpl();
                    break;
                case CHAR_TYPE:
                    checkAvailable(2);
                    result = readCharImpl();
                    break;
                case INT_TYPE:
                    checkAvailable(4);
                    result = readIntImpl();
                    break;
                case LONG_TYPE:
                    checkAvailable(8);
                    result = readLongImpl();
                    break;
                case FLOAT_TYPE:
                    checkAvailable(4);
                    result = readFloatImpl();
                    break;
                case DOUBLE_TYPE:
                    checkAvailable(8);
                    result = readDoubleImpl();
                    break;
                case NULL_STRING_TYPE:
                    result = null;
                    break;
                case STRING_TYPE:
                    checkAvailable(1);
                    result = readStringImpl();
                    break;
            }
            return result;
        }
        catch (RuntimeException e)
        {
            _data.position(position);
            throw e;
        }
    }

    protected void writeBoolean(boolean b) throws JMSException
    {
        writeTypeDiscriminator(BOOLEAN_TYPE);
        _data.put(b ? (byte) 1 : (byte) 0);
    }

    protected void writeByte(byte b) throws JMSException
    {
        writeTypeDiscriminator(BYTE_TYPE);
        _data.put(b);
    }

    protected void writeShort(short i) throws JMSException
    {
        writeTypeDiscriminator(SHORT_TYPE);
        _data.putShort(i);
    }

    protected void writeChar(char c) throws JMSException
    {
        writeTypeDiscriminator(CHAR_TYPE);
        _data.putChar(c);
    }

    protected void writeInt(int i) throws JMSException
    {
        writeTypeDiscriminator(INT_TYPE);
        writeIntImpl(i);
    }

    protected void writeIntImpl(int i)
    {
        _data.putInt(i);
    }

    protected void writeLong(long l) throws JMSException
    {
        writeTypeDiscriminator(LONG_TYPE);
        _data.putLong(l);
    }

    protected void writeFloat(float v) throws JMSException
    {
        writeTypeDiscriminator(FLOAT_TYPE);
        _data.putFloat(v);
    }

    protected void writeDouble(double v) throws JMSException
    {
        writeTypeDiscriminator(DOUBLE_TYPE);
        _data.putDouble(v);
    }

    protected void writeString(String string) throws JMSException
    {
        if (string == null)
        {
            writeTypeDiscriminator(NULL_STRING_TYPE);
        }
        else
        {
            writeTypeDiscriminator(STRING_TYPE);
            try
            {
                writeStringImpl(string);
            }
            catch (CharacterCodingException e)
            {
                JMSException ex = new JMSException("Unable to encode string: " + e);
                ex.setLinkedException(e);
                throw ex;
            }
        }
    }

    protected void writeStringImpl(String string)
            throws CharacterCodingException
    {
        _data.putString(string, Charset.forName("UTF-8").newEncoder());
        // we must write the null terminator ourselves
        _data.put((byte) 0);
    }

    protected void writeBytes(byte[] bytes) throws JMSException
    {
        writeBytes(bytes, 0, bytes == null ? 0 : bytes.length);
    }

    protected void writeBytes(byte[] bytes, int offset, int length) throws JMSException
    {
        writeTypeDiscriminator(BYTEARRAY_TYPE);
        if (bytes == null)
        {
            _data.putInt(-1);
        }
        else
        {
            _data.putInt(length);
            _data.put(bytes, offset, length);
        }
    }

    protected void writeObject(Object object) throws JMSException
    {
        checkWritable();
        Class clazz;

        if (object == null)
        {
            // string handles the output of null values
            clazz = String.class;
        }
        else
        {
            clazz = object.getClass();
        }

        if (clazz == Byte.class)
        {
            writeByte((Byte) object);
        }
        else if (clazz == Boolean.class)
        {
            writeBoolean((Boolean) object);
        }
        else if (clazz == byte[].class)
        {
            writeBytes((byte[]) object);
        }
        else if (clazz == Short.class)
        {
            writeShort((Short) object);
        }
        else if (clazz == Character.class)
        {
            writeChar((Character) object);
        }
        else if (clazz == Integer.class)
        {
            writeInt((Integer) object);
        }
        else if (clazz == Long.class)
        {
            writeLong((Long) object);
        }
        else if (clazz == Float.class)
        {
            writeFloat((Float) object);
        }
        else if (clazz == Double.class)
        {
            writeDouble((Double) object);
        }
        else if (clazz == String.class)
        {
            writeString((String) object);
        }
        else
        {
            throw new MessageFormatException("Only primitives plus byte arrays and String are valid types");
        }
    }
}
