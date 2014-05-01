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
package org.apache.qpid.client.message;

import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;

import javax.jms.BytesMessage;
import javax.jms.JMSException;
import javax.jms.MessageFormatException;

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicContentHeaderProperties;

public class JMSBytesMessage extends AbstractBytesMessage implements BytesMessage
{
    public static final String MIME_TYPE = "application/octet-stream";



    public JMSBytesMessage(AMQMessageDelegateFactory delegateFactory)
    {
        this(delegateFactory,null);

    }

    /**
     * Construct a bytes message with existing data.
     *
     * @param delegateFactory
     * @param data the data that comprises this message. If data is null, you get a 1024 byte buffer that is
     */
    JMSBytesMessage(AMQMessageDelegateFactory delegateFactory, ByteBuffer data)
    {

        super(delegateFactory, data); // this instanties a content header
    }

    JMSBytesMessage(AMQMessageDelegate delegate, ByteBuffer data) throws AMQException
    {
        super(delegate, data);
    }


    public void reset()
    {
        super.reset();
        _readableMessage = true;
    }

    protected String getMimeType()
    {
        return MIME_TYPE;
    }

    public long getBodyLength() throws JMSException
    {
        checkReadable();
        return _data.limit();
    }

    public boolean readBoolean() throws JMSException
    {
        checkReadable();
        checkAvailable(1);
        return _data.get() != 0;
    }

    public byte readByte() throws JMSException
    {
        checkReadable();
        checkAvailable(1);
        return _data.get();
    }

    public int readUnsignedByte() throws JMSException
    {
        checkReadable();
        checkAvailable(1);
        return _data.getUnsigned();
    }

    public short readShort() throws JMSException
    {
        checkReadable();
        checkAvailable(2);
        return _data.getShort();
    }

    public int readUnsignedShort() throws JMSException
    {
        checkReadable();
        checkAvailable(2);
        return _data.getUnsignedShort();
    }

    /**
     * Note that this method reads a unicode character as two bytes from the stream
     *
     * @return the character read from the stream
     * @throws JMSException
     */
    public char readChar() throws JMSException
    {
        checkReadable();
        checkAvailable(2);
        return _data.getChar();
    }

    public int readInt() throws JMSException
    {
        checkReadable();
        checkAvailable(4);
        return _data.getInt();
    }

    public long readLong() throws JMSException
    {
        checkReadable();
        checkAvailable(8);
        return _data.getLong();
    }

    public float readFloat() throws JMSException
    {
        checkReadable();
        checkAvailable(4);
        return _data.getFloat();
    }

    public double readDouble() throws JMSException
    {
        checkReadable();
        checkAvailable(8);
        return _data.getDouble();
    }

    public String readUTF() throws JMSException
    {
        checkReadable();
        // we check only for one byte since theoretically the string could be only a
        // single byte when using UTF-8 encoding

        try
        {
            short length = readShort();
            if(length == 0)
            {
                return "";
            }
            else
            {
                CharsetDecoder decoder = Charset.forName("UTF-8").newDecoder();
                ByteBuffer encodedString = _data.slice();
                encodedString.limit(length);
                _data.position(_data.position()+length);
                CharBuffer string = decoder.decode(encodedString.buf());
                
                return string.toString();
            }


            
        }
        catch (CharacterCodingException e)
        {
            JMSException je = new JMSException("Error decoding byte stream as a UTF8 string: " + e);
            je.setLinkedException(e);
            throw je;
        }
    }

    public int readBytes(byte[] bytes) throws JMSException
    {
        if (bytes == null)
        {
            throw new IllegalArgumentException("byte array must not be null");
        }
        checkReadable();
        int count = (_data.remaining() >= bytes.length ? bytes.length : _data.remaining());
        if (count == 0)
        {
            return -1;
        }
        else
        {
            _data.get(bytes, 0, count);
            return count;
        }
    }

    public int readBytes(byte[] bytes, int maxLength) throws JMSException
    {
        if (bytes == null)
        {
            throw new IllegalArgumentException("byte array must not be null");
        }
        if (maxLength > bytes.length)
        {
            throw new IllegalArgumentException("maxLength must be <= bytes.length");
        }
        checkReadable();
        int count = (_data.remaining() >= maxLength ? maxLength : _data.remaining());
        if (count == 0)
        {
            return -1;
        }
        else
        {
            _data.get(bytes, 0, count);
            return count;
        }
    }

    public void writeBoolean(boolean b) throws JMSException
    {
        checkWritable();
        _changedData = true;
        _data.put(b ? (byte) 1 : (byte) 0);
    }

    public void writeByte(byte b) throws JMSException
    {
        checkWritable();
        _changedData = true;
        _data.put(b);
    }

    public void writeShort(short i) throws JMSException
    {
        checkWritable();
        _changedData = true;
        _data.putShort(i);
    }

    public void writeChar(char c) throws JMSException
    {
        checkWritable();
        _changedData = true;
        _data.putChar(c);
    }

    public void writeInt(int i) throws JMSException
    {
        checkWritable();
        _changedData = true;
        _data.putInt(i);
    }

    public void writeLong(long l) throws JMSException
    {
        checkWritable();
        _changedData = true;
        _data.putLong(l);
    }

    public void writeFloat(float v) throws JMSException
    {
        checkWritable();
        _changedData = true;
        _data.putFloat(v);
    }

    public void writeDouble(double v) throws JMSException
    {
        checkWritable();
        _changedData = true;
        _data.putDouble(v);
    }

    public void writeUTF(String string) throws JMSException
    {
        checkWritable();
        try
        {
            CharsetEncoder encoder = Charset.forName("UTF-8").newEncoder();
            java.nio.ByteBuffer encodedString = encoder.encode(CharBuffer.wrap(string));
            
            _data.putShort((short)encodedString.limit());
            _data.put(encodedString);
            _changedData = true;
            //_data.putString(string, Charset.forName("UTF-8").newEncoder());
            // we must add the null terminator manually
            //_data.put((byte)0);
        }
        catch (CharacterCodingException e)
        {
            JMSException ex = new JMSException("Unable to encode string: " + e);
            ex.setLinkedException(e);
            throw ex;
        }
    }

    public void writeBytes(byte[] bytes) throws JMSException
    {
        checkWritable();
        _data.put(bytes);
        _changedData = true;
    }

    public void writeBytes(byte[] bytes, int offset, int length) throws JMSException
    {
        checkWritable();
        _data.put(bytes, offset, length);
        _changedData = true;
    }

    public void writeObject(Object object) throws JMSException
    {
        checkWritable();
        if (object == null)
        {
            throw new NullPointerException("Argument must not be null");
        }
        Class clazz = object.getClass();
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
            writeUTF((String) object);
        }
        else
        {
            throw new MessageFormatException("Only primitives plus byte arrays and String are valid types");
        }
    }
}
