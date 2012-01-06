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

import javax.jms.JMSException;
import javax.jms.StreamMessage;

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicContentHeaderProperties;

/**
 * @author Apache Software Foundation
 */
public class JMSStreamMessage extends AbstractBytesTypedMessage implements StreamMessage
{
    public static final String MIME_TYPE="jms/stream-message";



    /**
     * This is set when reading a byte array. The readBytes(byte[]) method supports multiple calls to read
     * a byte array in multiple chunks, hence this is used to track how much is left to be read
     */
    private int _byteArrayRemaining = -1;

    public JMSStreamMessage(AMQMessageDelegateFactory delegateFactory)
    {
        this(delegateFactory,null);

    }

    /**
     * Construct a stream message with existing data.
     *
     * @param delegateFactory
     * @param data the data that comprises this message. If data is null, you get a 1024 byte buffer that is
     */
    JMSStreamMessage(AMQMessageDelegateFactory delegateFactory, ByteBuffer data)
    {

        super(delegateFactory, data); // this instanties a content header
    }

    JMSStreamMessage(AMQMessageDelegate delegate, ByteBuffer data) throws AMQException
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



    public boolean readBoolean() throws JMSException
    {
        return super.readBoolean();
    }


    public byte readByte() throws JMSException
    {
        return super.readByte();
    }

    public short readShort() throws JMSException
    {
        return super.readShort();
    }

    /**
     * Note that this method reads a unicode character as two bytes from the stream
     *
     * @return the character read from the stream
     * @throws JMSException
     */
    public char readChar() throws JMSException
    {
        return super.readChar();
    }

    public int readInt() throws JMSException
    {
        return super.readInt();
    }

    public long readLong() throws JMSException
    {
        return super.readLong();
    }

    public float readFloat() throws JMSException
    {
        return super.readFloat();
    }

    public double readDouble() throws JMSException
    {
        return super.readDouble();
    }

    public String readString() throws JMSException
    {
        return super.readString();
    }

    public int readBytes(byte[] bytes) throws JMSException
    {
        return super.readBytes(bytes);
    }


    public Object readObject() throws JMSException
    {
        return super.readObject();
    }

    public void writeBoolean(boolean b) throws JMSException
    {
        super.writeBoolean(b);
    }

    public void writeByte(byte b) throws JMSException
    {
        super.writeByte(b);
    }

    public void writeShort(short i) throws JMSException
    {
        super.writeShort(i);
    }

    public void writeChar(char c) throws JMSException
    {
        super.writeChar(c);
    }

    public void writeInt(int i) throws JMSException
    {
        super.writeInt(i);
    }

    public void writeLong(long l) throws JMSException
    {
        super.writeLong(l);
    }

    public void writeFloat(float v) throws JMSException
    {
        super.writeFloat(v);
    }

    public void writeDouble(double v) throws JMSException
    {
        super.writeDouble(v);
    }

    public void writeString(String string) throws JMSException
    {
        super.writeString(string);
    }

    public void writeBytes(byte[] bytes) throws JMSException
    {
        super.writeBytes(bytes);
    }

    public void writeBytes(byte[] bytes, int offset, int length) throws JMSException
    {
        super.writeBytes(bytes,offset,length);
    }

    public void writeObject(Object object) throws JMSException
    {
        super.writeObject(object);
    }
}
