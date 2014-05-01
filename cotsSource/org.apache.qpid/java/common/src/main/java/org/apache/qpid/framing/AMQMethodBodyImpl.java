package org.apache.qpid.framing;

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

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.AMQChannelException;
import org.apache.qpid.AMQConnectionException;
import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.protocol.AMQVersionAwareProtocolSession;

public abstract class AMQMethodBodyImpl implements AMQMethodBody
{
    public static final byte TYPE = 1;

    public AMQMethodBodyImpl()
    {
    }

    public byte getFrameType()
    {
        return TYPE;
    }


    /** unsigned short */
    abstract protected int getBodySize();


    public AMQFrame generateFrame(int channelId)
    {
        return new AMQFrame(channelId, this);
    }

    /**
     * Creates an AMQChannelException for the corresponding body type (a channel exception should include the class and
     * method ids of the body it resulted from).
     */

    /**
     * Convenience Method to create a channel not found exception
     *
     * @param channelId The channel id that is not found
     *
     * @return new AMQChannelException
     */
    public AMQChannelException getChannelNotFoundException(int channelId)
    {
        return getChannelException(AMQConstant.NOT_FOUND, "Channel not found for id:" + channelId);
    }

    public AMQChannelException getChannelException(AMQConstant code, String message)
    {
        return new AMQChannelException(code, message, getClazz(), getMethod(), getMajor(), getMinor(), null);
    }

    public AMQChannelException getChannelException(AMQConstant code, String message, Throwable cause)
    {
        return new AMQChannelException(code, message, getClazz(), getMethod(), getMajor(), getMinor(), cause);
    }

    public AMQConnectionException getConnectionException(AMQConstant code, String message)
    {
        return new AMQConnectionException(code, message, getClazz(), getMethod(), getMajor(), getMinor(), null);
    }

    public AMQConnectionException getConnectionException(AMQConstant code, String message, Throwable cause)
    {
        return new AMQConnectionException(code, message, getClazz(), getMethod(), getMajor(), getMinor(), cause);
    }

    public void handle(final int channelId, final AMQVersionAwareProtocolSession session) throws AMQException
    {
        session.methodFrameReceived(channelId, this);
    }

    public int getSize()
    {
        return 2 + 2 + getBodySize();
    }

        public void writePayload(ByteBuffer buffer)
    {
        EncodingUtils.writeUnsignedShort(buffer, getClazz());
        EncodingUtils.writeUnsignedShort(buffer, getMethod());
        writeMethodPayload(buffer);
    }


    protected byte readByte(ByteBuffer buffer)
    {
        return buffer.get();
    }

    protected AMQShortString readAMQShortString(ByteBuffer buffer)
    {
        return EncodingUtils.readAMQShortString(buffer);
    }

    protected int getSizeOf(AMQShortString string)
    {
        return EncodingUtils.encodedShortStringLength(string);
    }

    protected void writeByte(ByteBuffer buffer, byte b)
    {
        buffer.put(b);
    }

    protected void writeAMQShortString(ByteBuffer buffer, AMQShortString string)
    {
        EncodingUtils.writeShortStringBytes(buffer, string);
    }

    protected int readInt(ByteBuffer buffer)
    {
        return buffer.getInt();
    }

    protected void writeInt(ByteBuffer buffer, int i)
    {
        buffer.putInt(i);
    }

    protected FieldTable readFieldTable(ByteBuffer buffer) throws AMQFrameDecodingException
    {
        return EncodingUtils.readFieldTable(buffer);
    }

    protected int getSizeOf(FieldTable table)
    {
        return EncodingUtils.encodedFieldTableLength(table);  //To change body of created methods use File | Settings | File Templates.
    }

    protected void writeFieldTable(ByteBuffer buffer, FieldTable table)
    {
        EncodingUtils.writeFieldTableBytes(buffer, table);
    }

    protected long readLong(ByteBuffer buffer)
    {
        return buffer.getLong();
    }

    protected void writeLong(ByteBuffer buffer, long l)
    {
        buffer.putLong(l);
    }

    protected int getSizeOf(byte[] response)
    {
        return (response == null) ? 4 : response.length + 4;
    }

    protected void writeBytes(ByteBuffer buffer, byte[] data)
    {
        EncodingUtils.writeBytes(buffer,data);
    }

    protected byte[] readBytes(ByteBuffer buffer)
    {
        return EncodingUtils.readBytes(buffer);
    }

    protected short readShort(ByteBuffer buffer)
    {
        return EncodingUtils.readShort(buffer);
    }

    protected void writeShort(ByteBuffer buffer, short s)
    {
        EncodingUtils.writeShort(buffer, s);
    }

    protected Content readContent(ByteBuffer buffer)
    {
        return null;  //To change body of created methods use File | Settings | File Templates.
    }

    protected int getSizeOf(Content body)
    {
        return 0;  //To change body of created methods use File | Settings | File Templates.
    }

    protected void writeContent(ByteBuffer buffer, Content body)
    {
        //To change body of created methods use File | Settings | File Templates.
    }

    protected byte readBitfield(ByteBuffer buffer)
    {
        return readByte(buffer);  //To change body of created methods use File | Settings | File Templates.
    }

    protected int readUnsignedShort(ByteBuffer buffer)
    {
        return buffer.getUnsignedShort();  //To change body of created methods use File | Settings | File Templates.
    }

    protected void writeBitfield(ByteBuffer buffer, byte bitfield0)
    {
        buffer.put(bitfield0);
    }

    protected void writeUnsignedShort(ByteBuffer buffer, int s)
    {
        EncodingUtils.writeUnsignedShort(buffer, s);
    }

    protected long readUnsignedInteger(ByteBuffer buffer)
    {
        return buffer.getUnsignedInt();
    }
    protected void writeUnsignedInteger(ByteBuffer buffer, long i)
    {
        EncodingUtils.writeUnsignedInteger(buffer, i);
    }


    protected short readUnsignedByte(ByteBuffer buffer)
    {
        return buffer.getUnsigned();
    }

    protected void writeUnsignedByte(ByteBuffer buffer, short unsignedByte)
    {
        EncodingUtils.writeUnsignedByte(buffer, unsignedByte);
    }

    protected long readTimestamp(ByteBuffer buffer)
    {
        return EncodingUtils.readTimestamp(buffer);
    }

    protected void writeTimestamp(ByteBuffer buffer, long t)
    {
        EncodingUtils.writeTimestamp(buffer, t);
    }
}
