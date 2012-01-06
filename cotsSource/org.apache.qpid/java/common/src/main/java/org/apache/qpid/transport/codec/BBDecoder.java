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

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import org.apache.qpid.transport.Binary;

/**
 * Byte Buffer Decoder.
 * Decoder concrete implementor using a backing byte buffer for decoding data.
 *
 * @author Rafael H. Schloming
 */
public final class BBDecoder extends AbstractDecoder
{
    private ByteBuffer in;

    public void init(ByteBuffer in)
    {
        this.in = in;
        this.in.order(ByteOrder.BIG_ENDIAN);
    }

    protected byte doGet()
    {
        return in.get();
    }

    protected void doGet(byte[] bytes)
    {
        in.get(bytes);
    }

    protected Binary get(int size)
    {
        if (in.hasArray())
        {
            byte[] bytes = in.array();
            Binary bin = new Binary(bytes, in.arrayOffset() + in.position(), size);
            in.position(in.position() + size);
            return bin;
        }
        else
        {
            return super.get(size);
        }
    }

    public boolean hasRemaining()
    {
        return in.hasRemaining();
    }

    public short readUint8()
    {
        return (short) (0xFF & in.get());
    }

    public int readUint16()
    {
        return 0xFFFF & in.getShort();
    }

    public long readUint32()
    {
        return 0xFFFFFFFFL & in.getInt();
    }

    public long readUint64()
    {
        return in.getLong();
    }

	public byte[] readBin128()
	{
		byte[] result = new byte[16];
		get(result);
		return result;
	}
	
	public byte[] readBytes(int howManyBytes)
	{
		byte[] result = new byte[howManyBytes];
		get(result);
		return result;
	}
	
	public double readDouble()
	{
		return in.getDouble();
	}

	public float readFloat()
	{
		return in.getFloat();
	}

	public short readInt16()
	{
		return in.getShort();
	}

	public int readInt32()
	{
		return in.getInt();
	}

	public byte readInt8()
	{
		return in.get();
	}

	public byte[] readReaminingBytes()
	{
      byte[] result = new byte[in.limit() - in.position()];
      get(result);
      return result;		
	}

	public long readInt64()
	{
		return in.getLong();
	}
}