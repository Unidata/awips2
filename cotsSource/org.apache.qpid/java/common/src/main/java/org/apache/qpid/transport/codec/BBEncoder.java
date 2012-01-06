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

import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;


/**
 * Byte Buffer Encoder.
 * Encoder concrete implementor using a backing byte buffer for encoding data.
 * 
 * @author Rafael H. Schloming
 */
public final class BBEncoder extends AbstractEncoder
{
    private ByteBuffer out;
    private int segment;

    public BBEncoder(int capacity) {
        out = ByteBuffer.allocate(capacity);
        out.order(ByteOrder.BIG_ENDIAN);
        segment = 0;
    }

    public void init()
    {
        out.clear();
        segment = 0;
    }

    public ByteBuffer segment()
    {
        int pos = out.position();
        out.position(segment);
        ByteBuffer slice = out.slice();
        slice.limit(pos - segment);
        out.position(pos);
        segment = pos;
        return slice;
    }

    public ByteBuffer buffer()
    {
        int pos = out.position();
        out.position(segment);
        ByteBuffer slice = out.slice();
        slice.limit(pos - segment);
        out.position(pos);
        return slice;
    }

    private void grow(int size)
    {
        ByteBuffer old = out;
        int capacity = old.capacity();
        out = ByteBuffer.allocate(Math.max(capacity + size, 2*capacity));
        out.order(ByteOrder.BIG_ENDIAN);
        old.flip();
        out.put(old);
    }

    protected void doPut(byte b)
    {
        try
        {
            out.put(b);
        }
        catch (BufferOverflowException e)
        {
            grow(1);
            out.put(b);
        }
    }

    protected void doPut(ByteBuffer src)
    {
        try
        {
            out.put(src);
        }
        catch (BufferOverflowException e)
        {
            grow(src.remaining());
            out.put(src);
        }
    }

    protected void put(byte[] bytes)
    {
        try
        {
            out.put(bytes);
        }
        catch (BufferOverflowException e)
        {
            grow(bytes.length);
            out.put(bytes);
        }
    }

    public void writeUint8(short b)
    {
        assert b < 0x100;

        try
        {
            out.put((byte) b);
        }
        catch (BufferOverflowException e)
        {
            grow(1);
            out.put((byte) b);
        }
    }

    public void writeUint16(int s)
    {
        assert s < 0x10000;

        try
        {
            out.putShort((short) s);
        }
        catch (BufferOverflowException e)
        {
            grow(2);
            out.putShort((short) s);
        }
    }

    public void writeUint32(long i)
    {
        assert i < 0x100000000L;

        try
        {
            out.putInt((int) i);
        }
        catch (BufferOverflowException e)
        {
            grow(4);
            out.putInt((int) i);
        }
    }

    public void writeUint64(long l)
    {
        try
        {
            out.putLong(l);
        }
        catch (BufferOverflowException e)
        {
            grow(8);
            out.putLong(l);
        }
    }

    public int beginSize8()
    {
        int pos = out.position();
        try
        {
            out.put((byte) 0);
        }
        catch (BufferOverflowException e)
        {
            grow(1);
            out.put((byte) 0);
        }
        return pos;
    }

    public void endSize8(int pos)
    {
        int cur = out.position();
        out.put(pos, (byte) (cur - pos - 1));
    }

    public int beginSize16()
    {
        int pos = out.position();
        try
        {
            out.putShort((short) 0);
        }
        catch (BufferOverflowException e)
        {
            grow(2);
            out.putShort((short) 0);
        }
        return pos;
    }

    public void endSize16(int pos)
    {
        int cur = out.position();
        out.putShort(pos, (short) (cur - pos - 2));
    }

    public int beginSize32()
    {
        int pos = out.position();
        try
        {
            out.putInt(0);
        }
        catch (BufferOverflowException e)
        {
            grow(4);
            out.putInt(0);
        }
        return pos;
    }

    public void endSize32(int pos)
    {
        int cur = out.position();
        out.putInt(pos, (cur - pos - 4));
    }

	public void writeDouble(double aDouble)
	{
		try 
		{
			out.putDouble(aDouble);
		} catch(BufferOverflowException exception)
		{
			grow(8);
			out.putDouble(aDouble);
		}
	}

	public void writeInt16(short aShort)
	{
		try 
		{
			out.putShort(aShort);
		} catch(BufferOverflowException exception)
		{
			grow(2);
			out.putShort(aShort);
		}
	}

	public void writeInt32(int anInt)
	{
		try
		{
			out.putInt(anInt);
		} catch(BufferOverflowException exception)
		{
			grow(4);
			out.putInt(anInt);
		}
	}

	public void writeInt64(long aLong)
	{
		try
		{
			out.putLong(aLong);
		} catch(BufferOverflowException exception)
		{
			grow(8);
			out.putLong(aLong);
		}
	}
      
	public void writeInt8(byte aByte)
	{
		try 
		{
			out.put(aByte);	
		} catch(BufferOverflowException exception)
		{
			grow(1);
			out.put(aByte);
		}
	}	
	
	public void writeBin128(byte[] byteArray)
	{
		byteArray = (byteArray != null) ? byteArray : new byte [16];
		
		assert byteArray.length == 16;
		
		try 
		{
			out.put(byteArray);
		} catch(BufferOverflowException exception)
		{
			grow(16);
			out.put(byteArray);			
		}
	}

	public void writeFloat(float aFloat)
	{
		try 
		{
			out.putFloat(aFloat);
		} catch(BufferOverflowException exception)
		{
			grow(4);
			out.putFloat(aFloat);
		}
	}

	public void writeMagicNumber()
	{
		out.put("AM2".getBytes());
	}	
}