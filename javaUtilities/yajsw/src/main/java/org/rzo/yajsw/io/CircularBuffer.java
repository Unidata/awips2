/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */
package org.rzo.yajsw.io;

import java.io.IOException;
import java.io.Reader;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.rzo.yajsw.util.MyReentrantLock;


// TODO: Auto-generated Javadoc
/**
 * A Synchronized circular byte buffer. This buffer orders elements FIFO
 * (first-in-first-out). Writers block if the buffer is full. Attempts to
 * retrieve an element from an empty buffer will block.
 */
public class CircularBuffer extends Reader
{

	/** Default buffer size. */
	public final static int	DEFAULT_BUFFER_SIZE		= 512;

	/** The synchronization lock. */
	private Lock			lock					= new MyReentrantLock();

	/** Sync condition indicating that buffer is not empty. */
	private Condition		notEmpty				= lock.newCondition();

	/** The not full. */
	private Condition		notFull					= lock.newCondition();

	/** The buffer. */
	private byte[]			buffer;

	/** The number of valid elements in the buffe. */
	private int				size					= 0;

	/** The index to put next value. */
	private int				putIndex				= 0;

	/** The index to get next value. */
	private int				getIndex				= 0;

	/** The blocking. */
	boolean					blocking				= true;

	byte[]					fullIndicator;
	boolean					fullIndocatorWritten	= false;
	boolean					writeBlocking;
	int						fullIndicatorIndex		= -1;

	/**
	 * Instantiates a new circular buffer.
	 * 
	 * @param bufferSize
	 *            the buffer size
	 * @param blocking
	 *            the blocking
	 */
	public CircularBuffer(int bufferSize, boolean blocking)
	{
		buffer = new byte[bufferSize];
		this.blocking = blocking;
		writeBlocking = blocking;
	}

	public void setWriteBlocking(boolean blocking)
	{
		writeBlocking = blocking;
	}

	/**
	 * Instantiates a new circular buffer with default size.
	 */
	public CircularBuffer()
	{
		buffer = new byte[DEFAULT_BUFFER_SIZE];
	}

	/**
	 * Put a value into the buffer. If buffer is full older values are
	 * overwritten.
	 * 
	 * @param value
	 *            the value
	 */
	public void put(byte value)
	{
		lock.lock(); // lock this object
		// while no empty locations, place thread in waiting state
		try
		{
			while (size == buffer.length)
			{
				if (writeBlocking)
				{
					// System.out.println("wait write");
					notFull.await();// await until a buffer element is free
				}
				else
				{
					lock.unlock();
					return;
				}
			} // end while
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}

		buffer[putIndex] = value; // set new buffer value

		putByte(value);

		notEmpty.signal(); // signal threads waiting to read from buffer
		lock.unlock(); // unlock this object
	} // end method put

	private void writeFullIndicator()
	{
		fullIndocatorWritten = true;
	}

	/**
	 * Put the values of a byte array into the buffer. In case of overflow put
	 * blocks
	 * 
	 * @param buf
	 *            the buf
	 * @param off
	 *            the off
	 * @param len
	 *            the len
	 */
	public void put(byte[] buf, int off, int len)
	{
		lock.lock(); // lock this object
		for (int i = off; i < off + len - 1; i++)
			putByte(buf[i]);
		notEmpty.signal(); // signal threads waiting to read from buffer
		lock.unlock(); // unlock this object

	}

	/**
	 * Put a single byte into the buffer.
	 * 
	 * @param value
	 *            the value
	 */
	private void putByte(byte value)
	{
		buffer[putIndex] = value; // set new buffer value

		// update circular write index
		putIndex++;
		if (putIndex >= buffer.length)
			putIndex = putIndex - buffer.length;

		size++; // one more buffer element is full

		// if buffer overflow
		if (size > buffer.length)
		{
			getIndex++;
			if (getIndex >= buffer.length)
				getIndex = getIndex - buffer.length;
			size = buffer.length;
			// System.out.println("overflow");
			if (fullIndicator != null && !fullIndocatorWritten)
				writeFullIndicator();
		}

	}

	/**
	 * Get next value from the buffer. Blocks indefinitely if buffer is empty.
	 * 
	 * @return the byte
	 */
	public byte get()
	{
		byte result = 0; // initialize value read from buffer
		lock.lock(); // lock this object

		// wait until buffer has data, then read value
		try
		{
			// while no data to read, place thread in waiting state
			while (size == 0)
			{
				if (blocking)
					notEmpty.await(); // await until a buffer element is
				// filled
				else
				{
					lock.unlock();
					return 0;
				}
			} // end while

			result = getByte();
			notFull.signal();
		} // end try
		// if waiting thread interrupted, print stack trace
		catch (InterruptedException exception)
		{
			exception.printStackTrace();
			Thread.currentThread().interrupt();
		} // end catch
		finally
		{
			lock.unlock(); // unlock this object
		} // end finally

		return result;
	} // end method get

	/**
	 * Get bytes from the buffer and return these in an array. Blocks if the
	 * buffer is empty and no values have yet been added to the array.
	 * 
	 * @param buf
	 *            array to return bytes
	 * @param off
	 *            the off
	 * @param len
	 *            the len
	 * 
	 * @return the number of bytes returned in the array
	 */
	public int get(byte[] buf, int off, int len)
	{
		lock.lock(); // lock this object
		int i = 0;

		try
		{
			// while no data to read, place thread in waiting state
			while (size == 0)
			{
				// System.out.println("read wait");
				notEmpty.await(); // await until a buffer element is
				// filled
			} // end while
		} // end try
		// if waiting thread interrupted, print stack trace
		catch (Exception exception)
		{
			exception.printStackTrace();
		} // end catch

		for (; i < len && i < size; i++)
		{
			buf[off + i] = getByte();
		}
		notFull.signal();
		lock.unlock(); // unlock this object

		return i;

	}

	/**
	 * Gets the byte.
	 * 
	 * @return the byte
	 */
	private byte getByte()
	{
		if (fullIndocatorWritten)
		{
			fullIndicatorIndex++;
			if (fullIndicatorIndex < fullIndicator.length)
			{
				return fullIndicator[fullIndicatorIndex];
			}
			else
			{
				fullIndicatorIndex = -1;
				fullIndocatorWritten = false;
			}
		}
		if (size == 0)
			return 0;
		byte result;
		result = buffer[getIndex]; // read value from buffer

		// update circular read index
		getIndex++;
		if (getIndex >= buffer.length)
			getIndex = getIndex - buffer.length;

		size--; // one more buffer element is empty
		return result;
	}

	/**
	 * Size.
	 * 
	 * @return the int
	 */
	public int size()
	{
		return size;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.Reader#close()
	 */
	public void close()
	{
		lock.lock();
		notFull.signal();
		notEmpty.signal();
		size = 0;
		putIndex = 0;
		getIndex = 0;
		lock.unlock();
	}

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args)
	{
		CircularBuffer b = new CircularBuffer(2, true);
		b.put((byte) 1);
		b.put((byte) 2);
		b.put((byte) 3);
		System.out.println(b.get());
		System.out.println(b.get());
		b.put(new byte[]
		{ 1, 2, 3, 4 }, 0, 4);
		System.out.println(b.get(new byte[10], 0, 10));
		System.out.println(b.get(new byte[10], 0, 10));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.Reader#read(char[], int, int)
	 */
	@Override
	public int read(char[] cbuf, int off, int len) throws IOException
	{
		lock.lock(); // lock this object
		int i = 0;

		try
		{
			// while no data to read, place thread in waiting state
			while (size == 0)
			{
				if (blocking)
					notEmpty.await(); // await until a buffer element is
				// filled
				else
				{
					lock.unlock();
					return -1;
				}
			} // end while
		} // end try
		// if waiting thread interrupted, print stack trace
		catch (Exception exception)
		{
			exception.printStackTrace();
		} // end catch

		for (; i < len && i < size; i++)
		{
			cbuf[off + i] = (char) getByte();
		}
		notFull.signal();
		lock.unlock(); // unlock this object

		return i;
	}

	/**
	 * Write.
	 * 
	 * @param cbuf
	 *            the cbuf
	 * @param off
	 *            the off
	 * @param len
	 *            the len
	 */
	public void write(char[] cbuf, int off, int len)
	{
		lock.lock(); // lock this object
		for (int i = off; i < off + len - 1; i++)
			putByte((byte) cbuf[i]);
		notEmpty.signal(); // signal threads waiting to read from buffer
		lock.unlock(); // unlock this object
	}

	/**
	 * Write.
	 * 
	 * @param str
	 *            the str
	 */
	public void write(String str)
	{
		char[] dst = new char[str.length() + 2];
		str.getChars(0, str.length(), dst, 0);
		dst[str.length()] = '\r';
		dst[str.length()] = '\n';
		write(dst, 0, dst.length);
	}

	public void setFullIndicator(String text)
	{
		fullIndicator = text.getBytes();
	}
} // end class CircularBuffer

