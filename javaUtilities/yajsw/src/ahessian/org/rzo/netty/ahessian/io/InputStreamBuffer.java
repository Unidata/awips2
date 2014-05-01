package org.rzo.netty.ahessian.io;

import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.jboss.netty.buffer.ChannelBuffer;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.utils.MyReentrantLock;

/**
 * InputStreamBuffer pipes bytes read from the channel to an input stream
 */
public class InputStreamBuffer extends InputStream
{

	/** Buffer for storing incoming bytes */
	// private ChannelBuffer _buf = dynamicBuffer();
	final LinkedList<ChannelBuffer>	_bufs			= new LinkedList<ChannelBuffer>();

	/** Indicates if the stream has been closed */
	private volatile boolean		_closed			= false;
	final private Lock				_lock			= new MyReentrantLock();
	/** Sync condition indicating that buffer is not empty. */
	final private Condition			_notEmpty		= _lock.newCondition();
	private volatile int			_available		= 0;
	boolean							blocking		= false;
	long							_readTimeout	= 3000;
	// close stream on empty buffer
	boolean _closeOnEmpty = false;

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.InputStream#read()
	 */
	@Override
	public int read() throws IOException
	{
		int result = -1;
		if (_closed)
			return -1;
		checkCloseOnEmpty();
		_lock.lock();
		try
		{
			while (!_closed && available() == 0)
				if (blocking)
				{
					if (_readTimeout > 0)
					{
						if (!_notEmpty.await(_readTimeout, TimeUnit.MILLISECONDS))
							throw new IOException("read timeout");
					}
					else
						_notEmpty.await();
				}
				else
				{
					throw new IOException("no data");
				}
			if (!_closed)
			{
				result = (int) _bufs.getFirst().readByte() & 0xFF;
				_available--;
				checkBufs();
			}
		}
		catch (Exception ex)
		{
			throw new IOException(ex.getMessage());
		}
		finally
		{
			_lock.unlock();
		}
		
		//System.out.println("read "+_available);
		checkCloseOnEmpty();				 
		return result;
	}

	private void checkBufs()
	{
		if (!_bufs.isEmpty() && _bufs.getFirst().readableBytes() == 0)
		{
			_bufs.removeFirst();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.InputStream#close()
	 */
	@Override
	public void close() throws IOException
	{
		_lock.lock();
		try
		{
			_closed = true;
			_notEmpty.signal();
			super.close();
		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.warn("error closing input stream", ex);
		}
		finally
		{
			_lock.unlock();
		}
	}

	/**
	 * Insert bytes to the input stream
	 * 
	 * @param buf
	 *            bytes received from previous upstream handler
	 * 
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public void write(ChannelBuffer buf) throws IOException
	{
		if (_closed)
			throw new IOException("stream closed");
		_lock.lock();
		// if (_available == 0)
		// System.out.println("input not empty");
		try
		{
			if (_bufs.isEmpty() || buf != _bufs.getLast())
			{
				_bufs.addLast(buf);
			}
			_available += buf.readableBytes();
			_notEmpty.signal();
		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.warn("", ex);
		}
		finally
		{
			_lock.unlock();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.InputStream#available()
	 */
	public int available() throws IOException
	{
		if (_closed)
			throw new IOException("stream closed");
		return _available;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.InputStream#read(byte[], int, int)
	 */
	@Override
	public int read(byte[] b, int off, int len) throws IOException
	{
		int result = -1;
		if (_closed)
			return -1;
		_lock.lock();
		try
		{
			while (!_closed && available() == 0)
			{
				checkCloseOnEmpty();
				if (_readTimeout > 0)
				{
					if (!_notEmpty.await(_readTimeout, TimeUnit.MILLISECONDS))
						throw new IOException("read timeout");
				}
				else
					_notEmpty.awaitUninterruptibly();
			}
			if (!_closed)
			{
				int length = Math.min(len, _bufs.getFirst().readableBytes());
				_bufs.getFirst().readBytes(b, off, length);
				result = length;
				_available -= length;
				checkBufs();
				// if (_available == 0)
				// System.out.println("input empty");
			}
		}
		catch (Exception ex)
		{
			throw new IOException(ex.getMessage());
		}
		finally
		{
			_lock.unlock();
		}
		checkCloseOnEmpty();
		return result;
	}

	/**
	 * Checks if the stream is closed.
	 * 
	 * @return true, if is closed
	 */
	public boolean isClosed()
	{
		return _closed;
	}

	public void setReadTimeout(long timeout)
	{
		_readTimeout = timeout;
	}

	public boolean isBlocking()
	{
		return blocking;
	}

	public void setBlocking(boolean blocking)
	{
		this.blocking = blocking;
	}
	
	private void checkCloseOnEmpty() throws IOException
	{
		if (_closeOnEmpty && !_closed && available() == 0)
			close();
	}
	
	public void closeOnEmpty() throws IOException
	{
		_closeOnEmpty = true;
		checkCloseOnEmpty();
	}

}
