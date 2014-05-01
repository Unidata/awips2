package org.rzo.netty.ahessian.io;

import static org.jboss.netty.buffer.ChannelBuffers.dynamicBuffer;

import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.DownstreamMessageEvent;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.utils.MyReentrantLock;

import com.caucho.hessian4.io.FlushableOutput;

/**
 * A buffer for storing outgoing bytes. Bytes are sent upstream if
 * The number of written bytes is > than a watermark, or if flush is called
 */
public class OutputStreamBuffer extends OutputStream implements FlushableOutput
{
	
	/** The context of the channel on which to send the bytes downstream */
	private volatile ChannelHandlerContext _ctx;
	
	/** Indicates if the stream has been closed */
	private volatile boolean _closed = false;
	private Lock			_lock				= new MyReentrantLock();
	
	/** If written bytes > watermark, the bytes are sent downstream */
	int _watermark = 1024*1024;
	int _initialBuffSize = 1024;

	/** The buffer for storing outgoing bytes. Once the bytes have been sent downstream a new buffer is created */
	private ChannelBuffer _buf = dynamicBuffer(_initialBuffSize);
	
	
	/**
	 * Instantiates a new output stream buffer.
	 * 
	 * @param ctx the context in which bytes are sent downstream
	 */
	OutputStreamBuffer(ChannelHandlerContext ctx)
	{
		super();
		_ctx = ctx;
	}

	/* (non-Javadoc)
	 * @see java.io.OutputStream#write(int)
	 */
	@Override
	public void write(int b) throws IOException
	{
		if (_closed)
			throw new IOException("stream closed");
		_lock.lock();
		try
		{
		//System.out.println("write "+_buf.readableBytes());
		_buf.writeByte((byte)b);
		if (_buf.writerIndex() >= _watermark)
			sendDownstream(null);
		}
		finally
		{
		_lock.unlock();
		}
	}
	
	/* (non-Javadoc)
	 * @see java.io.OutputStream#write(byte[], int, int)
	 */
	@Override		
	public void write(byte b[], int off, int len) throws IOException
	{
		if (_closed)
			throw new IOException("stream closed");
		_lock.lock();
		try
		{
		_buf.writeBytes(b, off, len);
		//System.out.println("write "+len+" "+_buf.readableBytes());
		if (_buf.writerIndex() >= _watermark)
			sendDownstream(null);
		}
		finally
		{
        _lock.unlock();
		}

	}
	
	/* (non-Javadoc)
	 * @see java.io.OutputStream#flush()
	 */
	@Override
	public void flush() throws IOException
	{
		flush(null);
	}

	
	public void flush(ChannelFuture future) throws IOException
	{
		_lock.lock();
		if (_buf.readableBytes() > 0)
		try
		{
		super.flush();
		if (future == null)
		{
			ChannelFuture f = sendDownstream(null);
			f.await(20000);
			//if (!f.await(10000))
			//	throw new IOException("write longer than 10 secs");
		}
		else
		{
			sendDownstream(future);
		}
		}
		catch (Exception e)
		{
			throw new IOException(e);
		}
		finally
		{
		_lock.unlock();
		}
	}
	
	private ChannelFuture sendDownstream(ChannelFuture future) throws IOException
	{
		if (! _ctx.getChannel().isConnected())
			throw new IOException("channel disconnected");
		while (!_ctx.getChannel().isWritable())
			try
		{
				Thread.sleep(100);
		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.warn("",ex);
		}
		if (future == null)
		future = Channels.future(_ctx.getChannel());
        _ctx.sendDownstream(new DownstreamMessageEvent(_ctx.getChannel(), future, _buf, _ctx.getChannel().getRemoteAddress()));
		_buf = dynamicBuffer(1024);
		_buf.clear();
		return future;
	}
	
	/* (non-Javadoc)
	 * @see java.io.OutputStream#close()
	 */
	@Override		
	public void close() throws IOException
	{
		_lock.lock();
		_closed = true;
		_lock.unlock();
	}

	public void setContext(ChannelHandlerContext ctx)
	{
		_ctx = ctx;
		reset();
	}
	
	public ChannelHandlerContext getContext()
	{
		return _ctx;
	}

	public void reset()
	{
		_lock.lock();
		_buf = dynamicBuffer();
		_closed = false;
		_lock.unlock();
	}

}
