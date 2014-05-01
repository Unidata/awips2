package org.rzo.netty.ahessian.io;

import java.io.InputStream;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.stopable.StopableHandler;
import org.rzo.netty.ahessian.utils.MyReentrantLock;

public class PullInputStreamConsumer extends SimpleChannelUpstreamHandler implements StopableHandler
{
	final InputStreamConsumer _consumer;
	final Executor _executor;
	final Lock _lock = new MyReentrantLock();
	final Condition _hasData = _lock.newCondition();
	volatile boolean _stop = false;
	volatile ChannelHandlerContext _ctx;
	volatile InputStream _inputStream;
	volatile boolean _waiting = false;
	static AtomicInteger _threadCounter = new AtomicInteger(0);
	private boolean	_stopEnabled = true;
	
	
	public PullInputStreamConsumer(InputStreamConsumer consumer, Executor executor)
	{
		_consumer = consumer;
		_executor = executor;
		
		_executor.execute(new Runnable()
		{
			public void run()
			{
				String tName = Thread.currentThread().getName();
				Thread.currentThread().setName("ahessian-PullInputStreamConsumer-#"+_threadCounter.incrementAndGet());
				try
				{
				waitForData();
				while (!_stop)
				{
					_consumer.consume(_ctx, _inputStream);
					waitForData();
					}
				}
				finally
				{
					Thread.currentThread().setName(tName);
					_threadCounter.decrementAndGet();
				}
			}
		});
	}
	
	private void waitForData()
	{
		//System.out.println("wait for data");
		while (! _stop &&( _consumer == null || _consumer.isBufferEmpty() || _ctx == null || !_ctx.getChannel().isConnected()))
		{

		_lock.lock();
		try
		{
			_waiting = true;
			_hasData.await(500, TimeUnit.MILLISECONDS);
		}
		catch (InterruptedException e)
		{
			Constants.ahessianLogger.warn("", e);
		}
		finally
		{
		_waiting = false;
		_lock.unlock();
		}
		}
		//System.out.println("got data");
	}
	

	
	@Override
	public void messageReceived(ChannelHandlerContext ctx, MessageEvent evnt) throws Exception
	{
			if (_ctx != ctx)
				_ctx = ctx;
			if (_inputStream != evnt.getMessage())
			{
				_inputStream = (InputStream) evnt.getMessage();
			((InputStreamBuffer)_inputStream).setReadTimeout(-1);
			}
			if (_waiting)
			{
		_lock.lock();
		try
		{
		_hasData.signal();
		}
		finally
		{
		_lock.unlock();
		}
		}
	}
	
    public void channelConnected(
            ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception 
            {
    	_lock.lock();
    	try
    	{
    	_consumer.setContext(ctx);
    	_ctx = ctx;
    	}
    	finally
    	{
    	_lock.unlock();
    	}
        ctx.sendUpstream(e);
    }
    
	public boolean isStopEnabled()
	{
		return _stopEnabled ;
	}

	public void setStopEnabled(boolean stopEnabled)
	{
		_stopEnabled = stopEnabled;
	}

	public void stop()
	{
			_stop = true;
	}
	
	

	


}
