package org.rzo.netty.ahessian.io;

import java.io.InputStream;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.utils.MyReentrantLock;

public class PushInputStreamConsumer extends SimpleChannelUpstreamHandler
{

	volatile Lock _lock = new MyReentrantLock();
	AtomicInteger _consumerThreadsCount = new AtomicInteger(0);
	
	volatile InputStreamConsumer _consumer;
	volatile Executor _executor;
	
	public PushInputStreamConsumer(InputStreamConsumer consumer, Executor executor)
	{
		_consumer = consumer;
		_executor = executor;
	}
	
	@Override
	public void messageReceived(final ChannelHandlerContext ctx, final MessageEvent evt) throws Exception
	{
		// input stream is consumed within a separate thread
		// we return the current worker thread to netty, so that it may continue feeding the input stream
		_executor.execute(new Runnable()
		{
			public void run()
			{
				String tName = Thread.currentThread().getName();
				try
				{
				PushInputStreamConsumer.this.run(ctx, evt);
				}
				finally
				{
				Thread.currentThread().setName(tName);
				_consumerThreadsCount.decrementAndGet();
				}
			}
		});

	}
	
	private void run(ChannelHandlerContext ctx, MessageEvent evt)
	{
		if (_consumer.isBufferEmpty())
		{
			// we have nothing to consume
			return;
		}
		
		if (_consumerThreadsCount.incrementAndGet() > 2)
		{
			// there is already a thread consuming and another at the gate to consume the last chunk
			_consumerThreadsCount.decrementAndGet();
			return;
		}

		Thread.currentThread().setName("ahessian-PushInputStreamConsumer-#"+_consumerThreadsCount.get());

		
		// consume only with one thread at a time
		_lock.lock();
		try
		{
			_consumer.consume(ctx, (InputStream)evt.getMessage());
		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.warn("", ex);
		}
		finally
		{
			_consumerThreadsCount.decrementAndGet();
			_lock.unlock();
		}
	
}
	
    public void channelConnected(
            ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception 
            {
    	_lock.lock();
    	try
    	{
    	_consumer.setContext(ctx);
    	}
    	finally
    	{
    	_lock.unlock();
    	}
        ctx.sendUpstream(e);
    }


	
}
