package org.rzo.netty.ahessian.rpc.message;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Timer;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.rzo.netty.ahessian.stopable.StopableHandler;
import org.rzo.netty.ahessian.utils.MyReentrantLock;
import org.rzo.netty.ahessian.utils.TimedBlockingPriorityQueue;


public class OutputProducer extends SimpleChannelHandler implements StopableHandler
{
	private TimedBlockingPriorityQueue<MessageEvent>		_pendingCalls = new TimedBlockingPriorityQueue("OutputProducer");
	
	
	AtomicInteger _producerThreadsCount = new AtomicInteger(0);
	Lock _lock = new MyReentrantLock();

	Executor _executor;
	Timer _timer;
	List<MessageEvent> _pendingTermination = new ArrayList<MessageEvent>();
	
	volatile boolean _stop = false;
	
	public OutputProducer(Executor executor)
	{
		_executor = executor;
	}
	
	public void writeRequested(final ChannelHandlerContext ctx, MessageEvent e) throws Exception
	{
		//System.out.println(Thread.currentThread()+ " OutputProducer writeRequesed");
		GroupedMessage m = (GroupedMessage) e.getMessage();
		_pendingCalls.put(e, m.getGroup());
		if (_producerThreadsCount.get() < 2)
			_executor.execute(new Runnable()
			{

				public void run()
				{
					produce(ctx);
				}
				
			});
		
	}
	@Override
	public void channelConnected(final ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		ctx.sendUpstream(e);
		_executor.execute(new Runnable()
		{
			public void run()
			{
				for (Iterator it = _pendingTermination.iterator(); it.hasNext(); )
				{
					_lock.lock();
					try
					{
						if (_stop)
							return;
					MessageEvent e = (MessageEvent) it.next();
					GroupedMessage m = (GroupedMessage) e.getMessage();
					_pendingCalls.put(e, m.getGroup());
					}
					catch (Exception ex)
					{
						ex.printStackTrace();
					}
					finally
					{
						_lock.unlock();
					}
				}
				produce(ctx);
			}
		});
	}
	
	private void produce(ChannelHandlerContext ctx)
	{
		if (_stop)
			return;

		if (_producerThreadsCount.incrementAndGet() > 2)
		{
			// there is already a thread consuming and another at the gate to consume the last chunk
			_producerThreadsCount.decrementAndGet();
			return;
		}
		//System.out.println(Thread.currentThread()+" produce");
		boolean produced = false;
		_lock.lock();
		try
		{
			MessageEvent toSend = null;
		while (ctx.getChannel().isConnected() && _pendingCalls.size() > 0)
		{
			if (_stop)
				return;

		try
		{
			toSend = _pendingCalls.take();
			//System.out.println(Thread.currentThread()+ " OutputProducer sendMessage");
			ctx.sendDownstream(toSend);
			_pendingTermination.add(toSend);
			produced = true;
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			_pendingCalls.put(toSend, ((GroupedMessage) toSend.getMessage()).getGroup());
		}
		}
		if (produced && _pendingCalls.size() == 0)
		{
			//System.out.println(Thread.currentThread()+ " OutputProducer flush");
			Channels.write(ctx, Channels.future(ctx.getChannel()), new FlushRequestMessage());
			for (Iterator it = _pendingTermination.iterator(); it.hasNext(); )
			{
				if (_stop)
					return;

				try
				{
				MessageEvent e = (MessageEvent) it.next();
				GroupedMessage m = (GroupedMessage) e.getMessage();
				it.remove();
				e.getFuture().setSuccess();
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
			}
		}

		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		finally
		{
			_producerThreadsCount.decrementAndGet();
			_lock.unlock();
		}

	}

	public boolean isStopEnabled()
	{
		return true;
	}

	public void setStopEnabled(boolean stopEnabled)
	{
	}

	public void stop()
	{
		_stop = true;
		for (MessageEvent event : _pendingCalls)
		{
			event.getFuture().cancel();
		}
	}


}
