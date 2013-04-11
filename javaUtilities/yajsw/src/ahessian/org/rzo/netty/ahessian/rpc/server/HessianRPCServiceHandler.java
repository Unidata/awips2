package org.rzo.netty.ahessian.rpc.server;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.jboss.netty.util.Timer;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyMessage;
import org.rzo.netty.ahessian.utils.MyReentrantLock;
import org.rzo.netty.ahessian.utils.TimedBlockingPriorityQueue;

/**
 * Handles server side hessian rpc calls. <br>
 * A typical setup for a protocol in a TCP/IP socket would be:
 * 
 * <pre>
 * {@link ChannelPipeline} pipeline = ...;
 *  pipeline.addLast(&quot;inputStream&quot;, new InputStreamDecoder(_executor));
 *  pipeline.addLast(&quot;outputStream&quot;, new OutputStreamEncoder());
 *  pipeline.addLast(&quot;callDecoder&quot;, new HessianRPCCallDecoder());
 *  pipeline.addLast(&quot;replyEncoder&quot;, new HessianRPCReplyEncoder());
 *  HessianRPCServiceHandler handler =  new HessianRPCServiceHandler(executor);
 *  {@link Service} service = ...
 *  {@link Executor} executor = ...
 *  factory.addService(&quot;default&quot;, new ContinuationService(service, ServiceApi.class, handler, executor));
 *  pipeline.addLast(&quot;hessianRPCServer&quot;, handler);
 * </pre>
 */
@ChannelPipelineCoverage("one")
public class HessianRPCServiceHandler extends SimpleChannelUpstreamHandler implements Constants
{

	/** maps service names to services. */
	private Map<String, HessianSkeleton>				_services	= new HashMap<String, HessianSkeleton>();
	/** queue of pending replies. */
	private TimedBlockingPriorityQueue<HessianRPCReplyMessage>	_pendingReplies;
	private TimedBlockingPriorityQueue<HessianRPCCallMessage>		_pendingCalls;
	private LinkedBlockingQueue<HessianRPCReplyMessage>	_repliesRetry;

	/** thread pool to get a thread to send the replies */
	private Executor									_executor;

	/** TODO indicates that execution should be stopped */
	private boolean										_stop		= false;
	
	final AtomicLong _openCounter = new AtomicLong(0);
	final Lock _lock = new MyReentrantLock();
	final Condition _channelOpen = _lock.newCondition();

	public HessianRPCServiceHandler(Executor executor)
	{
		this(executor, null, null);
	}

	/**
	 * Instantiates a new hessian rpc service handler.
	 * 
	 * @param executor
	 *            the thread pool to get a thread to send replies
	 */
	public HessianRPCServiceHandler(Executor executor, Map<String, Object> options, Timer timer)
	{
		_executor = executor;
		if (options == null || timer == null)
			_pendingReplies = new TimedBlockingPriorityQueue<HessianRPCReplyMessage>("HessianRPCServiceHandler-PendingReplies");
		else
			_pendingReplies = new TimedBlockingPriorityQueue<HessianRPCReplyMessage>(options, null, "HessianRPCServiceHandler-PendingReplies");

		if (options == null || timer == null)
			_pendingCalls = new TimedBlockingPriorityQueue<HessianRPCCallMessage>("HessianRPCServiceHandler-PendingCalls");
		else
			_pendingCalls = new TimedBlockingPriorityQueue<HessianRPCCallMessage>(options, null, "HessianRPCServiceHandler-PendingCalls");

			_repliesRetry = new LinkedBlockingQueue<HessianRPCReplyMessage>();

		_executor.execute(new Runnable()
		{
			public void run()
			{
				Thread.currentThread().setName("HessianRPCServiceHandler-Call-Rx");
				HessianRPCCallMessage message = null;
				while (!_stop)
					try
					{
						message = _pendingCalls.take();
						HessianSkeleton service = getService(message);
						service.messageReceived(message);
					}
					catch (Exception ex)
					{
						Constants.ahessianLogger.warn("", ex);
					}
			}
		});
/*
		_executor.execute(new Runnable()
		{
			public void run()
			{
				Thread.currentThread().setName("HessianRPCServiceHandler-Reply-Tx");
				while (!_stop)
				{
					if (_openCounter.get() == 0)
					{
						//System.out.println("waiting for an open channel");
						_lock.lock();
						try
						{
							_channelOpen.await();
						}
						catch (InterruptedException e2)
						{
							Constants.ahessianLogger.warn("", e2);
						}
						finally
						{
							_lock.unlock();
							//System.out.println("got an open channel");
						}
					}
					HessianRPCReplyMessage message = null;
					try
					{
						message = _pendingReplies.take();
					}
					catch (InterruptedException e1)
					{
						Constants.ahessianLogger.warn("", e1);
					}
					if (message == null)
						continue;
					sendMessage(message);

				}
			}
		});
		*/
		
/*		
		_executor.execute(new Runnable()
		{
			public void run()
			{
				Thread.currentThread().setName("HessianRPCServiceHandler-ReplyRetry-Tx");
				int counter = 0;
				while (!_stop)
				{
					if (_openCounter.get() == 0)
					{
						//System.out.println("waiting for an open channel");
						_lock.lock();
						try
						{
							_channelOpen.await();
						}
						catch (InterruptedException e2)
						{
							Constants.ahessianLogger.warn("", e2);
						}
						finally
						{
							_lock.unlock();
							//System.out.println("got an open channel");
						}
					}
					HessianRPCReplyMessage message = null;
					try
					{
						message = _repliesRetry.take();
					}
					catch (InterruptedException e1)
					{
						Constants.ahessianLogger.warn("", e1);
					}
					if (message == null)
						continue;
					if (counter == 0)
						counter = _repliesRetry.size()+1;
					sendMessage(message);
					if (counter-- == 0)
					{
						try
						{
							Thread.sleep(1000);
						}
						catch (InterruptedException e)
						{
							e.printStackTrace();
						}
					}

				}
			}
		});
*/
	}
	
	

	protected void sendMessage(HessianRPCReplyMessage message)
	{
		Channel ch = message.getChannel();
		if (ch != null)
			ch.write(message);
		else
			ahessianLogger.warn("message channel null -> ignored: #"+message.getCallId());
		/*
		//ahessianLogger.warn("send reply for #"+message.getHeaders().get(CALL_ID_STRING));
		if (message.isValid())
		{
			final Channel ch = message.getChannel();
			final HessianRPCReplyMessage mess = message;
			if (ch != null && ch.isConnected())
			{
			_executor.execute(new Runnable()
			{

				public void run()
				{
					//ahessianLogger.info("new thread for #"+mess.getHeaders().get(CALL_ID_STRING));
					String tName = Thread.currentThread().getName();
					//Thread.currentThread().setName("HessianRPCServiceHandler-Reply-Encod");
					try
					{
						ChannelFuture future = ch.write(mess);
						future.await();
			
//						if (_pendingCalls.size() == 0)
//						{
//							ch.write(new Integer(0));
//						}
					}
					catch (Exception ex)
					{
						Constants.ahessianLogger.warn("", ex);
						try
						{
							ahessianLogger.warn("message write threw an exception, retry later: #"+mess.getCallId());
							_pendingReplies.put(mess);
						}
						catch (InterruptedException e)
						{
							Constants.ahessianLogger.warn("", e);
						}

					}
					//Thread.currentThread().setName(tName);
				}
			});
			}
			else
			{
				try
				{
					ahessianLogger.warn("cannot send message, channel closed, retry later: #"+message.getCallId());
					_repliesRetry.put(message);
				}
				catch (InterruptedException e)
				{
					Constants.ahessianLogger.warn("", e);
				}
			}
		}
		else
		{
			ahessianLogger.warn("message invalid -> igonore: #"+message.getCallId());
		}
		*/
	}

	/**
	 * Adds a service to the handler.
	 * 
	 * @param name
	 *            the name of the service
	 * @param service
	 *            the service wrapper
	 */
	public void addService(String name, HessianSkeleton service)
	{
		_services.put(name, service);
	}

	/**
	 * Removes a service.
	 * 
	 * @param name
	 *            the name
	 */
	public void removeService(String name)
	{
		_services.remove(name);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jboss.netty.channel.SimpleChannelUpstreamHandler#messageReceived(
	 * org.jboss.netty.channel.ChannelHandlerContext,
	 * org.jboss.netty.channel.MessageEvent)
	 */
	@Override
	public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception
	{
		HessianRPCCallMessage message = (HessianRPCCallMessage) e.getMessage();
		Integer group = (Integer) message.getHeaders().get(Constants.GROUP_HEADER_KEY);
		_pendingCalls.put(message, group);

	}

	private HessianSkeleton getService(HessianRPCCallMessage message)
	{
		String id = (String) message.getHeaders().get(SERVICE_ID_HEADER_KEY);
		if (id == null)
			id = "default";
		return _services.get(id);
	}

	public void writeResult(HessianRPCReplyMessage message)
	{
		/*
		try
		{
			Integer group = message.getGroup();
			if (group == null)
				_pendingReplies.put(message);
			else
				_pendingReplies.put(message, group.intValue());
		}
		catch (InterruptedException e)
		{
			Constants.ahessianLogger.warn("", e);
		}
		*/
		sendMessage(message);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jboss.netty.channel.SimpleChannelUpstreamHandler#channelConnected
	 * (org.jboss.netty.channel.ChannelHandlerContext,
	 * org.jboss.netty.channel.ChannelStateEvent)
	 */
	public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		ahessianLogger.warn(ctx.getChannel()+" connected");
		if (_openCounter.incrementAndGet() == 1)
		{
		_lock.lock();
		try
		{
			_channelOpen.signal();
		}
		catch (Exception ex)
		{
			
		}
		finally
		{
			_lock.unlock();
		}
		}
		
		
		super.channelOpen(ctx, e);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jboss.netty.channel.SimpleChannelUpstreamHandler#channelDisconnected
	 * (org.jboss.netty.channel.ChannelHandlerContext,
	 * org.jboss.netty.channel.ChannelStateEvent)
	 */
	public void channelDisconnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		ahessianLogger.warn(ctx.getChannel()+" disconnected");

		_openCounter.decrementAndGet();
		super.channelClosed(ctx, e);
	}
	
	@Override
	public void exceptionCaught(
            ChannelHandlerContext ctx, ExceptionEvent e) throws Exception
            {
				ahessianLogger.warn(ctx.getChannel()+" exception "+ e.getCause());
            }
	//TODO
	public void stop()
	{
		_stop = true;
	}

}
