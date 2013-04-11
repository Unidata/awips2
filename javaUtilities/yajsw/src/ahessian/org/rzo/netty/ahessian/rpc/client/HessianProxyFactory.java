package org.rzo.netty.ahessian.rpc.client;

import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
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
import org.jboss.netty.channel.SimpleChannelHandler;
import org.jboss.netty.util.HashedWheelTimer;
import org.jboss.netty.util.Timeout;
import org.jboss.netty.util.Timer;
import org.jboss.netty.util.TimerTask;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.io.Hessian2Input;
import org.rzo.netty.ahessian.rpc.io.Hessian2Output;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyMessage;
import org.rzo.netty.ahessian.rpc.stream.ClientStreamManager;
import org.rzo.netty.ahessian.rpc.stream.InputStreamReplyMessage;
import org.rzo.netty.ahessian.session.ClientSessionFilter;
import org.rzo.netty.ahessian.utils.MyBlockingQueue;
import org.rzo.netty.ahessian.utils.MyLinkedBlockingQueue;
import org.rzo.netty.ahessian.utils.MyReentrantLock;
import org.rzo.netty.ahessian.utils.TimedBlockingPriorityQueue;

import com.caucho.hessian4.io.AbstractHessianInput;
import com.caucho.hessian4.io.AbstractHessianOutput;
import com.caucho.hessian4.io.HessianRemoteObject;

/**
 * Handles client side hessian rpc proxy invocations and is a factory for
 * service proxies. <br>
 * A typical setup for a protocol in a TCP/IP socket would be: <br>
 * 
 * <pre>
 * Executor executor = ...
 * HessianProxyFactory proxyFactory = ...
 * 
 * {@link ChannelPipeline} pipeline = ...;
 * pipeline.addLast(&quot;inputStream&quot;, new InputStreamDecoder(_executor));
 * pipeline.addLast(&quot;outputStream&quot;, new OutputStreamEncoder());        
 * pipeline.addLast(&quot;hessianReplyDecoder&quot;, new HessianRPCReplyDecoder(_factory));
 * pipeline.addLast(&quot;hessianCallEncoder&quot;, new HessianRPCCallEncoder());
 * pipeline.addLast(&quot;hessianHandler&quot;, proxyFactory);
 * </pre>
 * 
 * <br>
 * Typical usage within the client would be:
 * 
 * <pre>
 * 
 * ClientBootstrap bootstrap = ...
 * ChannelPipelineFactory pipelinetFactory = new ...(proxyFactory)
 * bootstrap.setPipelineFactory(...)
 * bootstrap.connect(...)
 * 
 * // get a service proxy 
 * Map options = new HashMap();
 * options.put(&quot;id&quot;, &quot;myServiceName&quot;);
 * // AsynchMyServiceInterface is an interface including the same methods as MyServiceInterface 
 * //except that the return type is always of type HessianProxyFuture
 * AsynchMyServiceInterface service = (AsynchMyServiceInterface) factory.create(AsynchMyServiceInterface.class, getClassLoader(), options);
 * 
 * // invoke a service method
 * HessianProxyFuture future = service.myMethod();
 * // wait for the result
 * // if an exception is thrown by the server the exception is thrown by the call to the get() method 
 * Object result = future.get();
 * </pre>
 */

@ChannelPipelineCoverage("all")
public class HessianProxyFactory extends SimpleChannelHandler implements Constants
{
	private volatile Map<Long, Future<Object>>						_openCalls				= Collections
																									.synchronizedMap(new HashMap<Long, Future<Object>>());
	private volatile int											_id						= 0;
	private volatile Channel										_channel				= null;
	private volatile com.caucho.hessian4.client.HessianProxyFactory	_factory				= null;
	private volatile MyBlockingQueue<HessianRPCCallMessage>			_pendingCalls;

	/** The _done listener. */
	Runnable														_doneListener;

	/** The _executor. */
	Executor														_executor;
	private Lock													_lock					= new MyReentrantLock();
	private Condition												_connected				= _lock.newCondition();

	/** The _stop. */
	boolean															_stop					= false;
	private String													_name;
	private boolean													_sessionListenerAdded	= false;
	private Runnable												_closedSessionListener;
	private Runnable												_newSessionListener;
	private Runnable												_disconnectedListener;
	private Runnable												_connectedListener;

	Map<Object, InvocationHandler>									_proxies				= Collections
																									.synchronizedMap(new HashMap<Object, InvocationHandler>());

	Timer															_timer					= new HashedWheelTimer();

	private volatile boolean										_blocked				= false;

	ClientStreamManager _clientStreamManager;
	/**
	 * Instantiates a new hessian proxy factory.
	 * 
	 * @param executor
	 *            the executor
	 */
	public HessianProxyFactory(Executor executor, String name)
	{
		this(executor, name, null, new HashMap());
	}

	public HessianProxyFactory(Executor executor, String name, Map options)
	{
		this(executor, name, null, options);
	}

	public HessianProxyFactory(Executor executor, String name, ClassLoader loader, Map options)
	{
		_executor = executor;
		_name = name;
		if (options != null)
			_pendingCalls = new TimedBlockingPriorityQueue<HessianRPCCallMessage>(options, null, "HessianProxyFactory-PendingCalls");
		else
			_pendingCalls = new MyLinkedBlockingQueue();
		if (loader == null)
			_factory = new com.caucho.hessian4.client.HessianProxyFactory();
		else
			_factory = new com.caucho.hessian4.client.HessianProxyFactory(loader);
		/*
		_executor.execute(new Runnable()
		{
			public void run()
			{
				Thread.currentThread().setName("HessianProxyFactory-Call-Tx");
				HessianRPCCallMessage message = null;
				while (!_stop)
				{
					// if previous message sent
					if (message == null)
						try
						{
							message = _pendingCalls.take();
						}
						catch (InterruptedException e1)
						{
							Constants.ahessianLogger.warn("", e1);
						}
					if (message == null)
						continue;
					_lock.lock();
					Channel channel = getChannel();
					while (channel == null || !channel.isConnected() && !_stop)
						try
						{
							_connected.await(1000, TimeUnit.MILLISECONDS);
							channel = getChannel();
						}
						catch (InterruptedException e)
						{
							Constants.ahessianLogger.warn("", e);
						}
					_lock.unlock();
					if (!_stop && message != null && message.getMethod() != null)
						try
						{
							ChannelFuture future = channel.write(message);
							future.await();
							if (future.isSuccess())
							{
//								if (_pendingCalls.size() == 0)
//									channel.write(new Integer(0));
								message = null;
							}
							else
								ahessianLogger.warn("cannot send message, will retry");
						}
						catch (Exception ex)
						{
							Constants.ahessianLogger.warn("", ex);
						}
						else if (message.getMethod() == null)
							message = null;

				}

			}
		});
		*/
	}

	/**
	 * Gets the hessian2 input.
	 * 
	 * @param is
	 *            the is
	 * 
	 * @return the hessian2 input
	 */
	public AbstractHessianInput getHessian2Input(InputStream is)
	{
		return new Hessian2Input(is);
	}

	public AbstractHessianOutput getHessian2Output(OutputStream out)
	{
		Hessian2Output out2 = new Hessian2Output(out);
		out2.setSerializerFactory(_factory.getSerializerFactory());
		return out2;
	}

	/**
	 * Checks if is overload enabled.
	 * 
	 * @return true, if is overload enabled
	 */
	public boolean isOverloadEnabled()
	{
		return _factory.isOverloadEnabled();
	}

	/**
	 * Send request.
	 * 
	 * @param methodName
	 *            the method name
	 * @param args
	 *            the args
	 * @param options
	 *            the options
	 * 
	 * @return the future< object>
	 * 
	 * @throws InterruptedException
	 *             the interrupted exception
	 */
	synchronized Future<Object> sendRequest(String methodName, Object[] args, Map options) throws InterruptedException
	{
		if (_blocked)
			throw new RuntimeException("send blocked");
		if (_stop)
			return null;
		Map<Object, Object> headers = options;
		final Long id = new Long(_id);
		_id++;
		headers.put(CALL_ID_HEADER_KEY, id);
		final HessianProxyFuture future = new HessianProxyFuture();
		future.handleCallbacks(args);
		final HessianRPCCallMessage message = new HessianRPCCallMessage(methodName, args, headers, null);
		_openCalls.put(id, future);
		Integer g = (Integer) options.get("group");
		final Integer group = g == null ? 0 : g;
		long timeout = _pendingCalls.getTimeout(group);
		if (timeout > 0)
		{
			TimerTask task = new TimerTask()
			{

				public void run(Timeout arg0) throws Exception
				{
					_openCalls.remove(id);
					future.timedOut();
				}

			};
			future.setTimeout(_timer.newTimeout(task, timeout, TimeUnit.MILLISECONDS));
		}
		while (getChannel() == null)
		{
			_lock.lock();
			try
			{
			_connected.await(1000, TimeUnit.MILLISECONDS);
			}
			finally
			{
			_lock.unlock();
			}
		}
		getChannel().write(message);
	
		return future;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jboss.netty.channel.SimpleChannelHandler#messageReceived(org.jboss
	 * .netty.channel.ChannelHandlerContext,
	 * org.jboss.netty.channel.MessageEvent)
	 */
	@Override
	public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception
	{
		if (e.getMessage() instanceof HessianRPCReplyMessage)
		{
			final HessianRPCReplyMessage message = (HessianRPCReplyMessage) e.getMessage();
			{
				final Long id = message.getCallId();
				if (id != null)
				{
					final HessianProxyFuture future = (HessianProxyFuture) _openCalls.get(id);
					if (future == null)
					{
						ahessianLogger.warn("no future found for call-id " + id);
						return;
					}
					if (message.getCompleted() == null || Boolean.TRUE.equals(message.getCompleted()))
						if ((!future.hasCallbacks()))
						{
							_openCalls.remove(id);
						}
					if (_doneListener != null && _openCalls.isEmpty())
						_doneListener.run();
					if (future != null)
						// setting message in future may fire listeners -> run
						// in separate thread
						_executor.execute(new Runnable()
						{
							public void run()
							{
								if (message.getValue() instanceof InputStreamReplyMessage)
								{
									InputStream stream = _clientStreamManager.newInputStream(((InputStreamReplyMessage)message.getValue()).getId());
									// caller should get a stream, not the reply
									message.setValue(stream);
								}
								future.set(message);
								// check in case this was a callback
								if (future.isDone())
									if (!future.hasCallbacks())
									{
										_openCalls.remove(id);

									}
							}
						});
					else
						ahessianLogger.warn("no future for call reply " + id + " " + message.getValue());
				}
				else
					ahessianLogger.warn("message missing id " + message);

			}
		}
		else if (e.getMessage() instanceof InputStreamReplyMessage)
		{
			_clientStreamManager.messageReceived((InputStreamReplyMessage)e.getMessage());
		}
		ctx.sendUpstream(e);
	}

	/**
	 * Creates a service proxy.
	 * 
	 * @param api
	 *            the "asynched" api of the service
	 * @param loader
	 *            the class loader for creating the proxy
	 * @param options
	 *            the options
	 * 
	 * @return the object
	 */
	public Object create(Class api, ClassLoader loader, Map options)
	{
		if (api == null)
			throw new NullPointerException("api must not be null for HessianProxyFactory.create()");
		InvocationHandler handler = null;

		handler = new AsyncHessianProxy(this, api, options);
		if (options.get("sync") != null)
			handler = new SyncHessianProxy(handler);

		Object result = Proxy.newProxyInstance(loader, new Class[]
		{ api, HessianRemoteObject.class }, handler);
		_proxies.put(result, handler);
		return result;

	}

	public void returnProxy(Object proxy)
	{
		Object handler = _proxies.remove(proxy);
		if (handler != null)
			if (handler instanceof SyncHessianProxy)
				handler = ((SyncHessianProxy) handler)._handler;
		((AsyncHessianProxy) handler).invalidate();
	}

	/**
	 * Gets the channel.
	 * 
	 * @return the channel
	 */
	public Channel getChannel()
	{
		return _channel;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jboss.netty.channel.SimpleChannelHandler#channelConnected(org.jboss
	 * .netty.channel.ChannelHandlerContext,
	 * org.jboss.netty.channel.ChannelStateEvent)
	 */
	public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		if (_connectedListener != null)
			try
			{
				_connectedListener.run();
			}
			catch (Throwable ex)
			{
				Constants.ahessianLogger.warn("", ex);
			}
		_lock.lock();
		try
		{
		if (!_sessionListenerAdded)
		{
			if (ctx.getPipeline().getContext(ClientSessionFilter.class) != null)
			{
				ClientSessionFilter sessionHandler = (ClientSessionFilter) ctx.getPipeline().getContext(ClientSessionFilter.class).getHandler();
				sessionHandler.addSessionClosedListener(new Runnable()
				{
					public void run()
					{
						_lock.lock();
						try
						{
						invalidateProxies();
						_openCalls.clear();

						_pendingCalls.clear();
						}
						finally
						{
						_lock.unlock();
						}
						if (_closedSessionListener != null)
							try
							{
								_closedSessionListener.run();
							}
							catch (Throwable ex)
							{
								Constants.ahessianLogger.warn("", ex);
							}
					}
				});
				sessionHandler.addSessionNewListener(new Runnable()
				{
					public void run()
					{
						if (_newSessionListener != null)
							try
							{
								_newSessionListener.run();
							}
							catch (Throwable ex)
							{
								Constants.ahessianLogger.warn("", ex);
							}
					}
				});
				_sessionListenerAdded = true;
			}
		}
		_channel = ctx.getChannel();
		super.channelConnected(ctx, e);
		_connected.signal();
		}
		finally
		{
		_lock.unlock();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jboss.netty.channel.SimpleChannelHandler#channelDisconnected(org.
	 * jboss.netty.channel.ChannelHandlerContext,
	 * org.jboss.netty.channel.ChannelStateEvent)
	 */
	public void channelDisconnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		_channel = null;
		// _stop = true;
		_lock.lock();
		try
		{
		_connected.signal();
		}
		finally
		{
		_lock.unlock();
		}
		// put something in the queue in case the worker thread hangs in
		// _pendingCalls.take()
		_pendingCalls.offer(new HessianRPCCallMessage(null, null, null, null));
		super.channelDisconnected(ctx, e);
		if (_disconnectedListener != null)
			try
			{
				_disconnectedListener.run();
			}
			catch (Throwable ex)
			{
				Constants.ahessianLogger.warn("", ex);
			}

	}

	/**
	 * Sets the done listener. This listener is fired whenever all requests have
	 * been completed
	 * 
	 * @param listener
	 *            the new listener
	 */
	public void setDoneListener(Runnable listener)
	{
		_doneListener = listener;
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent e)
	{
		ahessianLogger.warn("error accessing service " + _name + " Exception " + e.getCause().getClass() + " " + e.getCause().getMessage());
		ctx.getChannel().disconnect();
		ctx.getChannel().close();
		if (!_stop)
		{
			_channel = null;
			// _stop = true;
			_lock.lock();
			try
			{
			_connected.signal();
			}
			finally
			{
			_lock.unlock();
			}
			// put something in the queue in case the worker thread hangs in
			// _pendingCalls.take()
			_pendingCalls.offer(new HessianRPCCallMessage(null, null, null, null));
		}
	}

	public void invalidateProxies()
	{
		for (Object proxy : new HashSet(_proxies.keySet()))
		{
			returnProxy(proxy);
		}
	}

	public void setClosedSessionListener(Runnable listener)
	{
		_closedSessionListener = listener;
	}

	public void setDisconnectedListener(Runnable listener)
	{
		_disconnectedListener = listener;
	}

	public void setConnectedListener(Runnable listener)
	{
		_connectedListener = listener;
	}

	public void setNewSessionListener(Runnable listener)
	{
		_newSessionListener = listener;
	}

	public void invalidateAllPendingCalls()
	{

		final HessianRPCReplyMessage message = new HessianRPCReplyMessage(null, new RuntimeException("connection closed"), null);
		for (Future future : new ArrayList<Future>(_openCalls.values()))
		{
			((HessianProxyFuture) future).set(message);
		}
		_openCalls.clear();
		_pendingCalls.clear();
	}

	public void setBlocked(boolean blocked)
	{
		_blocked = blocked;
	}

}
