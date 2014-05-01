package org.rzo.netty.ahessian.session;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.jboss.netty.util.Timeout;
import org.jboss.netty.util.Timer;
import org.jboss.netty.util.TimerTask;
import org.rzo.netty.ahessian.Constants;

/**
 * Handles sessions on the server side. A typical setup for a
 * protocol in a TCP/IP socket would be:
 * 
 * <pre>
 * // _mixinFactory is a ChannelPipelineFactory which returns MixinPipeline
 * {@link ChannelPipeline} pipeline = ...;
 * 
 * pipeline.addLast(&quot;sessionFilter&quot;, new ServerSessionFilter(_mixinFactory));
 * </pre>
 */
public class ServerSessionFilter extends SimpleChannelUpstreamHandler
{
	
	/** Indicates if session has been assigned to the current channel */
	private boolean								_hasSession			= false;
	
	/** String for reading in a session id */
	private String								_sessionId			= "";
	
	/** Factory for creating new session objects */
	private SessionFactory						_factory			= new SessionFactory();
	
	/** Connected event is intercepted. It is sent upstream only after a session has been established*/
	private ChannelStateEvent					_connectedEvent;
	
	/** A pipeline factory which returns a MixinPipeline */
	private ChannelPipelineFactory				_mixinFactory;
	
	/** Assignment of session-id to the associated MixinPipeline */
	private static Map<String, MixinPipeline>	_sessionPipelines	= Collections.synchronizedMap(new HashMap<String, MixinPipeline>());
	
	private long _sessionTimeout = -1;
	
	private Timer _timer = null;
	
	private volatile Channel _channel = null;
	
	private volatile boolean _valid = true;

	/**
	 * Instantiates a new server session filter.
	 * 
	 * @param mixinFactory a pipeline factory which returns MixinPipeline
	 */
	public ServerSessionFilter(ChannelPipelineFactory mixinFactory, Timer timer, long sessionTimeout)
	{
		_mixinFactory = mixinFactory;
		_timer = timer;
		_sessionTimeout = sessionTimeout;
	}

	public ServerSessionFilter(ChannelPipelineFactory mixinFactory)
	{
		this(mixinFactory, null, -1);
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.SimpleChannelUpstreamHandler#messageReceived(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.MessageEvent)
	 */
	public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception
	{
		// if session established forward all messages
		if (_hasSession)
		{
			Session session = ((Session)ctx.getAttachment());
			session.onMessage();
			ctx.sendUpstream(e);
		}
		else
		{
			ChannelBuffer b = (ChannelBuffer) e.getMessage();
			_sessionId += b.toString("UTF-8");
			if (_sessionId.equals("?"))
				newSession(ctx);
			else
				checkSession(ctx);
		}
	}

	private void checkSession(ChannelHandlerContext ctx)
	{
		if (_sessionId.length() == _factory.getSessionIdLength() * 2)
		{
			Session session = _factory.getSession(_sessionId);
			if (session == null)
				newSession(ctx);
			else
				confirmSession(ctx);
		}

	}

	private void newSession(ChannelHandlerContext ctx)
	{
		Session session = _factory.createSession(null);
		Constants.ahessianLogger.info(ctx.getChannel()+" new session #"+session.getId());
		MixinPipeline pipeline = null;
		try
		{
			pipeline = (MixinPipeline) _mixinFactory.getPipeline();
			_sessionPipelines.put(session.getId(), pipeline);
		}
		catch (Exception e)
		{
			Constants.ahessianLogger.warn("", e);
		}
		handleSession(ctx, session, pipeline);
	}

	private void confirmSession(ChannelHandlerContext ctx)
	{
		Session session = _factory.getSession(_sessionId);
		Constants.ahessianLogger.info(ctx.getChannel()+" reuse session #"+session.getId());
		MixinPipeline pipeline = _sessionPipelines.get(_sessionId);
		handleSession(ctx, session, pipeline);
	}

	private void handleSession(ChannelHandlerContext ctx, Session session, MixinPipeline pipeline)
	{
		_hasSession = true;
		session.setClosed(false);
		
		// if we have a session timeout set, cancel it.
		Timeout timeOut = session.removeTimeout();
		if (timeOut != null)
			timeOut.cancel();
		
		// check if the session is already connected to a channel
		Channel c = pipeline.getChannel();
		if (c != null && c.isOpen())
		{
			Constants.ahessianLogger.warn(ctx.getChannel()+" session already attached -> close connection");
			c.close();
		}
		
		// now that we have a session extend the pipeline
		ChannelPipeline currentPipeline = ctx.getPipeline();
		pipeline.mixin(currentPipeline);
		ctx.setAttachment(session);
		_channel = ctx.getChannel();
		// first send session and wait until it has been transmitted
		ChannelFuture future = Channels.future(ctx.getChannel());
		Channels.write(ctx, future, ChannelBuffers.wrappedBuffer(session.getId().getBytes()));
		try
		{
			future.await();
		}
		catch (InterruptedException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// only then inform the mixin pipeline
		ctx.sendUpstream(_connectedEvent);
	}

	/**
	 * Helper Method: returns the session of associated with the pipeline of a given context
	 * 
	 * @param ctx the context
	 * 
	 * @return the session
	 */
	public static Session getSession(ChannelHandlerContext ctx)
	{
		ChannelHandlerContext handler = ctx.getPipeline().getContext(ServerSessionFilter.class);
		if (handler == null)
			return null;
		return (Session) handler.getAttachment();
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.SimpleChannelUpstreamHandler#channelConnected(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.ChannelStateEvent)
	 */
	@Override
	public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		// remeber the event. it will be sent upstream when session has been
		// created
		_connectedEvent = e;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.SimpleChannelUpstreamHandler#channelDisconnected(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.ChannelStateEvent)
	 */
	@Override
	public void channelDisconnected(final ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		
		_hasSession = false;
		((Session)ctx.getAttachment()).close();
		final String sessionId = ((Session)ctx.getAttachment()).getId();
		Constants.ahessianLogger.info("Session disconnected: "+ sessionId);
		_sessionId = "";
		_connectedEvent = null;
		_channel = null;
		if (_sessionTimeout > 0)
		{
			Timeout timeOut = _timer.newTimeout(new TimerTask()
			{

				public void run(Timeout arg0) throws Exception
				{
					((Session)ctx.getAttachment()).invalidate();
					_factory.removeSession(sessionId);
					_sessionPipelines.remove(sessionId);
					_valid = false;
					Constants.ahessianLogger.warn(ctx.getChannel()+" session timed out: "+sessionId);
				}
				
			}, _sessionTimeout, TimeUnit.MILLISECONDS);
			((Session)ctx.getAttachment()).setTimeOut(timeOut);
		}
		ctx.sendUpstream(e);		
	}
	
	public long getSessionTimeout()
	{
		return _sessionTimeout;
	}

	public void setSessionTimeout(long sessionTimeout)
	{
		_sessionTimeout = sessionTimeout;
	}
	
	public boolean isValid()
	{
		return _valid;
	}
	
	public Channel getChannel()
	{
		return _channel;
	}
	
	public static ServerSessionFilter getServerSessionFilter(ChannelHandlerContext ctx)
	{
		return (ServerSessionFilter) ctx.getPipeline().getContext(ServerSessionFilter.class).getHandler();
	}
	



}
