package org.rzo.netty.ahessian.auth;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.jboss.netty.logging.InternalLogger;
import org.jboss.netty.logging.InternalLoggerFactory;

/**
 * Server side authentication handler.
 * <br>
 * This must be the first handler in the pipeline.
 * 
 *  <br>
 * A typical setup for ServerAuthFilter for TCP/IP socket would be:
 * 
 * <pre>
 * {@link ChannelPipeline} pipeline = ...;
 * 
 *   EncryptedAuthToken token = new EncryptedAuthToken();
 *   token.setAlgorithm("SHA-1");
 *   token.setPassword("test");
 *   ServerAuthFilter auth = new ServerAuthFilter(token);
 *   pipeline.addLast("auth", auth);
 * </pre>
 * 
 */
@ChannelPipelineCoverage("one")
public class ServerAuthFilter extends SimpleChannelUpstreamHandler
{
	private AuthToken	_token			= null;
	private boolean		_authenticated	= false;
	private static final InternalLogger logger =
        InternalLoggerFactory.getInstance(ServerAuthFilter.class);

	/**
	 * Instantiates a new server auth filter.
	 * 
	 * @param token the token
	 */
	public ServerAuthFilter(AuthToken token)
	{
		setToken(token);
	}

	/**
	 * Sets the token.
	 * 
	 * @param token the new token
	 */
	public void setToken(AuthToken token)
	{
		_token = token;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.SimpleChannelUpstreamHandler#messageReceived(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.MessageEvent)
	 */
	@Override
	public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception
	{
		if (!_authenticated)
		{
			int result = _token.authenticate(ctx, e);
			if ( result == AuthToken.FAILED)
			{
				logger.warn("authentication failed -> close connection");
				ctx.getChannel().close();
			}
			else if (result == AuthToken.PASSED)
			{
				_authenticated = true;
			}
		}
		else
			ctx.sendUpstream(e);
	}
	
	@Override
    public void channelDisconnected(
            ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		_token.disconnected();
		ctx.sendUpstream(e);
	}


}
