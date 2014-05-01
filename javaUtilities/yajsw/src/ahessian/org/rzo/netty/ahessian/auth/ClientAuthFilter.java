package org.rzo.netty.ahessian.auth;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;

/**
 * Client side authentication handler.
 * <br>
 * This must be the first handler in the pipeline.
 * 
 *  <br>
 * A typical setup for ClientAuthFilter for TCP/IP socket would be:
 * 
 * <pre>
 * {@link ChannelPipeline} pipeline = ...;
 * 
 *   EncryptedAuthToken token = new EncryptedAuthToken();
 *   token.setAlgorithm("SHA-1");
 *   token.setPassword("test");
 *   ClientAuthFilter auth = new ClientAuthFilter(token);
 *   pipeline.addLast("auth", auth);
 * </pre>
 * 
 */
@ChannelPipelineCoverage("one")
public class ClientAuthFilter extends SimpleChannelUpstreamHandler
{
	
	/** The authentication token. */
	AuthToken _token;
	
	/**
	 * Instantiates a new client authentication handler.
	 * 
	 * @param token the token
	 */
	public ClientAuthFilter(AuthToken token)
	{
		_token = token;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.SimpleChannelUpstreamHandler#channelConnected(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.ChannelStateEvent)
	 */
	@Override
    public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception {
		_token.sendPassword(ctx);
		ctx.sendUpstream(e);
    }

}
