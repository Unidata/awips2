package org.rzo.netty.ahessian.stopable;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;

public class StopHandler extends SimpleChannelUpstreamHandler
{

	@Override
	public void channelClosed(final ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		ChannelPipeline p = ctx.getPipeline();
		if (p instanceof StopablePipeline)
			((StopablePipeline)p).stop();
	}


}
