package org.rzo.yajsw.nettyutils;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;

@ChannelPipelineCoverage("all")
public class ConditionFilter extends SimpleChannelUpstreamHandler
{

	Condition	_condition;

	public ConditionFilter(Condition condition)
	{
		_condition = condition;
	}

	@Override
	public void channelOpen(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		if (_condition.isOk(ctx, e))
		{
			// forward if condtion met
			ctx.sendUpstream(e);
		}
		else
		{
			ctx.getChannel().close();
		}
	}

}
