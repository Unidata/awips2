package org.rzo.yajsw.nettyutils;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.jboss.netty.channel.group.ChannelGroup;
import org.jboss.netty.channel.group.DefaultChannelGroup;

@ChannelPipelineCoverage("all")
public class ChannelGroupFilter extends SimpleChannelHandler
{
	ChannelGroup	_channels	= new DefaultChannelGroup();
	Condition		_condition;

	public ChannelGroupFilter(Condition condition)
	{
		_condition = condition;
	}

	@Override
	public void channelOpen(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		_channels.add(ctx.getChannel());
	}

	@Override
	public void writeRequested(ChannelHandlerContext ctx, MessageEvent e) throws Exception
	{
		if (_condition.isOk(ctx, e))
		{
			_channels.remove(ctx.getChannel());
			_channels.close();
		}
		ctx.sendDownstream(e);
	}

}
