package org.rzo.yajsw.nettyutils;

import org.jboss.netty.channel.ChannelEvent;
import org.jboss.netty.channel.ChannelHandlerContext;

public interface Condition
{
	public boolean isOk(ChannelHandlerContext ctx, ChannelEvent e);
}
