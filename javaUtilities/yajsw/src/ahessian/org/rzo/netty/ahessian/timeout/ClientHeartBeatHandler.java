package org.rzo.netty.ahessian.timeout;


import java.util.Date;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.DownstreamMessageEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.util.Timer;
import org.rzo.netty.ahessian.Constants;

public class ClientHeartBeatHandler extends AbstractHeartBeatHandler
{

	public ClientHeartBeatHandler(String name, Timer timer, long timeout)
	{
		super(name, timer, timeout);
	}

	@Override
	void timedOut(ChannelHandlerContext ctx)
	{
    	Constants.ahessianLogger.info("no writes since "+new Date(getLastCalled())+" -> send empty buffer heartbeat");
		ChannelFuture future = Channels.future(_ctx.getChannel());
		ChannelBuffer b = ChannelBuffers.buffer(1);
		b.writeByte(0);
        _ctx.sendDownstream(new DownstreamMessageEvent(_ctx.getChannel(), future, b, _ctx.getChannel().getRemoteAddress()));
        try
		{
			future.await();
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();
		}
	}
	
    public void writeRequested(ChannelHandlerContext ctx, MessageEvent e) throws Exception {
    	ping();
        ctx.sendDownstream(e);
    }
    


}
