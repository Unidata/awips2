package org.rzo.netty.ahessian.timeout;


import java.util.Date;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.handler.timeout.ReadTimeoutException;
import org.jboss.netty.util.Timer;
import org.rzo.netty.ahessian.Constants;

public class ServerHeartBeatHandler extends AbstractHeartBeatHandler
{
    static final ReadTimeoutException EXCEPTION = new ReadTimeoutException();


	public ServerHeartBeatHandler(String name, Timer timer, long timeout)
	{
		super(name, timer, timeout);
	}

	@Override
	void timedOut(ChannelHandlerContext ctx)
	{		
    	Constants.ahessianLogger.info("no reads since "+new Date(getLastCalled())+" -> close channel");
	    ctx.getChannel().close();
	 }
	
    public void messageReceived(
            ChannelHandlerContext ctx, MessageEvent e) throws Exception {
    	ping();
        ctx.sendUpstream(e);
    }



}
