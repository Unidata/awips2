package org.rzo.netty.ahessian.timeout;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.jboss.netty.util.Timeout;
import org.jboss.netty.util.Timer;
import org.jboss.netty.util.TimerTask;
import org.rzo.netty.ahessian.Constants;

abstract class AbstractHeartBeatHandler extends SimpleChannelHandler
{
	volatile long _lastCalled;
	volatile ChannelHandlerContext _ctx;
	final IntervalTimer _intervalTimer;
	final String _name;

	public AbstractHeartBeatHandler(final String name, final Timer timer, final long timeout)
	{
		_name = name;
		final TimerTask task = new TimerTask()
		{
			public void run(Timeout nTimeout) throws Exception
			{
				if (((getLastCalled() + timeout) <= System.currentTimeMillis()) && isConnected())
					try
					{
						timedOut(_ctx);
					}
					catch (Exception e)
					{
						Constants.ahessianLogger.warn("", e);
					}
			}
			
		};
		_intervalTimer = new IntervalTimer(timer, task, timeout);
	}
	
	abstract void timedOut(ChannelHandlerContext ctx);
	
    long getLastCalled()
    {
    	return _lastCalled;
    }
    
    boolean isConnected()
    {
    	return _ctx != null && _ctx.getChannel().isConnected();
    }
    
    public void channelDisconnected(
            ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception 
            {
    _ctx = null;
    _intervalTimer.stop();
    ctx.sendUpstream(e);
    }
    
    protected void ping()
    {
		_lastCalled = System.currentTimeMillis();    	
    }

    
    public void channelConnected(
            ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception {
		ping();
		_ctx = ctx;
		_intervalTimer.setName(_name+":"+_ctx.getChannel().getId());
		
		Constants.ahessianLogger.info("AbstractHeartBeatHandler scheduler started: "+ _intervalTimer.getInterval());
		_intervalTimer.start();
		ctx.sendUpstream(e);
    }



	
	

}
