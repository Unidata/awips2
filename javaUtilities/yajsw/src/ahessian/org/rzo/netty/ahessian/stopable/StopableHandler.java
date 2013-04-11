package org.rzo.netty.ahessian.stopable;

import org.jboss.netty.channel.ChannelHandler;

public interface StopableHandler extends ChannelHandler
{
	public void stop();
	public boolean isStopEnabled();
	public void setStopEnabled(boolean stopEnabled);
}
