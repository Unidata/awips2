package org.rzo.netty.ahessian.stopable;

import org.jboss.netty.channel.ChannelHandler;
import org.jboss.netty.channel.DefaultChannelPipeline;

public class StopablePipeline extends DefaultChannelPipeline
{
	public void stop()
	{
		ChannelHandler handler = this.getFirst();
		while (handler != null)
		{
			if (handler instanceof StopableHandler)
			{
				StopableHandler stopableHandler = (StopableHandler) handler;
				if (stopableHandler.isStopEnabled())
					try
				{
					stopableHandler.stop();
				}
				catch (Exception ex)
				{
					
				}
			}
			this.removeFirst();
			handler = this.getFirst();
		}
	}
	
	public static StopablePipeline pipeline()
	{
		return new StopablePipeline();
	}

}
