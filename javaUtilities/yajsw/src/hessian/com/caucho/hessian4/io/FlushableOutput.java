package com.caucho.hessian4.io;

import java.io.IOException;

import org.jboss.netty.channel.ChannelFuture;

public interface FlushableOutput
{
	public void flush(ChannelFuture future)  throws IOException;
	public void reset();

}
