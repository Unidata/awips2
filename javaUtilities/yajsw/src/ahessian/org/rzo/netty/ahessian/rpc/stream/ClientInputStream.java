package org.rzo.netty.ahessian.rpc.stream;

import java.io.IOException;
import java.io.InputStream;
import java.nio.Buffer;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.rzo.netty.ahessian.io.InputStreamBuffer;

public class ClientInputStream extends InputStreamBuffer
{
	private long _id;

	public ClientInputStream(long id)
	{
		_id = id;
	}
	
	public void addMessage(InputStreamReplyMessage msg)
	{
		if (msg.isClosed())
			try
			{
				closeOnEmpty();
			}
			catch (IOException e1)
			{
				e1.printStackTrace();
			}
		else
			try
			{
				this.write(ChannelBuffers.wrappedBuffer(msg.getData()));
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
	}

	public long getId()
	{
		return _id;
	}


}
