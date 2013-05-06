package org.rzo.netty.ahessian.crypto;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.channel.DownstreamMessageEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.UpstreamMessageEvent;

public class Util implements CryptoConstants
{
	static MessageEvent code(StreamCipher cipher, MessageEvent e, boolean decode) throws Exception
	{
		try
		{
		ChannelBuffer b = (ChannelBuffer) e.getMessage();
		byte[] encodedData = cipher.crypt(b.array(), b.readerIndex(), b.readableBytes());
		return toMessageEvent(e, ChannelBuffers.wrappedBuffer(encodedData));
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			throw ex;
		}
		
	}
	
	static MessageEvent toMessageEvent(MessageEvent e, ChannelBuffer data)
	{
		if (e instanceof DownstreamMessageEvent)
		{
			return new DownstreamMessageEvent(e.getChannel(), e.getFuture(), data, e.getRemoteAddress());
		}
		else if (e instanceof UpstreamMessageEvent)
		{
			return new UpstreamMessageEvent(e.getChannel(), data, e.getRemoteAddress());
		}
		else
		{
			System.out.println("unxepected message type in Util.toMessageEvent: " + e.getMessage());
			return e;
		}
		}


}
