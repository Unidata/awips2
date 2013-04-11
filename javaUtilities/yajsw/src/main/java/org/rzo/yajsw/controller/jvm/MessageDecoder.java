package org.rzo.yajsw.controller.jvm;

import java.nio.charset.Charset;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.handler.codec.oneone.OneToOneDecoder;
import org.rzo.yajsw.controller.Message;

@ChannelPipelineCoverage("one")
public class MessageDecoder extends OneToOneDecoder
{
	@Override
	protected Object decode(ChannelHandlerContext ctx, Channel channel, Object message) throws Exception
	{
		ChannelBuffer b = (ChannelBuffer) message;

		byte code = b.readByte();
		// TODO remove the nul
		b.writerIndex(b.writerIndex());
		String msg = b.toString(Charset.defaultCharset().displayName());
		Message result = new Message(code, msg);
		return result;
	}

}
