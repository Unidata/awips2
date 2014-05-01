package org.rzo.yajsw.controller.jvm;

import static org.jboss.netty.buffer.ChannelBuffers.buffer;

import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.handler.codec.oneone.OneToOneEncoder;
import org.rzo.yajsw.controller.Message;

@ChannelPipelineCoverage("one")
public class MessageEncoder extends OneToOneEncoder
{

	/** The Constant encoder. */
	static final CharsetEncoder	encoder	= Charset.defaultCharset().newEncoder();

	@Override
	protected Object encode(ChannelHandlerContext ctx, Channel channel, Object message) throws Exception
	{
		Message m = (Message) message;
		ChannelBuffer buffer = buffer(m.getMessage().length() + 2);
		buffer.writeByte(m.getCode());
		buffer.writeBytes(encoder.encode(CharBuffer.wrap(m.getMessage())));
		buffer.writeByte((byte) 0);
		// System.out.println("encode " + buffer.toString("UTF-8"));
		return buffer;
	}

}
