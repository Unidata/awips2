package org.rzo.netty.ahessian.serialization;

import java.io.InputStream;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.rzo.netty.ahessian.Constants;

import com.caucho.hessian4.io.HessianInput;

/**
 * Decodes a {@link ChannelBuffer} into a {@link java.lang.Object}.
 * A typical setup for a serialization protocol in a TCP/IP socket would be:
 * <pre>
 * {@link ChannelPipeline} pipeline = ...;
 * 
 * // Encoder
 * pipeline.addLast("outputStream", new {@link io.OutputStream}());
 * pipeline.addLast("hessianEncoder", new {@link HessianEncoder}());
 * 
 * // Decoder
 * pipeline.addLast("inputStream", new {@link io.InputStream}());
 * pipeline.addLast("hessianDecoder", new {@link HessianDecoder}());
 * pipeline.addLast("handler", new MyHandler());
 * </pre>
 * and then, within the handler you can use a {@link java.lang.Object} instead of a {@link ChannelBuffer}
 * as a message:
 * <pre>
 * void messageReceived(ChannelHandlerContext ctx, MessageEvent e) {
 * // get the message
 * Object msg = e.getMessage();
 * // return the current time
 * ch.write(new Date());
 * }
 * </pre>
 */

public class HessianDecoder extends SimpleChannelUpstreamHandler
{

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.SimpleChannelUpstreamHandler#messageReceived(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.MessageEvent)
	 */
	@Override
    public void messageReceived(
            ChannelHandlerContext ctx, MessageEvent e) throws Exception 
    {
		InputStream in = (InputStream) e.getMessage();
		try
		{
			HessianInput hin = new HessianInput(in);
			while (true)
			{
				Object obj = hin.readObject(null);
				Channels.fireMessageReceived(ctx, obj);
			}
		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.debug("", ex);
		}
    }
    
	

}
