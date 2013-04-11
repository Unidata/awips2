package org.rzo.netty.ahessian.serialization;

import java.io.OutputStream;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelDownstreamHandler;
import org.rzo.netty.ahessian.io.OutputStreamEncoder;

import com.caucho.hessian4.io.HessianOutput;

/**
 * Encodes the requested {@link java.lang.Object} into a {@link ChannelBuffer}.
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
public class HessianEncoder extends SimpleChannelDownstreamHandler
{

    /* (non-Javadoc)
     * @see org.jboss.netty.channel.SimpleChannelDownstreamHandler#writeRequested(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.MessageEvent)
     */
    public void writeRequested(ChannelHandlerContext ctx, MessageEvent e) throws Exception {
		OutputStream out = OutputStreamEncoder.getOutputStream(ctx);
		HessianOutput hout = new HessianOutput(out);
		hout.writeObject(e.getMessage());
		hout.flush();
    }


}
