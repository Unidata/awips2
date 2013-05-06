package org.rzo.netty.ahessian.io;

import java.io.IOException;
import java.io.OutputStream;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.rzo.netty.ahessian.stopable.StopableHandler;

/**
 * Encodes bytes written to an {@link OutputStream} into a {@link ChannelBuffer}
 * . A typical setup for a serialization protocol in a TCP/IP socket would be:
 * 
 * <pre>
 * {@link ChannelPipeline} pipeline = ...;
 * 
 * // Encoder
 * pipeline.addLast(&quot;outputStream&quot;, new {@link handler.io.OutputStream}());
 * pipeline.addLast(&quot;outputHandler&quot;, new MyOutputHandler());
 * 
 * // Decoder
 * pipeline.addLast(&quot;inputStream&quot;, new {@link handler.io.InputStream}());
 * pipeline.addLast(&quot;inputHandler&quot;, new MyInputHandler());
 * </pre>
 * 
 * and then, within the handler you can use a {@link java.io.InputStream} or
 * {@link java.io.OutputStream} instead of a {@link ChannelBuffer} as a message: <br>
 * Writing to OutputStream:
 * 
 * <pre>
 * // synchronized for multithreaded environment to avoid messages mixing
 * synchronized public void writeRequested(ChannelHandlerContext ctx, MessageEvent e) throws Exception
 * {
 * byte[] message = (byte[]) e.getMessage();
 * OutputStream out = OutputStreamEncoder.getOutputStream(ctx);
 * out.write(message);
 * // if this is the last chunk of bytes we should flush the output
 * out.flush();
 * // netty seems to require this, so that the boss thread may read input from the channel
 * Thread.yield();
 * }
 * 
 * </pre>
 * 
 * <br>
 * Reading from InputStream:
 * 
 * <pre>
 * void messageReceived(ChannelHandlerContext ctx, MessageEvent e)
 * {
 * // message received is called only once to deliver the input stream
 * // it is called in a separate thread and not in the netty worker thread.
 * // incoming bytes are consumed in this method.
 * // the stream is closed once the channel is disconnected
 * InputStream in = (InputStream) evt.getMessage();
 * 
 * while (ctx.getChannel().isConnected())
 * {
 * // parse the incoming stream and forward the result to the next handler
 * Channels.fireMessageReceived(ctx, parseReply(in));
 * }
 * }
 * </pre>
 */
public class OutputStreamEncoder extends SimpleChannelHandler implements StopableHandler
{
	volatile OutputStreamBuffer _buffer = null;
	private boolean	_stopEnabled = true;
	boolean _crcCheck = false;
	
	public OutputStreamEncoder()
	{
		
	}
	
	public OutputStreamEncoder(boolean crcCheck)
	{
		_crcCheck = crcCheck;
	}
	
	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.SimpleChannelHandler#channelConnected(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.ChannelStateEvent)
	 */
	@Override
	public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		if (_buffer == null)
		{
			if (_crcCheck)
				_buffer = new CRCOutputStream(ctx);
			else
				_buffer = new OutputStreamBuffer(ctx);
			ctx.setAttachment(_buffer);
		}
		else
			_buffer.setContext(ctx);
		ctx.sendUpstream(e);
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.SimpleChannelHandler#channelDisconnected(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.ChannelStateEvent)
	 */
	@Override
	public void channelDisconnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		if (_buffer != null)
		{
			_buffer.close();
		}
		ctx.sendUpstream(e);
	}

	/**
	 * Helper method: Gets the output stream from the pipeline of a given context.
	 * 
	 * @param ctx the context
	 * 
	 * @return the output stream
	 */
	public static OutputStream getOutputStream(ChannelHandlerContext ctx)
	{
		return (OutputStream) ctx.getPipeline().getContext(OutputStreamEncoder.class).getAttachment();
	}
	
	public static OutputStreamEncoder getOutputEncoder(ChannelHandlerContext ctx)
	{
		return (OutputStreamEncoder) ctx.getPipeline().getContext(OutputStreamEncoder.class).getHandler();
	}
	
	public OutputStreamBuffer getBuffer()
	{
		return _buffer;
	}
	
	public boolean isStopEnabled()
	{
		return _stopEnabled ;
	}

	public void setStopEnabled(boolean stopEnabled)
	{
		_stopEnabled = stopEnabled;
	}

	public void stop()
	{
		try
		{
			_buffer.close();
		}
		catch (IOException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		_buffer = null;
	}


}
