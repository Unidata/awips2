package org.rzo.netty.ahessian.rpc.message;

import java.io.OutputStream;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.io.OutputStreamEncoder;
import org.rzo.netty.ahessian.rpc.io.Hessian2Output;
import org.rzo.netty.ahessian.session.ClientSessionFilter;

import com.caucho.hessian4.io.AbstractSerializerFactory;
import com.caucho.hessian4.io.SerializerFactory;

/**
 * writes a call request to an output stream
 */
@ChannelPipelineCoverage("all")
public class HessianRPCCallEncoder extends SimpleChannelHandler
{
	SerializerFactory sFactory = new SerializerFactory();
	Hessian2Output hOut = null;
	boolean _hasSessionFilter = false;
	
	public HessianRPCCallEncoder()
	{
		super();
	}
	
	public HessianRPCCallEncoder(AbstractSerializerFactory serializerFactory)
	{
		super();
		if (serializerFactory != null)
			sFactory.addFactory(serializerFactory);
	}

	 /* (non-Javadoc)
 	 * @see org.jboss.netty.channel.SimpleChannelDownstreamHandler#writeRequested(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.MessageEvent)
 	 */
 	public void writeRequested(ChannelHandlerContext ctx, MessageEvent e) throws Exception
	{
 		try
 		{
// 			if (e.getMessage() instanceof Integer)
// 			{
// 				hOut.flush();
// 				return;
// 			}
 			Object msg = e.getMessage();
 			if (msg instanceof FlushRequestMessage)
 			{
 				hOut.flush(e.getFuture());
 				e.getFuture().await(5000);
 				return;
 			}
 		HessianRPCCallMessage message = (HessianRPCCallMessage) e.getMessage();
		message.setHasSessionFilter(_hasSessionFilter);
		hOut.resetReferences();
		hOut.call(message);
 		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.warn("", ex);
			e.getFuture().setFailure(ex);
		}
		//--Thread.yield();
	}

	private OutputStream getOutputStream(ChannelHandlerContext ctx)
	{
		return (OutputStream) ctx.getPipeline().getContext(OutputStreamEncoder.class).getAttachment();
	}
	
	@Override
	 public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e)
	{
		_hasSessionFilter = ctx.getPipeline().getContext(ClientSessionFilter.class) != null;
		if (hOut == null)
		{
		OutputStream out = (OutputStream) ctx.getPipeline().getContext(OutputStreamEncoder.class).getAttachment();
		hOut = new Hessian2Output(out);
	    hOut.getSerializerFactory().addFactory(sFactory);
		}
		else
			hOut.reset();
		ctx.sendUpstream(e);
	}
	

}