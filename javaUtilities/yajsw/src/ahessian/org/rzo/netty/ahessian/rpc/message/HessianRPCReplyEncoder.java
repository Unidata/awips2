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

import com.caucho.hessian4.io.AbstractSerializerFactory;

/**
 * writes an invocation reply message to an output stream
 */
@ChannelPipelineCoverage("one")
public class HessianRPCReplyEncoder extends SimpleChannelHandler
	{
	volatile Hessian2Output hOut = null;
	volatile AbstractSerializerFactory _serializerFactory;
	
	public HessianRPCReplyEncoder()
	{
		this(null);
	}

	public HessianRPCReplyEncoder(AbstractSerializerFactory serializerFactory)
	{
		_serializerFactory = serializerFactory;
	}

		/* (non-Javadoc)
		 * @see org.jboss.netty.channel.SimpleChannelDownstreamHandler#writeRequested(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.MessageEvent)
		 */
		synchronized public void writeRequested(ChannelHandlerContext ctx, MessageEvent e) throws Exception
		{
 			Object msg = e.getMessage();
 			if (msg instanceof FlushRequestMessage)
 			{
 				hOut.flush(e.getFuture());
 				e.getFuture().await(5000);
 				return;
 			}

			try
			{
//				if (e.getMessage() instanceof Integer)
//				{
//					hOut.flush();
//					return;
//				}
			HessianRPCReplyMessage message = (HessianRPCReplyMessage) e.getMessage();
			//Constants.ahessianLogger.warn("encode reply for #"+message.getHeaders().get(Constants.CALL_ID_STRING));

			hOut.resetReferences();
			hOut.writeReply(message);
			//hOut.flush();
			//e.getFuture().setSuccess();
			}
			catch (Exception ex)
			{
				Constants.ahessianLogger.warn("", ex);
				e.getFuture().setFailure(ex);
			}
		}
		
		public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
		{
			if (hOut == null)
			{
				OutputStream out = OutputStreamEncoder.getOutputStream(ctx);
				hOut = new Hessian2Output(out);
				if (_serializerFactory != null)
					hOut.getSerializerFactory().addFactory(_serializerFactory);
			}
			else
				hOut.reset();
				ctx.sendUpstream(e);
		}


}
