package org.rzo.netty.ahessian.rpc.message;

import java.io.InputStream;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.Channels;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.io.InputStreamConsumer;
import org.rzo.netty.ahessian.io.InputStreamDecoder;
import org.rzo.netty.ahessian.rpc.callback.CallbackReplyMessage;
import org.rzo.netty.ahessian.rpc.client.HessianProxyFactory;
import org.rzo.netty.ahessian.rpc.io.Hessian2Input;

import com.caucho.hessian4.io.AbstractSerializerFactory;
import com.caucho.hessian4.io.HessianProtocolException;

/**
 * reads a reply message from an input stream
 */
public class HessianRPCReplyDecoder implements InputStreamConsumer, Constants
{

	/** The _factory. */
	HessianProxyFactory		_factory;
	volatile Hessian2Input	in	= null;
	volatile AbstractSerializerFactory _serializerFactory;

	/**
	 * Instantiates a new hessian rpc reply decoder.
	 * 
	 * @param factory
	 *            the factory
	 */
	public HessianRPCReplyDecoder(HessianProxyFactory factory)
	{
		_factory = factory;
	}

	public HessianRPCReplyDecoder(HessianProxyFactory factory, AbstractSerializerFactory serializerFactory)
	{
		_factory = factory;
		_serializerFactory = serializerFactory;
	}

	public void consume(ChannelHandlerContext ctx, InputStream inx)
	{
		while (ctx.getChannel().isConnected() && !isBufferEmpty())
		{
			Channels.fireMessageReceived(ctx, parseReply(in));
			in.resetReferences();

				if (isBufferEmpty())
				{
					break;
				}
		}
	}

	/**
	 * Parses the reply.
	 * 
	 * @param is
	 *            the is
	 * 
	 * @return the hessian rpc reply message
	 */
	HessianRPCReplyMessage parseReply(Hessian2Input in)
	{
		Object value = null;
		Object fault = null;
		Long callbackId = null;
		String callbackMethod = null;
		Object[] callbackArgs = null;
		Boolean callbackDone = null;
		Boolean completed = null;
		Integer group = null;
		Long callId = null;
		int code;
		try
		{
			if ((code = in.read()) != 'H')
			{
				throw new HessianProtocolException("'" + (char) code + "' is an unknown code");
			}
			in.read();
			in.read();
			in.readEnvelope();
			String h = in.readString();
			if (!HEADER_STRING.equals(h))
			{
				throw new HessianProtocolException("Missing headers");
			}
			int l = in.readInt();
			for (int i = 0; i < l; i++)
			{
				Integer key = in.readInt();
				Object hvalue = in.readObject();
				switch (key)
				{
				
				case ICALLBACK_ID_HEADER_KEY:
					callbackId = (Long)hvalue;
					break;
				case ICALLBACK_METHOD_HEADER_KEY:
					callbackMethod = (String) hvalue;
					break;
				case ICALLBACK_ARGS_HEADER_KEY:
					callbackArgs = (Object[]) hvalue;
					break;
				case ICALLBACK_DONE_HEADER_KEY:
					callbackDone = (Boolean) hvalue;
					break;
				case ICOMPLETED_HEADER_KEY:
					completed = (Boolean) hvalue;
					break;
				case IGROUP_HEADER_KEY:
					group = (Integer) hvalue;
					break;
				case ICALL_ID_HEADER_KEY:
					callId = (Long) hvalue;
					break;
				}
			}
			if ((code = in.read()) != 'H')
			{
				throw new HessianProtocolException("'" + (char) code + "' is an unknown code");
			}
			in.read();
			in.read();

			Object obj = null;
			try
			{
				obj = in.readReply(null);
			}
			catch (Throwable e)
			{
				HessianRPCReplyMessage result = new HessianRPCReplyMessage(null, e, null);
				result .setCallId(callId);
				result.setGroup(group);
				result.setCallbackId(callbackId);
				return result;
			}
			finally
			{
				in.completeReply();
				in.completeEnvelope();
				in.resetReferences();
			}

			if (callbackId != null)
			{
				
				CallbackReplyMessage result =  new CallbackReplyMessage(callbackMethod, callbackArgs, null, null);
				result.setCallbackDone(callbackDone);
				result.setCallbackId(callbackId);
				result.setCallId(callId);
				result.setCompleted(completed);
				result.setGroup(group);
				return result;
			}
			else
			{
				HessianRPCReplyMessage result =  new HessianRPCReplyMessage(obj, null, null);
				result.setCallId(callId);
				result.setCompleted(completed);
				result.setGroup(group);
				return result;
			}

		}
		catch (Throwable ex)
		{
			Constants.ahessianLogger.warn("", ex);
			{
				HessianRPCReplyMessage result = new HessianRPCReplyMessage(null, ex, null);
				result.setCallId(callId);
				result.setGroup(group);
				result.setCallbackId(callbackId);
				return result;

			}
		}

	}

	public boolean isBufferEmpty()
	{
		return in != null && in.bufferEmpty();
	}
	
	public void setContext(ChannelHandlerContext ctx)
	{
		if (in == null)
		{
			in = new Hessian2Input(InputStreamDecoder.getInputStream(ctx));
			if (_serializerFactory != null)
				in.getSerializerFactory().addFactory(_serializerFactory);
		}
	}

}
