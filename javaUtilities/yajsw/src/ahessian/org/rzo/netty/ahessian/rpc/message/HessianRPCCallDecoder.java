package org.rzo.netty.ahessian.rpc.message;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.Channels;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.io.InputStreamConsumer;
import org.rzo.netty.ahessian.io.InputStreamDecoder;
import org.rzo.netty.ahessian.io.OutputStreamEncoder;
import org.rzo.netty.ahessian.rpc.io.Hessian2Input;
import org.rzo.netty.ahessian.rpc.server.HessianRPCServiceHandler;
import org.rzo.netty.ahessian.session.ServerSessionFilter;

import com.caucho.hessian4.io.AbstractSerializerFactory;
import com.caucho.hessian4.io.SerializerFactory;

/**
 *  Decodes a call request from an input stream
 */
public class HessianRPCCallDecoder implements InputStreamConsumer, Constants
{
	
	/** The _factory. */
	HessianRPCServiceHandler _factory;
	SerializerFactory sFactory = new SerializerFactory();
	volatile Hessian2Input in = null;

	
	public HessianRPCCallDecoder()
	{
		super();
	}
	
	public HessianRPCCallDecoder(AbstractSerializerFactory serializerFactory)
	{
		super();
		if (serializerFactory != null)
			sFactory.addFactory(serializerFactory);
	}

	
    public void consume(ChannelHandlerContext ctx, InputStream inx) 
    {
		HessianRPCCallMessage result = null;
		boolean getNextMessage = true;
		if (in == null || in.isClosed() || in.getInputStream() != inx)
		{
		in = new Hessian2Input(inx);
		in.setSerializerFactory(sFactory);
		}
		while (ctx.getChannel().isConnected() && getNextMessage)
		{
		try
		{
			
		if (in.bufferEmpty())
		{
			// we have nothing to parse
			break;
		}
	
		int ch;
		if ((ch=in.read()) != 'H')
		{
			ahessianLogger.warn("H expected got " + "0x" + Integer.toHexString(ch & 0xff) + " (" + (char) + ch + ")");
			continue;
		}
		in.read();
		in.read();
		in.readEnvelope();
		String h = in.readString();
		if (!HEADER_STRING.equals(h))
		{
			ahessianLogger.warn("missing header");
			continue;
		}
		
		Map<Object, Object> headers = new HashMap<Object, Object>();
		String methodName = null;
		List values = new ArrayList();
				
	    int l = in.readInt();
	    for (int i=0; i<l; i++) 
	    {
	      Integer key = in.readInt();
	      Object value = in.readObject();
	      headers.put(key, value);
	    }
	    in.readCall();
	    methodName = in.readMethod();
	    int argsLength = in.readInt();

	   for (int i=0; i<argsLength; i++)
	    	values.add(in.readObject());
	   
	   in.completeCall();
	   in.completeEnvelope();

	   in.resetReferences();
	    result = new HessianRPCCallMessage(methodName, values.toArray(), headers, (OutputStreamEncoder) OutputStreamEncoder.getOutputEncoder(ctx));
	    result.setServer(true);
	    result.setHasSessionFilter((Boolean) headers.get(HAS_SESSION_FILTER_HEADER_KEY));
	    result.setSession(ServerSessionFilter.getSession(ctx));
		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.warn("", ex);
			result = null;
		}
		if (in.bufferEmpty())
		{
			getNextMessage = false;
		}
			if (result != null)
		    Channels.fireMessageReceived(ctx, result);
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
			in.setSerializerFactory(sFactory);
		}
	}


	
}
