package org.rzo.netty.ahessian.rpc.server;

import java.io.InputStream;
import java.lang.reflect.Method;

import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyMessage;
import org.rzo.netty.ahessian.rpc.stream.InputStreamReplyMessage;
import org.rzo.netty.ahessian.rpc.stream.ServerInputStream;
import org.rzo.netty.ahessian.rpc.stream.ServerInputStreamManager;

/**
 * The Class HessianSkeleton, extends the original HessianSkeleton, 
 * so that it can be used in a non-servlet environment
 * 
 * TODO method name overloading. currently only overloading by number of arguments is supported
 */
public abstract class HessianSkeleton extends com.caucho.hessian4.server.HessianSkeleton implements Service, Constants
{
	
	/** The _service. */
	Object _service;
	
	/** The _factory. */
	HessianRPCServiceHandler _factory;
	
	ServerInputStreamManager _serverInputStreamManager;
	
	/**
	 * Instantiates a new hessian skeleton.
	 * 
	 * @param service the service
	 * @param apiClass the api class
	 * @param factory the factory
	 */
	public HessianSkeleton(Object service, Class apiClass, HessianRPCServiceHandler factory)
	{
	    super(apiClass);
		_service = service;
		_factory = factory;
	}
	
	
	/**
	 * Gets the method.
	 * 
	 * @param message the message
	 * 
	 * @return the method
	 */
	public Method getMethod(HessianRPCCallMessage message)
	{
		Object[] args = message.getArgs();
		String methodName = message.getMethod();
	    Method method = null;
	    if (args == null || args.length == 0)
	    	method = getMethod(mangleName(methodName, args));
	    if (method == null)
	      method = getMethod(message.getMethod());	    
	    return method;
	}
	
	  public static String mangleName(String method, Object[] args)
	  {
	    if (args != null && args.length > 0)
	    {
		StringBuffer sb = new StringBuffer();
		sb.append(method);
	    sb.append('_');
	    sb.append(args.length);
	    return sb.toString();
	    }
	    return method;
	  }

	/**
	 * Write result.
	 * 
	 * @param message the message
	 */
	public void writeResult(HessianRPCReplyMessage message)
	{
		_factory.writeResult(message);
	}
	
	public void handleDefaultResult(Object fault, Object result, HessianRPCCallMessage message)
	{
		HessianRPCReplyMessage reply = new HessianRPCReplyMessage(result, fault, message);
		reply.setCompleted(true);
		reply.setCallId((Long) message.getHeaders().get(CALL_ID_HEADER_KEY));
		reply.setGroup((Integer) message.getHeaders().get(GROUP_HEADER_KEY));
		writeResult(reply);
	}
	
	public void handleInputStreamResult(Object fault, Object result, HessianRPCCallMessage message)
	{
		ServerInputStream serverInputStream = _serverInputStreamManager.createServerInputStream((InputStream) result, message.getChannel());
		InputStreamReplyMessage internalReply = new InputStreamReplyMessage();
		internalReply.setId(serverInputStream.getId());
		internalReply.setCreated(true);
		HessianRPCReplyMessage reply = new HessianRPCReplyMessage(result, fault, message);
		reply.setCompleted(true);
		reply.setCallId((Long) message.getHeaders().get(CALL_ID_HEADER_KEY));
		reply.setGroup((Integer) message.getHeaders().get(GROUP_HEADER_KEY));
		writeResult(reply);
		serverInputStream.start();
	}
	
	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.rpc.server.Service#messageReceived(org.rzo.netty.ahessian.rpc.HessianRPCCallMessage)
	 */
	abstract public void messageReceived(HessianRPCCallMessage message);
	
	public void setServerInputStreamManager(ServerInputStreamManager serverInputStreamManager)
	{
		_serverInputStreamManager = serverInputStreamManager;
	}

}
