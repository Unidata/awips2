package org.rzo.netty.ahessian.rpc.server;

import java.io.InputStream;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.callback.ClientCallback;
import org.rzo.netty.ahessian.rpc.callback.ServerCallbackProxy;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyMessage;
import org.rzo.netty.ahessian.rpc.stream.ServerInputStreamManager;

/**
 * Wraps an object as a {@link Service}. Methods are invoked as soon as they are received.
 * Invocation and return of result are executed within the netty worker thread. <br>
 * This type of service is used for short running invocations.
 * <br>
 * Typical usage:
 * <pre>
 * 
 * // the object to be wrapped, implements MyServiceInterface
 * Object myServiceObject; 
 * 
 * // the netty rpc service handler
 * HessianRPCServiceHandler handler;
 * 
 * Service myService = new ImmediateInvokeService(myServiceObject, MyServiceInterface.class);
 * 
 * // Clients will access the service through the given name
 * handler.addService("myServiceName", myService);
 * 
 * </pre>
 */
public class ImmediateInvokeService extends HessianSkeleton implements Constants
{

	/**
	 * Instantiates a new immediate invoke service.
	 * 
	 * @param service the service object implementing apiClass
	 * @param apiClass the api of the service exposed to the client
	 * @param factory the netty handler
	 */
	public ImmediateInvokeService(Object service, Class apiClass, HessianRPCServiceHandler factory)
	{
		super(service, apiClass, factory);
	}

	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.rpc.server.HessianSkeleton#messageReceived(org.rzo.netty.ahessian.rpc.HessianRPCCallMessage)
	 */
	@Override
	public void messageReceived(HessianRPCCallMessage message)
	{
		ServiceSessionProvider.set(message.getSession());
		invoke(message);
		ServiceSessionProvider.remove();
	}
	
	/**
	 * Invokes the RPC call and sends back the result
	 * 
	 * @param message the message
	 */
	 void invoke(HessianRPCCallMessage message)
	{
		Object result = null;
		Object fault = null;
		try
		{
			Method method = getMethod(message);
			Object[] args = message.getArgs();
			if (args != null)
			{
				for (int i=0; i<args.length; i++)
				{
					if (args[i] instanceof ClientCallback)
					{
						ClientCallback cc = (ClientCallback) args[i];
						ClassLoader cl = cc.getClass().getClassLoader();
						Class clazz = cl.loadClass(cc.getCallbackClass());
						List<Class> clazzes = new ArrayList();
						while (clazz != null && (!clazz.equals(Object.class)))
						{
								clazzes.addAll(Arrays.asList(clazz.getInterfaces()));
								clazz = clazz.getSuperclass();
						}
						args[i] = Proxy.newProxyInstance(cl, (Class[])clazzes.toArray(new Class[clazzes.size()]), new ServerCallbackProxy(_factory, message, (ClientCallback) args[i]));
					}
				}
			}
			result = method.invoke(_service, args);
		}
		catch (Throwable ex)
		{
			Constants.ahessianLogger.warn("", ex);
			fault = ex;
		}
		if (fault == null && result instanceof InputStream)
		{
			handleInputStreamResult(fault, result, message);
		}
		else
		{
			handleDefaultResult(fault, result, message);
		}
	}
	

}
