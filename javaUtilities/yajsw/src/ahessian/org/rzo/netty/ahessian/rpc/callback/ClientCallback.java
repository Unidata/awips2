package org.rzo.netty.ahessian.rpc.callback;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicLong;

import org.rzo.netty.ahessian.Constants;


public class ClientCallback implements Serializable
{

	private transient Callback _callback;
	private Long _id;
	private String _callbackClass;
	private static final AtomicLong _idCounter = new AtomicLong();
	private transient boolean _done = false;
	
	public ClientCallback()
	{
		
	}

	
	public ClientCallback(Callback callback)
	{
		_callback = callback;
		_id = _idCounter.getAndIncrement();
		_callbackClass = _callback.getClass().getName();
	}

	public Long getId()
	{
		return _id;
	}

	public void invoke(CallbackReplyMessage message)
	{
		try
		{
		String methodName = message.getMethod();
		Object[] args = message.getArgs();
		Method[] methods = _callback.getClass().getMethods();
		for (Method method : methods)
		{
			if (methodName.equals(method.getName()) && method.getParameterTypes().length == args.length)
			{
				method.invoke(_callback, args);
				break;
			}
		}
		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.warn("", ex);
		}
	}

	public String getCallbackClass()
	{
		return _callbackClass;
	}
	
}
