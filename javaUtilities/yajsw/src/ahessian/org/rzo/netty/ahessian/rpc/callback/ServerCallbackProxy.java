package org.rzo.netty.ahessian.rpc.callback;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;
import org.rzo.netty.ahessian.rpc.server.HessianRPCServiceHandler;

public class ServerCallbackProxy implements InvocationHandler, Constants
{
	private boolean _done = false;
	private HessianRPCServiceHandler _handler;
	private HessianRPCCallMessage _message;
	private ClientCallback _clientCallback;
	boolean _closed = false;
	
	public ServerCallbackProxy(HessianRPCServiceHandler handler, HessianRPCCallMessage message, ClientCallback clientCallback)
	{
		_message = message;
		_clientCallback = clientCallback;
		_handler = handler;
	}

	public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
	{
		if (_closed)
			throw new RuntimeException("cannot invoke callback after call to setDone(true)");
		String methodName = method.getName();
		if ("setDone".equals(methodName) && args.length == 1 && (args[0] instanceof Boolean))
		{
			_done = ((Boolean)args[0]).booleanValue();
			return null;
		}
		if ("isDone".equals(method.getName()) && (args == null || args.length == 0))
		{
			return (Boolean)_done;
		}

		if ("isValid".equals(method.getName()) && (args == null || args.length == 0))
		{
			return (Boolean)_message.isValid();
		}

		CallbackReplyMessage reply = new CallbackReplyMessage(methodName, args, null, _message);
		reply.setCallId((Long) _message.getHeaders().get(CALL_ID_HEADER_KEY));
		reply.setGroup((Integer) _message.getHeaders().get(GROUP_HEADER_KEY));
		reply.setCallbackId(_clientCallback.getId());
		reply.setCallbackArgs(args);
		reply.setCallbackMethod(methodName);
		if (_done)
			reply.setCallbackDone(true);
		_handler.writeResult(reply);
		if (_done)
			_closed = true;
		return null;
	}

}
