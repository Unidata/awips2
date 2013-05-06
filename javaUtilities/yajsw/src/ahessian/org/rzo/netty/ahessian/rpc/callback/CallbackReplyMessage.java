package org.rzo.netty.ahessian.rpc.callback;

import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyMessage;

public class CallbackReplyMessage extends HessianRPCReplyMessage implements Constants
{
	
	private Object[] _args;
	private String _method;
	private boolean _done = false;
	

	public CallbackReplyMessage(String method, Object[] args, Object fault, HessianRPCCallMessage message)
	{
		super(null, fault, message);
		_args = args;
		_method = method;
	}
	
	public void setDone(Boolean done)
	{
		_done = done;
	}


	public Object[] getArgs()
	{
		return _args;
	}


	public String getMethod()
	{
		return _method;
	}
	
	public boolean isDone()
	{
		return _done;
	}

}
