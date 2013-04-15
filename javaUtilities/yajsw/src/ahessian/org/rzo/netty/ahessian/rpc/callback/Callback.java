package org.rzo.netty.ahessian.rpc.callback;

public interface Callback
{
	public void setDone(boolean value);
	public boolean isDone();
	public boolean isValid();
}
