package org.rzo.yajsw.os;

import java.util.concurrent.Executor;

public interface Mouse
{
	public void registerMouseUpListner(Runnable listner, Executor executor);
	public void unregisterMouseUpListner();


}
