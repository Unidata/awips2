package org.rzo.yajsw.os;

public interface StopableService
{

	public void onStop();
	public void waitOnStop();
	public void signalStopping(long waitHint);

}
