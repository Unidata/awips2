package org.rzo.yajsw.app;

public interface WrapperManagerImplMBean
{
	public void restart();

	public void start();

	public void stop();

	public void stopTimer();

	public void threadDump();

	boolean isControlledByWrapper();

	boolean isLaunchedAsService();

	public boolean isAppearHanging();

	public void setAppearHanging(boolean appearHanging);
	
	public void dumpHeap(String fileName);
	
	public String getStopReason();

}
