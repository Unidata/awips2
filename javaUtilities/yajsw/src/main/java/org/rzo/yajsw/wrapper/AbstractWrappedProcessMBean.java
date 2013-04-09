package org.rzo.yajsw.wrapper;

import java.awt.Color;
import java.util.Date;

public interface AbstractWrappedProcessMBean
{
	/**
	 * Start.
	 */
	public void start();

	/**
	 * Stop.
	 */
	public void stop();
	public void stop(String reason);

	/**
	 * Restart.
	 */
	public void restart();

	/**
	 * Gets the pid.
	 * 
	 * @return the pid
	 */
	public int getAppPid();

	/**
	 * Gets the exit code.
	 * 
	 * @return the exit code
	 */
	public int getExitCode();

	public String getStringState();

	public void threadDump();

	public void gc();

	public void wrapperThreadDump();

	public String getType();

	public String getName();

	public void waitFor();

	public void stopTimerCondition();

	public boolean isTimerActive();

	public boolean isConditionActive();

	public int getTotalRestartCount();

	public int getRestartCount();

	public Date getAppStarted();

	public Date getAppStopped();

	public int getWrapperPid();

	public Date getWrapperStarted();

	public int getAppCpu();

	public int getAppHandles();

	public long getAppVMemory();
	public long getAppPMemory();

	public int getAppThreads();

	public void startDrain();

	public String readDrainLine();

	public void stopDrain();

	public int getState();

	public String[][] getTrayIconMessages();

	public void stopWrapper();

	public boolean hasOutput();

	public void writeOutput(String txt);

	public void writeInquireResponse(String s);

	public String getInquireMessage();

	public void init();

	public void setProperty(String key, String value);

	public void resetCache();

	public boolean isAppReportedReady();

	public void dumpHeap(String s);

	public Color getUserTrayColor();
	
	public void update(String updateConfFile);

}
